(* dbuild (dumb build): a simple (single file) build tool for compiling ocaml
 * projects.
 *
 * Likely to turn into a poor man's jenga. Why do it?
 *  - trying to keep things simple
 *  - want to implement the ocaml rules from the ground up to ensure I fully
 *    understand how a project actually builds
 *
 * Goals:
 *  - output build commands for easy testing
 *  - incremental rebuilds
 *  - parallel
 *  - maybe install libs as findlib packages?
 * *)

open Core.Std

let () = Unix.create_process_backend := `spawn_vfork

(* Package names refer to ocamlfind packages (defined outside of this project). *)
module Package_name = String_id.Make (struct
  let module_name = "Package_name"
end) ()

(* CR datkin: May change the build rules to produce ocamlfind packages for
 * libraries. *)
(* Lib name refers to a packed library of ocaml modules defined within a dbuild
 * project. *)
(* lowercase name *)
module Lib_name = String_id.Make (struct
  let module_name = "Lib_name"
end) ()

(* lowercase name *)
module Module_name = String_id.Make (struct
  let module_name = "Module_name"
end) ()

(* _Not_ Filename, the Core module *)
module File_name = String_id.Make (struct
  let module_name = "File"
end) ()

module Opam_switch_name = String_id.Make (struct
  let module_name = "Opam_switch_name"
end) ()

module Project_spec = struct
  type direct_deps = {
    packages : Package_name.Set.t [@default Package_name.Set.empty];
    libs : Lib_name.Set.t [@default Lib_name.Set.empty];
  } [@@deriving sexp]

  type library = {
    dir : Lib_name.t;
    modules : Module_name.t list;
    c_stub_basenames : string sexp_list;
    direct_deps : direct_deps;
  } [@@deriving sexp]

  type binary = {
    (* Must be defined in the bin/ directory. *)
    module_name : Module_name.t;
    direct_deps : direct_deps;
    output : [`native | `js];
  } [@@deriving sexp]

  type t = {
    libraries : library list;
    binaries : binary list;
  } [@@deriving sexp]
end

let opam_native = Opam_switch_name.of_string "4.03.0"
let opam_js = Opam_switch_name.of_string "4.03.0+for-js"

(* _Not_ Command, the Core module *)
module Cmd = struct
  type t = {
    exe : string;
    args : string list;
    (* Switch opam environment to eval before running the command. *)
    opam_switch : Opam_switch_name.t option;
  } [@@deriving sexp, hash, compare]
end

(* CR-soon datkin: This may actually be right, but it needs more tests. *)
let topological_fold ~sexp_of_key ~key_set ~roots ~direct_deps:next ~init ~f =
  let rec visit ~descended ~visited node acc =
    if Set.mem descended node
    then raise_s [%message "Cycle" (node : key) (Set.to_list descended : key list)]
    else if Set.mem visited node
    then (visited, acc)
    else
    let unvisited_children =
      next node |> List.filter ~f:(fun x -> not (Set.mem visited x))
    in
    let (visited, acc) =
      let descended = Set.add descended node in
      List.fold unvisited_children ~init:(visited, acc) ~f:(fun (visited, acc) node ->
        visit ~descended ~visited node acc)
    in
    (Set.add visited node, f node acc)
  in
  assert (Set.is_empty key_set);
  let (_visited, acc) =
    List.fold roots ~init:(key_set, init) ~f:(fun (visited, acc) node ->
      visit ~descended:key_set ~visited node acc)
  in
  acc

let%expect_test _ =
  let direct_deps = function
    | "A" -> ["B"; "C"]
    | "B" -> ["C"; "D"]
    | "C" -> ["D"]
    | _ -> []
  in
  printf !"%{sexp:string list Or_error.t}"
    (Or_error.try_with (fun () ->
      (topological_fold
      ~sexp_of_key:[%sexp_of: string]
      ~key_set:String.Set.empty
      ~roots:["A"]
      ~direct_deps
      ~init:[]
      ~f:List.cons) |> List.rev));
  [%expect {| (Ok (D C B A)) |}];
;;

module Action = struct
  module T = struct
  type t =
    | Cmd of Cmd.t
    | Write_file of {
      file : File_name.t;
      contents : string;
    }
  [@@deriving sexp, hash, compare]
  end

  include T
  include Comparable.Make (T)
end

module Build_graph = struct
  type node = {
    action : Action.t;
    inputs : File_name.Set.t;
    outputs : File_name.Set.t;
  } [@@deriving sexp]

  type per_file = {
    needs : node option;
    provides : node list;
  }

  type t = {
    nodes : node list; (* topo sorted? *)
    by_file : per_file File_name.Map.t;
  }

  let of_nodes nodes =
    let by_file =
      List.concat_map nodes ~f:(fun ({ action = _; inputs; outputs; } as node) ->
        let needs =
          Set.to_list outputs |> List.map ~f:(fun output -> output, `needs node)
        in
        let provides =
          Set.to_list inputs |> List.map ~f:(fun input -> input, `provides node)
        in
        List.concat_no_order [needs; provides;]
      )
      |> File_name.Map.of_alist_multi
      |> Map.mapi ~f:(fun ~key:file ~data:nodes ->
          let needs, provides =
            List.partition_map nodes ~f:(function
              | `needs node -> `Fst node
              | `provides node -> `Snd node)
          in
          let needs =
            match needs with
            | [] -> None
            | [ x ] -> Some x
            | nodes ->
              raise_s [%message "More than one command builds file"
                (file : File_name.t)
                (nodes : node list)
              ]
          in
          { needs; provides; })
    in
    { nodes; by_file; }

  let prune t ~roots =
    let _, nodes =
    topological_fold
      ~sexp_of_key:[%sexp_of: File_name.t]
      ~key_set:File_name.Set.empty
      ~roots
      ~direct_deps:(fun file ->
        match Map.find t.by_file file with
        | Some { needs = None; _ } -> []
        | Some { needs = Some node; _ } -> File_name.Set.to_list node.inputs
        | None -> raise_s [%message "Unknown" (file : File_name.t)])
      ~init:(Action.Set.empty, [])
      ~f:(fun file (action_set, nodes) ->
        match Map.find t.by_file file with
        | None -> raise_s [%message "Unknown" (file : File_name.t)]
        | Some { needs = None; _ } -> (action_set, nodes)
        | Some { needs = Some node; _} ->
          if Set.mem action_set node.action
          then (action_set, nodes)
          else
            let action_set = Set.add action_set node.action in
            (action_set, node :: nodes))
    in
    of_nodes (List.rev nodes)
end

type kind =
  | Native
  | Js
[@@deriving sexp]

type dir_kind = [
  | `modules
  | `pack
  | `archive
  | `c
  | `linked
  | `generated
] [@@deriving sexp]

let build_dir kind dir_kind lib_name =
  sprintf !".dbuild/%{sexp:kind}/%{Lib_name}/%{sexp:dir_kind}" kind lib_name dir_kind
  |> String.lowercase

let f x = List.map x ~f:File_name.of_string |> File_name.Set.of_list

let opam_switch = function
  | Native -> opam_native
  | Js -> opam_js

module C_compiler : sig
  val compile : Lib_name.t -> c_base:string -> Build_graph.node
  val archive : Lib_name.t -> c_bases:string list -> Build_graph.node
end = struct
  let compile lib_name ~c_base =
    let input = sprintf !"%{Lib_name}/%s.c" lib_name c_base in
    let output = sprintf !"%s/%s.o" (build_dir Native `c lib_name) c_base in
    let cmd =
      { Cmd.
        (* Could be [None], but we need this for ocamlc. *)
        opam_switch = Some (opam_switch Native);
        exe = "bash";
        args = [
          "-c";
          (String.concat ~sep:" " [
          "gcc";
          "-I"; "$(ocamlc -where)";
          "-c"; input;
          "-o"; output;
          ])
        ];
      }
    in
    { Build_graph.
      action = Cmd cmd;
      inputs = f [ input ];
      outputs = f [ output ];
    }

  let archive lib_name ~c_bases =
    let inputs =
      List.map c_bases ~f:(fun c_base ->
        sprintf !"%s/%s.o" (build_dir Native `c lib_name) c_base)
    in
    let output = sprintf !"%s/lib%{Lib_name}.a" (build_dir Native `c lib_name) lib_name in
    let cmd =
      { Cmd.
        opam_switch = None;
        exe = "ar";
        args = [
          "cr";
          output;
        ] @ inputs;
      }
    in
    { Build_graph.
      action = Cmd cmd;
      inputs = f inputs;
      outputs = f [ output ];
    }

  let%expect_test _ =
    printf !"%{sexp:Build_graph.node}\n%!" (compile (Lib_name.of_string "foo") ~c_base:"bar");
    printf !"%{sexp:Build_graph.node}\n%!" (archive (Lib_name.of_string "foo") ~c_bases:["bar"; "baz"]);
    [%expect {|
      ((action
        (Cmd
         ((exe bash)
          (args
           (-c
            "gcc -I $(ocamlc -where) -c foo/bar.c -o .dbuild/native/foo/c/bar.o"))
          (opam_switch (4.03.0)))))
       (inputs (foo/bar.c)) (outputs (.dbuild/native/foo/c/bar.o)))
      ((action
        (Cmd
         ((exe ar)
          (args
           (cr .dbuild/native/foo/c/libfoo.a .dbuild/native/foo/c/bar.o
            .dbuild/native/foo/c/baz.o))
          (opam_switch ()))))
       (inputs (.dbuild/native/foo/c/bar.o .dbuild/native/foo/c/baz.o))
       (outputs (.dbuild/native/foo/c/libfoo.a))) |}];
  ;;
end

(* Special "library" name for executables *)
let bin_lib = Lib_name.of_string "bin"

let add_namespace lib_name module_name =
  Module_name.of_string (sprintf !"%{Lib_name}__%{Module_name}" lib_name module_name)

module Ocaml_compiler : sig
  val compile
    :  kind
    -> Package_name.Set.t (* package dependencies *)
    -> Lib_name.Set.t (* library dependencies *)
    -> Lib_name.t
    -> Module_name.Set.t (* module dependencies in this lib *)
    -> Module_name.t
    -> [ `ml of [ `has_mli | `no_mli ] | `mli ]
    -> [ `generated | `lib_code | `vanilla ]
    -> Build_graph.node

  val pack : kind -> modules_in_dep_order:Module_name.t list -> Lib_name.t -> Build_graph.node

  val archive : kind -> c_stubs:string list -> Lib_name.t -> Module_name.Set.t -> Build_graph.node

  val link
    : kind -> Package_name.Set.t -> libs_in_dep_order:Lib_name.t list -> Module_name.t -> Build_graph.node
end = struct

  let ocamlc = function
    | Native -> "ocamlopt"
    | Js -> "ocamlc"

  let ext kind which_file =
    match kind, which_file with
    | Native, `ml -> "cmx"
    | Js, `ml -> "cmo"
    | Native, `archive -> "cmxa"
    | Js, `archive -> "cma"
    | _, `mli -> "cmi"

  let compile kind pkgs libs lib_name modules module_name which_file context =
    let which_file, has_mli =
      match which_file with
      | `mli -> `mli, true
      | `ml has_mli ->
        `ml, (match has_mli with | `has_mli -> true | `no_mli -> false)
    in
    let maybe_js_ppx =
      match context with
      | `generated -> []
      | `vanilla | `lib_code ->
        match kind with
        | Native -> []
        | Js -> ["-ppx"; "$(opam config var lib)/js_of_ocaml/ppx_js"]
    in
    let extra_includes =
      Set.to_list libs
      |> List.concat_map ~f:(fun lib ->
          let dir = build_dir kind `modules lib in
          [ "-I"; dir;
          (*
            sprintf !"%s/%{Lib_name}.%s" dir lib (ext kind `archive);
            *)
          ])
    in
    (* CR datkin: If we instead look at the modules in the `modules dir, we might
     * get more parallelism? *)
    let extra_inputs =
      Set.to_list libs
      |> List.map ~f:(fun lib ->
          sprintf !"%s/%{Lib_name}.%s" (build_dir kind `archive lib) lib (ext kind `archive))
    in
    let src_dir =
      match context with
      | `generated -> build_dir kind `generated lib_name
      | `lib_code
      | `vanilla -> Lib_name.to_string lib_name
    in
    let build_dir = build_dir kind `modules lib_name in
    let namespace =
      match context with
      | `vanilla
      | `generated -> ""
      | `lib_code -> sprintf !"%{Lib_name}__" lib_name
    in
    let output =
      let ext = ext kind which_file in
      sprintf !"%s/%s%{Module_name}.%s" build_dir namespace module_name ext
    in
    let extra_outputs =
      match has_mli with
      | true -> []
      | false ->
        [ sprintf !"%s/%s%{Module_name}.%s" build_dir namespace module_name (ext kind `mli) ]
    in
    let extra_inputs =
      match which_file, has_mli with
      | `ml, true ->
        (* The cmi is required *)
        (sprintf !"%s/%s%{Module_name}.%s" build_dir namespace module_name (ext kind `mli))
        :: extra_inputs
      | _, _ -> extra_inputs
    in
    let module_deps =
      begin
        match context with
        | `lib_code -> ()
        | `vanilla | `generated -> assert (Set.is_empty modules)
      end;
      (* The build dir needs to have the cmi's of the modules we depend on
       * compiled. *)
      Set.to_list modules
      |> List.map ~f:(fun module_name ->
        let ext = ext kind `mli in
        sprintf !"%s/%s%{Module_name}.%s" build_dir namespace module_name ext)
    in
    let module_deps =
      match context with
      | `lib_code ->
        sprintf !"%s/%{Lib_name}.%s" build_dir lib_name (ext kind `mli)
        :: module_deps
      | `generated | `vanilla ->
        module_deps
    in
    let input =
      match context with
      | `generated ->
        assert (which_file = `ml);
        assert (has_mli = false);
        sprintf !"%s/%{Module_name}.ml" src_dir module_name
      | `lib_code
      | `vanilla ->
        sprintf !"%s/%{Module_name}.%s" src_dir module_name (match which_file with | `ml -> "ml" | `mli -> "mli")
    in
    let opaque_interface =
      (* It seems like we can only get away with having ml's depend only on
       * other cmi (and not on cmo/cmx) files if we pass this flag. What are the
       * downsides? *)
      match which_file with
      (* CR datkin: Opaque doesn't matter given we depend on the impl now too. *)
      | `mli -> [(*"-opaque"*)]
      | `ml -> []
    in
    let implicit_open =
      match context with
      | `lib_code -> [ "-open"; Lib_name.to_string lib_name |> String.capitalize ]
      | `generated -> []
      | `vanilla -> []
    in
    let ppx =
      match context with
      | `generated -> []
      | `vanilla | `lib_code ->
        [ "-ppx"; sprintf !"ppx-jane -as-ppx -inline-test-lib %{Lib_name}" lib_name; ]
    in
    let cmd =
    { Cmd.
      opam_switch = Some (opam_switch kind);
      exe = "ocamlfind";
      args = [
        ocamlc kind;
        "-w"; "+a-40-42-44";
        "-g";
      ]
      @ opaque_interface
      @ implicit_open
      @ maybe_js_ppx
      @ ppx
      @ [
        "-thread";
        "-package"; Set.to_list pkgs |> List.map ~f:Package_name.to_string |> String.concat ~sep:",";
      ]
      @ extra_includes
      @ [
        "-I"; build_dir;
        "-no-alias-deps";
        "-c"; input;
        "-o"; output;
      ];
    }
    in
    (* CR datkin: In some cases this outputs the cmi too, I think. Check. E.g.,
     * if there's no mli, and maybe even if there is? *)
    { Build_graph.
      action = Cmd cmd;
      inputs = f (input :: extra_inputs @ module_deps);
      outputs = f (output :: extra_outputs);
    }

  let%expect_test _ =
    let pkgs = List.map ["a"; "b"] ~f:Package_name.of_string |> Package_name.Set.of_list in
    let libs = List.map ["x"; "y"] ~f:Lib_name.of_string |> Lib_name.Set.of_list in
    let mods = List.map ["flub"; "blub"] ~f:Module_name.of_string |> Module_name.Set.of_list in
    printf !"%{sexp:Build_graph.node}"
      (compile Native pkgs libs (Lib_name.of_string "foo") mods (Module_name.of_string "bar") (`ml `no_mli) `lib_code);
    [%expect {|
      ((action
        (Cmd
         ((exe ocamlfind)
          (args
           (ocamlopt -w +a-40-42-44 -g -open Foo -ppx
            "ppx-jane -as-ppx -inline-test-lib foo" -thread -package a,b -I
            .dbuild/native/x/modules -I .dbuild/native/y/modules -I
            .dbuild/native/foo/modules -no-alias-deps -c foo/bar.ml -o
            .dbuild/native/foo/modules/foo__bar.cmx))
          (opam_switch (4.03.0)))))
       (inputs
        (.dbuild/native/foo/modules/foo.cmi
         .dbuild/native/foo/modules/foo__blub.cmi
         .dbuild/native/foo/modules/foo__flub.cmi .dbuild/native/x/archive/x.cmxa
         .dbuild/native/y/archive/y.cmxa foo/bar.ml))
       (outputs
        (.dbuild/native/foo/modules/foo__bar.cmi
         .dbuild/native/foo/modules/foo__bar.cmx))) |}];
  ;;

  (* CR datkin: Is it possible to pack the cmi independently? Presumably that would allow
   * you to avoid rebuilding upstream libraries when the implementation of a
   * downstream library changes. Hopefully you can go off the md5sum of the cmi
   * to decide if anything actually changed, but it's sad that you have to
   * rebuild the packed cmi even if just an implementation changed. On the plus
   * side, I suppose the pack operation is extremely cheap? *)
  let pack kind ~modules_in_dep_order lib_name =
    let inputs =
      List.concat_map modules_in_dep_order ~f:(fun module_name ->
        (* CR datkin: Including the mli seems to lead to errors. Is it right to
         * leave 'em out? Maybe test by seeing if the interface is restricted. *)
        List.map [`ml (*; `mli *)] ~f:(fun file_kind ->
          sprintf !"%s/%{Module_name}.%s" (build_dir kind `modules lib_name) module_name (ext kind file_kind)))
    in
    let output, output_cmi =
      let f ext_kind =
        sprintf !"%s/%{Lib_name}.%s" (build_dir kind `pack lib_name) lib_name (ext kind ext_kind)
      in
      f `ml, f `mli
    in
    let cmd =
    { Cmd.
      opam_switch = Some (opam_switch kind);
      exe = "ocamlfind";
      args = [
        ocamlc kind;
        "-pack";
      ] @
      inputs
      @ [
        "-o"; output;
      ];
    }
    in
    { Build_graph.
      action = Cmd cmd;
      inputs = f inputs;
      outputs = f [ output; output_cmi ];
    }

  let%expect_test _ =
    let modules_in_dep_order = List.map ["x"; "y"] ~f:Module_name.of_string in
    printf !"%{sexp:Build_graph.node}" (pack Native ~modules_in_dep_order (Lib_name.of_string "foo"));
    [%expect {|
      ((action
        (Cmd
         ((exe ocamlfind)
          (args
           (ocamlopt -pack .dbuild/native/foo/modules/x.cmx
            .dbuild/native/foo/modules/y.cmx -o .dbuild/native/foo/pack/foo.cmx))
          (opam_switch (4.03.0)))))
       (inputs (.dbuild/native/foo/modules/x.cmx .dbuild/native/foo/modules/y.cmx))
       (outputs (.dbuild/native/foo/pack/foo.cmi .dbuild/native/foo/pack/foo.cmx))) |}];
  ;;

  let archive kind ~c_stubs lib_name module_names =
    let c_input =
      if List.is_empty c_stubs
      then None
      else Some (
        sprintf !"%s/lib%{Lib_name}.a" (build_dir kind `c lib_name) lib_name
      )
    in
    let ml_inputs =
      Set.to_list module_names
      |> List.map ~f:(fun module_name ->
        sprintf !"%s/%{Module_name}.%s" (build_dir kind `modules lib_name) module_name (ext kind `ml))
    in
    let output =
      sprintf !"%s/%{Lib_name}.%s" (build_dir kind `archive lib_name) lib_name (ext kind `archive)
    in
    let c_opts =
      match c_stubs with
      | [] -> []
      | _ :: _ -> [
        "-ccopt"; sprintf !"-L%s" (build_dir kind `c lib_name);
        "-cclib"; sprintf !"-l%{Lib_name}" lib_name;
      ]
    in
    let cmd =
      { Cmd.
        opam_switch = Some (opam_switch kind);
        exe = "ocamlfind";
        args = [
          ocamlc kind;
          "-a";
        ]
        @ c_opts
        @ ml_inputs
        @ [
          "-o"; output;
        ];
      }
    in
    let inputs =
      match c_input with
      | None -> ml_inputs
      | Some x -> x :: ml_inputs
    in
    { Build_graph.
      action = Cmd cmd;
      (* I don't think this actually does anything with the c archive... so it's
       * not actually a dependency? *)
      inputs = f inputs;
      outputs = f [ output ];
    }

  let%expect_test _ =
    let modules =
      List.map [ "a"; "b" ] ~f:Module_name.of_string |> Module_name.Set.of_list
    in
    printf !"%{sexp:Build_graph.node}" (archive Native ~c_stubs:["blah"] (Lib_name.of_string "foo") modules);
    [%expect {|
      ((action
        (Cmd
         ((exe ocamlfind)
          (args
           (ocamlopt -a -ccopt -L.dbuild/native/foo/c -cclib -lfoo
            .dbuild/native/foo/modules/a.cmx .dbuild/native/foo/modules/b.cmx -o
            .dbuild/native/foo/archive/foo.cmxa))
          (opam_switch (4.03.0)))))
       (inputs
        (.dbuild/native/foo/c/libfoo.a .dbuild/native/foo/modules/a.cmx
         .dbuild/native/foo/modules/b.cmx))
       (outputs (.dbuild/native/foo/archive/foo.cmxa))) |}];
  ;;

  let link kind pkgs ~libs_in_dep_order module_name =
    let output =
      let ext =
        match kind with
        | Js -> "byte"
        | Native -> "native"
      in
      sprintf !"%s/%{Module_name}.%s" (build_dir kind `linked bin_lib) module_name ext
    in
    let packages =
      Set.to_list pkgs
      |> List.map ~f:Package_name.to_string
      |> String.concat ~sep:","
    in
    let input_archives =
      (* CR datkin: Do we need to check for c archives too? *)
      List.map libs_in_dep_order ~f:(fun lib ->
        sprintf !"%s/%{Lib_name}.%s"
          (build_dir kind `archive lib)
          lib
          (ext kind `archive))
    in
    let input_module =
      sprintf !"%s/%{Module_name}.%s"
        (build_dir kind `modules bin_lib)
        module_name
        (ext kind `ml)
    in
    let cmd =
      { Cmd.
        opam_switch = Some (opam_switch kind);
        exe = "ocamlfind";
        args = [
          ocamlc kind;
          "-linkpkg";
          "-thread";
          "-package"; packages;
        ] @ input_archives
        @ [
          input_module;
          "-o"; output;
        ];
      }
    in
    { Build_graph.
      action = Cmd cmd;
      inputs = f (input_module :: input_archives);
      outputs = f [ output ];
    }

  let%expect_test _ =
    let pkgs = List.map ["a"; "b"] ~f:Package_name.of_string |> Package_name.Set.of_list in
    let libs_in_dep_order = List.map ["x"; "y"] ~f:Lib_name.of_string in
    let module_name = Module_name.of_string "main" in
    printf !"%{sexp:Build_graph.node}" (link Native pkgs ~libs_in_dep_order module_name);
    [%expect {|
      ((action
        (Cmd
         ((exe ocamlfind)
          (args
           (ocamlopt -linkpkg -thread -package a,b .dbuild/native/x/archive/x.cmxa
            .dbuild/native/y/archive/y.cmxa .dbuild/native/bin/modules/main.cmx -o
            .dbuild/native/bin/linked/main.native))
          (opam_switch (4.03.0)))))
       (inputs
        (.dbuild/native/bin/modules/main.cmx .dbuild/native/x/archive/x.cmxa
         .dbuild/native/y/archive/y.cmxa))
       (outputs (.dbuild/native/bin/linked/main.native))) |}];
  ;;

end

let spec_to_nodes ~file_exists ~get_deps { Project_spec. libraries; binaries; } : Build_graph.node list =
  let of_lib { Project_spec. dir; modules; c_stub_basenames; direct_deps = { packages; libs; }; } =
    (* Instead of using module packing, we use the aliasing technique described
     * here: https://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec235
     *)
    let file module_name ext =
      sprintf !"%{Lib_name}/%{Module_name}.%s" dir module_name ext
    in
    let module_deps_by_module =
      List.map modules ~f:(fun module_name ->
        let basename = sprintf !"%{Module_name}.ml" module_name in
        module_name, get_deps dir ~basename)
      |> Module_name.Map.of_alist_exn
    in
    let mli =
      List.filter_map modules ~f:(fun module_name ->
        let mli = file module_name "mli" in
        if not (file_exists mli)
        then None
        else
          let module_deps =
            Map.find module_deps_by_module module_name
            |> Option.value ~default:[]
            |> Module_name.Set.of_list
          in
          (* CR datkin: Confirm that the compiler kind doesn't matter here.
           * Oh this is wrong -- the compiler type determines the build
           * directory here. *)
          Some (Ocaml_compiler.compile Native packages libs dir module_deps module_name `mli `lib_code))
    in
    let ml =
      List.concat_map [Native; Js;] ~f:(fun kind ->
        let mls =
          List.map modules ~f:(fun module_name ->
            let module_deps =
              Map.find module_deps_by_module module_name
              |> Option.value ~default:[]
              |> Module_name.Set.of_list
            in
            let has_mli =
              let mli = file module_name "mli" in
              match file_exists mli with
              | true -> `has_mli
              | false -> `no_mli
            in
            Ocaml_compiler.compile kind packages libs dir module_deps module_name (`ml has_mli) `lib_code)
        in
        (* CR datkin: Add a test that we get these modules in the right order.
         * *)
        let modules_in_dep_order =
          topological_fold
            ~sexp_of_key:[%sexp_of: Module_name.t]
            ~key_set:Module_name.Set.empty
            ~roots:modules
            ~direct_deps:(fun module_name ->
              match Map.find module_deps_by_module module_name with
              | Some modules -> modules
              | None -> assert false)
            ~init:[]
            ~f:(fun module_name modules -> module_name :: modules)
          |> List.rev
        in
        let wrapper_module = Module_name.of_string (Lib_name.to_string dir) in
        let generated_wrapper =
          let file =
            File_name.of_string (sprintf !"%s/%{Lib_name}.ml" (build_dir kind `generated dir) dir)
          in
          (* CR-someday datkin: This generates the ml file twice. *)
          { Build_graph.
          action = Write_file {
            file;
            contents =
              List.map modules_in_dep_order ~f:(fun module_name ->
                sprintf !"module %s = %s"
                  (String.capitalize (Module_name.to_string module_name))
                  (String.capitalize (Module_name.to_string (add_namespace dir module_name)))
              )
              |> String.concat ~sep:"\n"
              ;
          };
          inputs = f [];
          outputs = File_name.Set.singleton file;
          };
        in
        let compiled_wrapper =
          Ocaml_compiler.compile
          kind
          Package_name.Set.empty
          Lib_name.Set.empty
          dir
          Module_name.Set.empty
          wrapper_module
          (`ml `no_mli)
          `generated;
        in
        (* CR datkin: Are the archive arguments in dep order? *)
        let archive =
          let modules =
            (wrapper_module
            :: (List.map modules_in_dep_order ~f:(add_namespace dir))
            )
            |> Module_name.Set.of_list
          in
          Ocaml_compiler.archive kind ~c_stubs:c_stub_basenames dir modules
        in
        compiled_wrapper :: generated_wrapper :: archive :: mls)
    in
    let c =
      if List.is_empty c_stub_basenames
      then []
      else
        let cs =
          List.map c_stub_basenames ~f:(fun c_base -> C_compiler.compile dir ~c_base)
        in
        let c_archive = C_compiler.archive dir ~c_bases:c_stub_basenames in
        c_archive :: cs
    in
    List.concat_no_order [ mli; ml; c ]
  in
  let deps_by_lib_name =
    List.map libraries ~f:(fun { Project_spec. dir; direct_deps; _ } ->
      dir, direct_deps)
    |> Lib_name.Map.of_alist_exn
  in
  let of_bin { Project_spec. module_name; direct_deps = { packages; libs; }; output; } =
    let (extra_pkgs, libs_in_dep_order) =
      topological_fold
        ~sexp_of_key:[%sexp_of: Lib_name.t]
        ~key_set:Lib_name.Set.empty
        ~roots:(Set.to_list libs)
        ~direct_deps:(fun lib ->
          match Map.find deps_by_lib_name lib with
          | Some { Project_spec. packages = _; libs; } -> Set.to_list libs
          | None -> assert false)
        ~init:(Package_name.Set.empty, [])
        ~f:(fun lib (packages, libs) ->
          match Map.find deps_by_lib_name lib with
          (* We ignore [libs] here, b/c we're traversing them later (from
           * [direct_deps]. *)
          | Some { Project_spec. packages = p; libs = _; } ->
            (Set.union packages p, lib :: libs)
          | None -> assert false)
    in
    let libs_in_dep_order = List.rev libs_in_dep_order in
    let packages = Set.union packages extra_pkgs in
    let kind = match output with `native -> Native | `js -> Js in
    [
      (* CR datkin: `no_mli is a guess *)
      Ocaml_compiler.compile kind packages libs bin_lib Module_name.Set.empty
        module_name (`ml `no_mli) `vanilla;
      Ocaml_compiler.link kind packages ~libs_in_dep_order module_name;
    ]
  in
  let libs = List.concat_map libraries ~f:of_lib in
  let bins = List.concat_map binaries ~f:of_bin in
  List.concat_no_order [libs; bins;]
;;

let project_spec =
  [%of_sexp: Project_spec.t] (Sexp.of_string {|
  (
    (libraries (
      (
        (dir odditty_kernel)
        (modules (
          terminfo
          dec_private_mode
          control_functions
          window
          character_attributes
          character_set
        ))
        (direct_deps (
          (packages (core_kernel async_kernel))
        ))
      )
      (
        (dir odditty)
        (modules (
          pty
          terminfo
        ))
        (c_stub_basenames (pty_stubs))
        (direct_deps (
          (packages (core async))
          (libs (odditty_kernel))
        ))
      )
      (
        (dir web)
        (modules (
          main
        ))
        (direct_deps (
          (packages (js_of_ocaml js_of_ocaml.async async_js virtual_dom))
          (libs ())
        ))
      )
      (
        (dir server)
        (modules (
          web_server
        ))
        (direct_deps (
          (packages (async websocket.async))
          (libs ())
        ))
      )
    ))

    (binaries (
      (
        (module_name inline_test_runner)
        (direct_deps (
          (packages (core async ppx_inline_test.runner.lib ppx_expect.evaluator))
          (libs (odditty_kernel odditty))
        ))
        (output native)
      )
      (
        (module_name main)
        (direct_deps (
          (packages (core async))
          (libs (odditty_kernel odditty server))
        ))
        (output native)
      )
      (
        (module_name web_main)
        (direct_deps (
          (packages ())
          (libs (web))
        ))
        (output js)
      )
    ))
  )
  |})

let%expect_test _ =
  let file_exists = function
    | "odditty_kernel/character_attributes.mli"
    | "odditty_kernel/dec_private_mode.mli"
    | "odditty_kernel/character_set.mli" -> false
    | _ -> true
  in
  let get_deps lib_name ~basename =
    let dep_names =
      match Lib_name.to_string lib_name, basename with
      | "odditty_kernel", "control_functions.ml" ->
        [ "terminfo"; "dec_private_mode"; "character_set"; ]
      | "odditty_kernel", "window.ml" ->
        [ "control_functions" ]
      | _, _ -> []
    in
    List.map dep_names ~f:Module_name.of_string
  in
    (spec_to_nodes ~file_exists ~get_deps project_spec
    |> Build_graph.of_nodes
    |> Build_graph.prune ~roots:[
      File_name.of_string (sprintf "%s/main.native" (build_dir Native `linked bin_lib));
    ]
    |> fun x -> x.Build_graph.nodes
    |> List.iter ~f:(fun node ->
        printf "%s\n"
          (node.Build_graph.outputs
          |> Set.to_list
          |> List.map ~f:File_name.to_string
          |> String.concat ~sep:", ");
        Set.iter node.Build_graph.inputs ~f:(fun file_name ->
          printf "  %s\n" (File_name.to_string file_name));
        printf "\n";
      )
    );
  (* CR datkin: It seems like a bug that the deps of [control_functions.cmi]
   * aren't actually above [control_functions.cmi] in that list.
   *
   * datkin: Ahh, I get it. We didn't have build rules for these deps so we just
   * assumed these files exists. If we sandboxed (or just checked that inputs
   * exist as a pre-cond) we could error out.
   * *)
  [%expect {|
    .dbuild/native/odditty/c/pty_stubs.o
      odditty/pty_stubs.c

    .dbuild/native/odditty/c/libodditty.a
      .dbuild/native/odditty/c/pty_stubs.o

    .dbuild/native/odditty/generated/odditty.ml

    .dbuild/native/odditty/modules/odditty.cmi, .dbuild/native/odditty/modules/odditty.cmx
      .dbuild/native/odditty/generated/odditty.ml

    .dbuild/native/odditty_kernel/generated/odditty_kernel.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi, .dbuild/native/odditty_kernel/modules/odditty_kernel.cmx
      .dbuild/native/odditty_kernel/generated/odditty_kernel.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmi, .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      odditty_kernel/character_attributes.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi, .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      odditty_kernel/character_set.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi, .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      odditty_kernel/dec_private_mode.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      odditty_kernel/terminfo.mli

    .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      odditty_kernel/control_functions.mli

    .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      odditty_kernel/control_functions.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      odditty_kernel/terminfo.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      odditty_kernel/window.mli

    .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmi
      odditty_kernel/window.ml

    .dbuild/native/odditty_kernel/archive/odditty_kernel.cmxa
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmx

    .dbuild/native/odditty/modules/odditty__pty.cmi
      .dbuild/native/odditty/modules/odditty.cmi
      .dbuild/native/odditty_kernel/archive/odditty_kernel.cmxa
      odditty/pty.mli

    .dbuild/native/odditty/modules/odditty__pty.cmx
      .dbuild/native/odditty/modules/odditty.cmi
      .dbuild/native/odditty/modules/odditty__pty.cmi
      .dbuild/native/odditty_kernel/archive/odditty_kernel.cmxa
      odditty/pty.ml

    .dbuild/native/odditty/modules/odditty__terminfo.cmi
      .dbuild/native/odditty/modules/odditty.cmi
      .dbuild/native/odditty_kernel/archive/odditty_kernel.cmxa
      odditty/terminfo.mli

    .dbuild/native/odditty/modules/odditty__terminfo.cmx
      .dbuild/native/odditty/modules/odditty.cmi
      .dbuild/native/odditty/modules/odditty__terminfo.cmi
      .dbuild/native/odditty_kernel/archive/odditty_kernel.cmxa
      odditty/terminfo.ml

    .dbuild/native/odditty/archive/odditty.cmxa
      .dbuild/native/odditty/c/libodditty.a
      .dbuild/native/odditty/modules/odditty.cmx
      .dbuild/native/odditty/modules/odditty__pty.cmx
      .dbuild/native/odditty/modules/odditty__terminfo.cmx

    .dbuild/native/server/generated/server.ml

    .dbuild/native/server/modules/server.cmi, .dbuild/native/server/modules/server.cmx
      .dbuild/native/server/generated/server.ml

    .dbuild/native/server/modules/server__web_server.cmi
      .dbuild/native/server/modules/server.cmi
      server/web_server.mli

    .dbuild/native/server/modules/server__web_server.cmx
      .dbuild/native/server/modules/server.cmi
      .dbuild/native/server/modules/server__web_server.cmi
      server/web_server.ml

    .dbuild/native/server/archive/server.cmxa
      .dbuild/native/server/modules/server.cmx
      .dbuild/native/server/modules/server__web_server.cmx

    .dbuild/native/bin/modules/main.cmi, .dbuild/native/bin/modules/main.cmx
      .dbuild/native/odditty/archive/odditty.cmxa
      .dbuild/native/odditty_kernel/archive/odditty_kernel.cmxa
      .dbuild/native/server/archive/server.cmxa
      bin/main.ml

    .dbuild/native/bin/linked/main.native
      .dbuild/native/bin/modules/main.cmx
      .dbuild/native/odditty/archive/odditty.cmxa
      .dbuild/native/odditty_kernel/archive/odditty_kernel.cmxa
      .dbuild/native/server/archive/server.cmxa |}]
;;

let file_exists file =
  match Core.Std.Unix.access file [`Exists] with
  | Ok () -> true
  | Error _ -> false
;;

module Cache : sig
  type t

  val load : unit -> t

  (* Checks the file against the cache, and also updates the cache. *)
  val snapshot : t -> File_name.t -> [ `Changed | `Same | `Missing ]

  val save : t -> unit
end = struct
  type t = string File_name.Table.t [@@deriving sexp]

  let file = ".dbuild-cache"

  let load () =
    if file_exists file
    then Sexp.load_sexp_conv_exn file [%of_sexp: t]
    else File_name.Table.create ()

  let save t = Sexp.save_hum file ([%sexp_of: t] t)

  let snapshot t filename =
    if not (file_exists (File_name.to_string filename))
    then `Missing
    else
      let new_digest = Digest.file (File_name.to_string filename) in
      match Hashtbl.find t filename with
      | None -> `Changed
      | Some old_digest ->
        if String.(=) old_digest new_digest
        then `Same
        else (
          Hashtbl.set t ~key:filename ~data:new_digest;
          `Changed
        )
end

let file_arg = Command.Arg_type.create File_name.of_string

let parse_deps lines : Module_name.t list =
  match lines with
  | [] -> assert false
  | line :: _ ->
    let module_name_of_filename filename =
      Filename.basename filename
      |> String.rsplit2_exn ~on:'.'
      |> fst
      |> Module_name.of_string
    in
    match String.split ~on:' ' line with
    | target_file :: ":" :: deps ->
      let target_module = module_name_of_filename target_file in
      List.filter_map deps ~f:(fun filename ->
        let module_name = module_name_of_filename filename in
        Option.some_if (module_name <> target_module) module_name)
    | _ -> assert false
;;

let%expect_test _ =
  let ocamldep_output =
    {|odditty_kernel/control_functions.cmo : odditty_kernel/terminfo.cmi odditty_kernel/dec_private_mode.cmo odditty_kernel/character_set.cmo odditty_kernel/control_functions.cmi
odditty_kernel/control_functions.cmx : odditty_kernel/terminfo.cmx odditty_kernel/dec_private_mode.cmx odditty_kernel/character_set.cmx odditty_kernel/control_functions.cmi|}
    |> String.split ~on:'\n'
  in
  printf !"%{sexp: Module_name.t list}" (parse_deps ocamldep_output);
  [%expect "(terminfo dec_private_mode character_set)"];
;;

let get_deps dir ~basename =
  (* CR-someday datkin: Add '-ppx'? *)
  let cmd = sprintf !"ocamldep -one-line -I %{Lib_name} %{Lib_name}/%s" dir dir basename in
  let output = Unix.open_process_in cmd |> In_channel.input_lines in
  (* eprintf !"> %s\n< %{sexp#mach:string list}\n" cmd output; *)
  parse_deps output
;;

let pruned_build_graph ~roots =
  spec_to_nodes ~file_exists ~get_deps project_spec
  |> Build_graph.of_nodes
  |> Build_graph.prune ~roots

let get_opam_env =
  let cache : (string * string) list Opam_switch_name.Table.t =
    Opam_switch_name.Table.create ()
  in
  fun opam_switch_name ->
    Hashtbl.find_or_add cache opam_switch_name ~default:(fun () ->
      let cmd =
        sprintf !"opam config env --switch=\"%{Opam_switch_name}\"" opam_switch_name
      in
      Unix.open_process_in cmd
      |> In_channel.input_lines
      |> List.map ~f:(fun line ->
          line
          |> String.rsplit2_exn ~on:';'
          |> fst
          |> String.rsplit2_exn ~on:';'
          |> fst
          |> String.lsplit2_exn ~on:'='
          |> fun (key, value) ->
              key, Scanf.sscanf value "%S" ident
    ))

open Async.Std

let dot_cmd =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"Output a dot file for the graph spec. You can compile with, e.g.  `dot -Tpng /tmp/x.dot  > /tmp/x.png`."
    [%map_open
      let roots = anon (sequence ("target" %: file_arg)) in
      fun () ->
        printf "digraph deps {\n%!";
        printf "  rankdir=LR;\n";
        printf "  splines=line;\n"; (* also "polyline"? *)
        printf "  edge[samehead=x sametail=y];\n";
        (pruned_build_graph ~roots
        |> fun x -> x.Build_graph.nodes
        |> List.iter ~f:(fun node ->
            Set.iter node.Build_graph.outputs ~f:(fun output ->
              Set.iter node.Build_graph.inputs ~f:(fun input ->
                printf !{|  "%{File_name}" -> "%{File_name}";|} input output;
                printf "\n";
             )))
        );
        printf "}\n";
        Deferred.unit]

let build_cmd =
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary:"Build the specified targets"
    [%map_open
      let targets = anon (sequence ("target" %: file_arg)) in
      fun () ->
        pruned_build_graph ~roots:targets
        |> fun x -> x.Build_graph.nodes
        |> Deferred.List.fold ~init:(Ok ()) ~f:(fun result { Build_graph.  outputs; inputs = _; action; } ->
            Deferred.return result
            >>=? fun () ->
            (* CR datkin: Add sandboxing to verify input dependencies. *)
            let dirs =
              Set.to_list outputs
              |> List.map ~f:(fun f -> Filename.dirname (File_name.to_string f))
              |> List.dedup ~compare:String.compare
            in
            List.iter dirs ~f:Core.Unix.mkdir_p;
            begin
              match action with
              | Write_file { file; contents; } ->
                Writer.with_file (File_name.to_string file) ~f:(fun writer ->
                  Writer.write writer contents;
                  Deferred.unit)
                >>= fun () ->
                Deferred.return (Ok ())
              | Cmd { Cmd. exe; args; opam_switch; } ->
                let env =
                  Option.map opam_switch ~f:(fun name ->
                    `Replace (get_opam_env name))
                in
                Process.create
                  ?env
                  ~prog:exe
                  ~args
                  ()
                >>=? fun process ->
                Process.collect_output_and_wait process
                >>= fun { stdout; stderr; exit_status; } ->
                printf !"> (%{sexp#mach:Unix.env option}) %s %{sexp#mach:string list}\n"
                  env exe args;
                printf "  stdout:\n%s\n" stdout;
                printf "  stderr:\n%s\n" stderr;
                Deferred.return (Unix.Exit_or_signal.or_error exit_status)
            end)
    ]

let () =
  let is_test =
    Core.Std.Unix.environment ()
    |> Array.exists ~f:(String.is_prefix ~prefix:"TEST=")
  in
  if is_test
  then
    let open Ppx_inline_test_lib in
    Runtime.exit ()
  else
    let open Command.Let_syntax in
    Command.group
      ~summary:"Build commands" [
        "dot-graph", dot_cmd;
        "build", build_cmd;
      ]
    |> Command.run
;;
