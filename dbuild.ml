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

open Core_kernel.Std

(* Package names refer to ocamlfind packages (defined outside of this project). *)
module Package_name = String_id.Make (struct
    let module_name = "Package_name"
  end) ()

(* CR-someday datkin: May change the build rules to produce ocamlfind packages for
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
    args : string list list;
    (* Switch opam environment to eval before running the command. *)
    opam_switch : Opam_switch_name.t option;
  } [@@deriving sexp, hash, compare]

  let to_string ?env:_ { exe; args; opam_switch; } =
    ( sprintf !"%{sexp:Opam_switch_name.t option} $ %s \\" opam_switch exe
    :: (List.map args ~f:(fun arg_group ->
      List.map arg_group ~f:(fun str ->
        if String.mem str ' '
        then sprintf "%S" str
        else String.to_string str)
      |> String.concat ~sep:" "
      |> (fun line -> sprintf "  %s \\" line)
      )
    )
    |> String.concat ~sep:"\n")
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

  let to_string ?env t =
    match t with
    | Cmd cmd ->
      Cmd.to_string ?env cmd
    | Write_file { file; contents = _; } ->
      sprintf !"<generate %{File_name}>" file
end

let group_files_by_dir files =
  Set.to_list files
    |> List.map ~f:(fun file ->
        let file = File_name.to_string file in
        Filename.dirname file, Filename.basename file)
    |> String.Map.of_alist_multi
    |> Map.to_alist
;;

let (^/) = Core.Std.(^/)

let abbreviated_files_to_string files =
  group_files_by_dir files
    |> List.map ~f:(fun (dir, files) ->
        match files with
        | [] -> assert false
        | [file] -> dir ^/ file
        | files ->
          dir ^/ "{" ^ (String.concat files ~sep:",") ^ "}"
    )
;;


module Build_graph = struct
  type node = {
    action : Action.t;
    inputs : File_name.Set.t;
    outputs : File_name.Set.t;
  } [@@deriving sexp, fields]

  module Node = struct
    type t = node

    let to_string { action; inputs; outputs; } =
      String.concat ~sep:"\n" (
        [ Action.to_string action ]
        @
        (abbreviated_files_to_string inputs
        |> List.map ~f:(fun x -> "> " ^ x))
        @
        (abbreviated_files_to_string outputs
        |> List.map ~f:(fun x -> "< " ^ x))
        )
    ;;
  end

  let _hash_fold_t state { action; inputs; outputs; } =
    [%hash_fold: (Action.t * File_name.t list * File_name.t list)]
      state (action, Set.to_list inputs, Set.to_list outputs)

  type per_file = {
    needs : node option;
    provides : node list;
  } [@@deriving fields]

  type t = {
    (* CR-soon datkin: in the [pruned] case this is topologically sorted.
     * Should probably make that an invariant. *)
    nodes : node list;
    by_file : per_file File_name.Map.t;
  } [@@deriving fields]

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
        args = [[
          "-c";
          (String.concat ~sep:" " [
             "gcc";
             "-I"; "$(ocamlc -where)";
             "-c"; input;
             "-o"; output;
           ])
        ]];
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
        args = [[
          "cr";
          output;
        ]] @ [inputs];
      }
    in
    { Build_graph.
      action = Cmd cmd;
      inputs = f inputs;
      outputs = f [ output ];
    }

  let%expect_test _ =
    printf !"%{Build_graph.Node}\n%!" (compile (Lib_name.of_string "foo") ~c_base:"bar");
    printf !"%{Build_graph.Node}\n%!" (archive (Lib_name.of_string "foo") ~c_bases:["bar"; "baz"]);
    [%expect {|
      (4.03.0) $ bash \
        -c "gcc -I $(ocamlc -where) -c foo/bar.c -o .dbuild/native/foo/c/bar.o" \
      > foo/bar.c
      < .dbuild/native/foo/c/bar.o
      () $ ar \
        cr .dbuild/native/foo/c/libfoo.a \
        .dbuild/native/foo/c/bar.o .dbuild/native/foo/c/baz.o \
      > .dbuild/native/foo/c/{bar.o,baz.o}
      < .dbuild/native/foo/c/libfoo.a |}];
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
    -> Module_name.Set.t Lib_name.Map.t (* library dependencies *)
    -> Lib_name.t
    -> Module_name.Set.t (* module dependencies in this lib *)
    -> Module_name.t
    -> [ `ml of [ `has_mli | `no_mli ] | `mli ]
    -> [ `generated | `lib_code | `vanilla ]
    -> Build_graph.node

  val archive : kind -> c_stubs:string list -> Lib_name.t -> modules_in_dep_order:Module_name.t list -> Build_graph.node

  val link
    :  kind
    -> Package_name.Set.t
    -> libs_in_dep_order:(Lib_name.t * [ `archive | `no_archive ]) list
    -> Module_name.t
    -> Build_graph.node
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
    | _, `obj -> "o"

  let compile kind pkgs libs lib_name modules module_name which_file context =
    let which_file, has_mli =
      match which_file with
      | `mli -> `mli, true
      | `ml has_mli ->
        `ml, (match has_mli with | `has_mli -> true | `no_mli -> false)
    in
    let kind_flag =
      match which_file with
      | `ml -> "-impl"
      | `mli -> "-intf"
    in
    let maybe_js_ppx =
      match context with
      | `generated -> []
      | `vanilla | `lib_code ->
        match kind with
        | Native -> []
        | Js -> ["-ppx"; "$(opam config var lib --root ~/.opam)/js_of_ocaml/ppx_js"]
    in
    let extra_includes, extra_inputs =
      Map.to_alist libs
      |> List.map ~f:(fun (lib, lib_modules) ->
        let dir = build_dir kind `modules lib in
        [ "-I"; dir ],
        List.map (Set.to_list lib_modules) ~f:(fun module_name ->
          sprintf !"%s/%{Module_name}.cmi" (build_dir kind `modules lib) module_name)
        )
      |> List.unzip
      |> (fun (a, b) -> List.concat a, List.concat b)
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
    let extra_outputs =
      match which_file, kind with
      | `ml, Native ->
        (sprintf !"%s/%s%{Module_name}.o" build_dir namespace module_name)
        :: extra_outputs
      | _, _ -> extra_outputs
    in
    let extra_inputs =
      match which_file, has_mli with
      | `ml, true ->
        (* The cmi is required *)
        (sprintf !"%s/%s%{Module_name}.%s" build_dir namespace module_name (ext kind `mli))
        (* And we need to include the mli in the sandbox. *)
        :: (sprintf !"%{Lib_name}/%{Module_name}.mli" lib_name module_name)
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
      |> List.concat_map ~f:(fun module_name ->
          List.map [`mli;(* `ml; `obj*)] ~f:(fun file_ext ->
        let ext = ext kind file_ext in
        sprintf !"%s/%s%{Module_name}.%s" build_dir namespace module_name ext))
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
    let disabled_warnings = [ 4; 40; 42; 44; 48; 58; ] in
    let disabled_warnings =
      match context with
      | `generated ->
        (* The generated aliases file references modules we haven't compiled
         * yet. *)
        49 :: disabled_warnings
      | `lib_code | `vanilla -> disabled_warnings
    in
    let warnings =
      sprintf "+a%s"
        (List.map disabled_warnings ~f:(fun x -> sprintf "-%d" x)
        |> String.concat ~sep:"")
    in
    let cmd =
      { Cmd.
        opam_switch = Some (opam_switch kind);
        exe = "ocamlfind";
        args = [[
          ocamlc kind;
          "-w"; warnings;
          "-g";
        ]]
          @ [implicit_open]
          @ [maybe_js_ppx]
          @ [ppx]
          @ [
            ["-thread"];
            ["-package";
            Set.to_list pkgs |> List.map ~f:Package_name.to_string |> String.concat ~sep:",";]
          ]
          @ [extra_includes]
          @ [
            ["-I"; build_dir];
            ["-no-alias-deps"];
            ["-c"; kind_flag; input];
            ["-o"; output];
          ];
      }
    in
    { Build_graph.
      action = Cmd cmd;
      inputs = f (input :: extra_inputs @ module_deps);
      outputs = f (output :: extra_outputs);
    }

  let%expect_test _ =
    let pkgs = List.map ["a"; "b"] ~f:Package_name.of_string |> Package_name.Set.of_list in
    let libs =
      List.map [
        "x", ["x"; "x__a"];
        "y", ["y"; "y__b"];
    ] ~f:(fun (lib, mods) ->
      Lib_name.of_string lib, List.map mods ~f:Module_name.of_string |> Module_name.Set.of_list)
      |> Lib_name.Map.of_alist_exn
    in
    let mods = List.map ["flub"; "blub"] ~f:Module_name.of_string |> Module_name.Set.of_list in
    printf !"%{sexp:Build_graph.node}"
      (compile Native pkgs libs (Lib_name.of_string "foo") mods (Module_name.of_string "bar") (`ml `no_mli) `lib_code);
    [%expect {|
      ((action
        (Cmd
         ((exe ocamlfind)
          (args
           ((ocamlopt -w +a-4-40-42-44-48-58 -g) (-open Foo) ()
            (-ppx "ppx-jane -as-ppx -inline-test-lib foo") (-thread) (-package a,b)
            (-I .dbuild/native/x/modules -I .dbuild/native/y/modules)
            (-I .dbuild/native/foo/modules) (-no-alias-deps) (-c -impl foo/bar.ml)
            (-o .dbuild/native/foo/modules/foo__bar.cmx)))
          (opam_switch (4.03.0)))))
       (inputs
        (.dbuild/native/foo/modules/foo.cmi
         .dbuild/native/foo/modules/foo__blub.cmi
         .dbuild/native/foo/modules/foo__flub.cmi .dbuild/native/x/modules/x.cmi
         .dbuild/native/x/modules/x__a.cmi .dbuild/native/y/modules/y.cmi
         .dbuild/native/y/modules/y__b.cmi foo/bar.ml))
       (outputs
        (.dbuild/native/foo/modules/foo__bar.cmi
         .dbuild/native/foo/modules/foo__bar.cmx
         .dbuild/native/foo/modules/foo__bar.o))) |}];
    printf !"%{sexp:Build_graph.node}"
      (compile Native pkgs Lib_name.Map.empty (Lib_name.of_string "foo")
         Module_name.Set.empty (Module_name.of_string "bar") (`ml `no_mli) `generated);
    [%expect {|
      ((action
        (Cmd
         ((exe ocamlfind)
          (args
           ((ocamlopt -w +a-49-4-40-42-44-48-58 -g) () () () (-thread)
            (-package a,b) () (-I .dbuild/native/foo/modules) (-no-alias-deps)
            (-c -impl .dbuild/native/foo/generated/bar.ml)
            (-o .dbuild/native/foo/modules/bar.cmx)))
          (opam_switch (4.03.0)))))
       (inputs (.dbuild/native/foo/generated/bar.ml))
       (outputs
        (.dbuild/native/foo/modules/bar.cmi .dbuild/native/foo/modules/bar.cmx
         .dbuild/native/foo/modules/bar.o))) |}];
  ;;

  let archive kind ~c_stubs lib_name ~modules_in_dep_order =
    let c_input =
      if List.is_empty c_stubs
      then None
      else Some (
        sprintf !"%s/lib%{Lib_name}.a" (build_dir kind `c lib_name) lib_name
      )
    in
    let ml_inputs =
      List.map modules_in_dep_order ~f:(fun module_name ->
        sprintf !"%s/%{Module_name}.%s" (build_dir kind `modules lib_name) module_name (ext kind `ml))
    in
    let obj_inputs =
      match kind with
      | Js -> []
      | Native ->
        List.map modules_in_dep_order ~f:(fun module_name ->
          sprintf !"%s/%{Module_name}.o" (build_dir kind `modules lib_name) module_name)
    in
    let output =
      sprintf !"%s/%{Lib_name}.%s" (build_dir kind `archive lib_name) lib_name (ext kind `archive)
    in
    let a_output =
      match kind with
      | Js -> []
      | Native ->
        [ sprintf !"%s/%{Lib_name}.a" (build_dir kind `archive lib_name) lib_name ]
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
        args = [[
          ocamlc kind;
          "-a";
        ]]
          @ [c_opts]
          @ [ml_inputs]
          @ [[
            "-o"; output;
          ]];
      }
    in
    let inputs = ml_inputs @ obj_inputs in
    let inputs =
      match c_input with
      | None -> inputs
      | Some x -> x :: inputs
    in
    { Build_graph.
      action = Cmd cmd;
      (* I don't think this actually does anything with the c archive... so it's
       * not actually a dependency? *)
      inputs = f inputs;
      outputs = f (output :: a_output);
    }

  let%expect_test _ =
    let modules_in_dep_order = List.map [ "a"; "b" ] ~f:Module_name.of_string in
    printf !"%{sexp:Build_graph.node}" (archive Native ~c_stubs:["blah"] (Lib_name.of_string "foo") ~modules_in_dep_order);
    [%expect {|
      ((action
        (Cmd
         ((exe ocamlfind)
          (args
           ((ocamlopt -a) (-ccopt -L.dbuild/native/foo/c -cclib -lfoo)
            (.dbuild/native/foo/modules/a.cmx .dbuild/native/foo/modules/b.cmx)
            (-o .dbuild/native/foo/archive/foo.cmxa)))
          (opam_switch (4.03.0)))))
       (inputs
        (.dbuild/native/foo/c/libfoo.a .dbuild/native/foo/modules/a.cmx
         .dbuild/native/foo/modules/a.o .dbuild/native/foo/modules/b.cmx
         .dbuild/native/foo/modules/b.o))
       (outputs
        (.dbuild/native/foo/archive/foo.a .dbuild/native/foo/archive/foo.cmxa))) |}];
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
      List.map libs_in_dep_order ~f:(fun (lib, _) ->
        sprintf !"%s/%{Lib_name}.%s"
          (build_dir kind `archive lib)
          lib
          (ext kind `archive))
    in
    let input_c_archives =
      List.filter_map libs_in_dep_order ~f:(fun (lib, has_archive) ->
        match has_archive with
        | `no_archive -> None
        | `archive ->
          sprintf !"%s/lib%{Lib_name}.a"
            (build_dir kind `c lib)
            lib
          |> Some)
    in
    let extra_inputs =
      match kind with
      | Js -> []
      | Native ->
        List.concat_map libs_in_dep_order ~f:(fun (lib, _) -> [
          sprintf !"%s/%{Lib_name}.a"
            (build_dir kind `archive lib)
            lib;
          sprintf !"%s/%{Module_name}.o"
            (build_dir kind `modules bin_lib)
            module_name;
        ])
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
          [ocamlc kind;
          "-thread";
          "-linkpkg";
          ];
          ["-package"; packages];
        ] @ [input_archives]
          @ [
            [input_module];
            ["-o"; output];
          ];
      }
    in
    { Build_graph.
      action = Cmd cmd;
      inputs = f (input_module :: input_archives @ input_c_archives @ extra_inputs);
      outputs = f [ output ];
    }

  let%expect_test _ =
    let pkgs = List.map ["a"; "b"] ~f:Package_name.of_string |> Package_name.Set.of_list in
    let libs_in_dep_order =
      ["x", `archive; "y", `no_archive]
      |> List.map ~f:(fun (lib, x) -> Lib_name.of_string lib, x)
    in
    let module_name = Module_name.of_string "main" in
    printf !"%{sexp:Build_graph.node}" (link Native pkgs ~libs_in_dep_order module_name);
    [%expect {|
      ((action
        (Cmd
         ((exe ocamlfind)
          (args
           ((ocamlopt -thread -linkpkg) (-package a,b)
            (.dbuild/native/x/archive/x.cmxa .dbuild/native/y/archive/y.cmxa)
            (.dbuild/native/bin/modules/main.cmx)
            (-o .dbuild/native/bin/linked/main.native)))
          (opam_switch (4.03.0)))))
       (inputs
        (.dbuild/native/bin/modules/main.cmx .dbuild/native/bin/modules/main.o
         .dbuild/native/x/archive/x.a .dbuild/native/x/archive/x.cmxa
         .dbuild/native/x/c/libx.a .dbuild/native/y/archive/y.a
         .dbuild/native/y/archive/y.cmxa))
       (outputs (.dbuild/native/bin/linked/main.native))) |}];
  ;;

end

let spec_to_nodes ~file_exists ~get_deps { Project_spec. libraries; binaries; } : Build_graph.node list =
  let archive_by_lib =
    List.map libraries ~f:(fun { Project_spec. dir; c_stub_basenames; _ } ->
      let has_archive =
        if List.is_empty c_stub_basenames
        then `no_archive
        else `archive
      in
      dir, has_archive
    )
    |> Lib_name.Map.of_alist_exn
  in
  let modules_by_lib =
    List.map libraries ~f:(fun { Project_spec. dir; modules; _ } ->
      let modules =
        Module_name.of_string (Lib_name.to_string dir)
        :: List.map modules ~f:(fun module_name ->
          Module_name.of_string (sprintf !"%{Lib_name}__%{Module_name}" dir module_name))
      in
      dir, Module_name.Set.of_list modules
    )
    |> Lib_name.Map.of_alist_exn
  in
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
    let libs =
      (* CR-soon datkin: Need to also take the topo closure of libs. *)
      Lib_name.Set.to_map libs ~f:(Map.find_exn modules_by_lib)
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
          (* CR-someday datkin: Can we share the compiled cmi between 4.03.0 and
           * 4.03.0+for-js (32 bit)? *)
          Some (Ocaml_compiler.compile Native packages libs dir module_deps module_name `mli `lib_code))
    in
    let ml =
      List.concat_map [Native; Js;] ~f:(fun kind ->
        let mls =
          List.map modules ~f:(fun module_name ->
            let module_deps =
              (* These are the module dependencies named explicitly in the file.
               * However, we could, e.g. reference constructors implicitly (i.e.
               * w/o mentioning the module named) from other modules in the
               * dependency closure, so we'll grab the closure. *)
              Map.find module_deps_by_module module_name
              |> Option.value ~default:[]
            in
            let module_deps =
              topological_fold
                ~sexp_of_key:[%sexp_of: Module_name.t]
                ~key_set:Module_name.Set.empty
                ~roots:module_deps
                ~direct_deps:(fun module_name ->
                  match Map.find module_deps_by_module module_name with
                  | Some modules -> modules
                  | None -> assert false)
                ~init:Module_name.Set.empty
                ~f:(fun module_name modules -> Set.add modules module_name)
            in
            let has_mli =
              let mli = file module_name "mli" in
              match file_exists mli with
              | true -> `has_mli
              | false -> `no_mli
            in
            Ocaml_compiler.compile kind packages libs dir module_deps module_name (`ml has_mli) `lib_code)
        in
        (* CR-soon datkin: Add a test that we get these modules in the right
         * order. *)
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
            Lib_name.Map.empty
            dir
            Module_name.Set.empty
            wrapper_module
            (`ml `no_mli)
            `generated;
        in
        let archive =
          let modules_in_dep_order =
            wrapper_module
            :: (List.map modules_in_dep_order ~f:(add_namespace dir))
          in
          Ocaml_compiler.archive kind ~c_stubs:c_stub_basenames dir ~modules_in_dep_order
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
    let libs =
      (* CR datkin: Need to also take the topo closure of libs. *)
      Lib_name.Set.to_map libs ~f:(Map.find_exn modules_by_lib)
    in
    let libs_in_dep_order =
      List.rev libs_in_dep_order
      |> List.map ~f:(fun lib -> lib, Map.find_exn archive_by_lib lib)
    in
    let packages = Set.union packages extra_pkgs in
    let kind = match output with `native -> Native | `js -> Js in
    [
      (* CR-soon datkin: `no_mli is a guess *)
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
          ;; CR datkin: What's the difference between:
          ;;   js_of_ocaml.async
          ;;   async_js
          (packages (js_of_ocaml.async core_kernel async_kernel virtual_dom))
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

let%expect_test "dependency summary" =
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
  let bg =
    spec_to_nodes ~file_exists ~get_deps project_spec
    |> Build_graph.of_nodes
    |> Build_graph.prune ~roots:[
      File_name.of_string (sprintf "%s/main.native" (build_dir Native `linked bin_lib));
      File_name.of_string (sprintf "%s/web_main.byte" (build_dir Js `linked bin_lib));
    ]
  in
  List.iter (Build_graph.nodes bg) ~f:(fun node ->
    printf "%s\n"
      (node.Build_graph.outputs
       |> Set.to_list
       |> List.map ~f:File_name.to_string
       |> String.concat ~sep:", ");
    Set.iter node.Build_graph.inputs ~f:(fun file_name ->
      printf "  %s\n" (File_name.to_string file_name));
    printf "\n";
  );
  [%expect {|
    .dbuild/native/odditty/generated/odditty.ml

    .dbuild/native/odditty/modules/odditty.cmi, .dbuild/native/odditty/modules/odditty.cmx, .dbuild/native/odditty/modules/odditty.o
      .dbuild/native/odditty/generated/odditty.ml

    .dbuild/native/odditty_kernel/generated/odditty_kernel.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi, .dbuild/native/odditty_kernel/modules/odditty_kernel.cmx, .dbuild/native/odditty_kernel/modules/odditty_kernel.o
      .dbuild/native/odditty_kernel/generated/odditty_kernel.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmi, .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmx, .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      odditty_kernel/character_attributes.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi, .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmx, .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      odditty_kernel/character_set.ml

    .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi, .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmx, .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.o
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

    .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      odditty_kernel/window.mli

    .dbuild/native/odditty/modules/odditty__pty.cmi
      .dbuild/native/odditty/modules/odditty.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmi
      odditty/pty.mli

    .dbuild/native/odditty/modules/odditty__terminfo.cmi
      .dbuild/native/odditty/modules/odditty.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmi
      odditty/terminfo.mli

    .dbuild/native/server/generated/server.ml

    .dbuild/native/server/modules/server.cmi, .dbuild/native/server/modules/server.cmx, .dbuild/native/server/modules/server.o
      .dbuild/native/server/generated/server.ml

    .dbuild/native/server/modules/server__web_server.cmi
      .dbuild/native/server/modules/server.cmi
      server/web_server.mli

    .dbuild/native/bin/modules/main.cmi, .dbuild/native/bin/modules/main.cmx, .dbuild/native/bin/modules/main.o
      .dbuild/native/odditty/modules/odditty.cmi
      .dbuild/native/odditty/modules/odditty__pty.cmi
      .dbuild/native/odditty/modules/odditty__terminfo.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmi
      .dbuild/native/server/modules/server.cmi
      .dbuild/native/server/modules/server__web_server.cmi
      bin/main.ml

    .dbuild/native/odditty/c/pty_stubs.o
      odditty/pty_stubs.c

    .dbuild/native/odditty/c/libodditty.a
      .dbuild/native/odditty/c/pty_stubs.o

    .dbuild/native/odditty/modules/odditty__pty.cmx, .dbuild/native/odditty/modules/odditty__pty.o
      .dbuild/native/odditty/modules/odditty.cmi
      .dbuild/native/odditty/modules/odditty__pty.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmi
      odditty/pty.ml
      odditty/pty.mli

    .dbuild/native/odditty/modules/odditty__terminfo.cmx, .dbuild/native/odditty/modules/odditty__terminfo.o
      .dbuild/native/odditty/modules/odditty.cmi
      .dbuild/native/odditty/modules/odditty__terminfo.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmi
      odditty/terminfo.ml
      odditty/terminfo.mli

    .dbuild/native/odditty/archive/odditty.a, .dbuild/native/odditty/archive/odditty.cmxa
      .dbuild/native/odditty/c/libodditty.a
      .dbuild/native/odditty/modules/odditty.cmx
      .dbuild/native/odditty/modules/odditty.o
      .dbuild/native/odditty/modules/odditty__pty.cmx
      .dbuild/native/odditty/modules/odditty__pty.o
      .dbuild/native/odditty/modules/odditty__terminfo.cmx
      .dbuild/native/odditty/modules/odditty__terminfo.o

    .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmx, .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      odditty_kernel/control_functions.ml
      odditty_kernel/control_functions.mli

    .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmx, .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      odditty_kernel/terminfo.ml
      odditty_kernel/terminfo.mli

    .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmx, .dbuild/native/odditty_kernel/modules/odditty_kernel__window.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmi
      .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmi
      odditty_kernel/window.ml
      odditty_kernel/window.mli

    .dbuild/native/odditty_kernel/archive/odditty_kernel.a, .dbuild/native/odditty_kernel/archive/odditty_kernel.cmxa
      .dbuild/native/odditty_kernel/modules/odditty_kernel.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_attributes.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__character_set.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__control_functions.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__dec_private_mode.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__terminfo.o
      .dbuild/native/odditty_kernel/modules/odditty_kernel__window.cmx
      .dbuild/native/odditty_kernel/modules/odditty_kernel__window.o

    .dbuild/native/server/modules/server__web_server.cmx, .dbuild/native/server/modules/server__web_server.o
      .dbuild/native/server/modules/server.cmi
      .dbuild/native/server/modules/server__web_server.cmi
      server/web_server.ml
      server/web_server.mli

    .dbuild/native/server/archive/server.a, .dbuild/native/server/archive/server.cmxa
      .dbuild/native/server/modules/server.cmx
      .dbuild/native/server/modules/server.o
      .dbuild/native/server/modules/server__web_server.cmx
      .dbuild/native/server/modules/server__web_server.o

    .dbuild/native/bin/linked/main.native
      .dbuild/native/bin/modules/main.cmx
      .dbuild/native/bin/modules/main.o
      .dbuild/native/odditty/archive/odditty.a
      .dbuild/native/odditty/archive/odditty.cmxa
      .dbuild/native/odditty/c/libodditty.a
      .dbuild/native/odditty_kernel/archive/odditty_kernel.a
      .dbuild/native/odditty_kernel/archive/odditty_kernel.cmxa
      .dbuild/native/server/archive/server.a
      .dbuild/native/server/archive/server.cmxa

    .dbuild/js/web/generated/web.ml

    .dbuild/js/web/modules/web.cmi, .dbuild/js/web/modules/web.cmo
      .dbuild/js/web/generated/web.ml

    .dbuild/js/bin/modules/web_main.cmi, .dbuild/js/bin/modules/web_main.cmo
      .dbuild/js/web/modules/web.cmi
      .dbuild/js/web/modules/web__main.cmi
      bin/web_main.ml

    .dbuild/js/web/modules/web__main.cmo
      .dbuild/js/web/modules/web.cmi
      .dbuild/js/web/modules/web__main.cmi
      web/main.ml
      web/main.mli

    .dbuild/js/web/archive/web.cma
      .dbuild/js/web/modules/web.cmo
      .dbuild/js/web/modules/web__main.cmo

    .dbuild/js/bin/linked/web_main.byte
      .dbuild/js/bin/modules/web_main.cmo
      .dbuild/js/web/archive/web.cma |}];
  let node =
    Map.find_exn
      (Build_graph.by_file bg)
      (File_name.of_string ".dbuild/native/odditty_kernel/generated/odditty_kernel.ml")
    |> Build_graph.needs
    |> Option.value_exn
  in
  printf !"%{sexp:Build_graph.node}\n" node;
  [%expect {|
    ((action
      (Write_file
       (file .dbuild/native/odditty_kernel/generated/odditty_kernel.ml)
       (contents
         "module Terminfo = Odditty_kernel__terminfo\
        \nmodule Dec_private_mode = Odditty_kernel__dec_private_mode\
        \nmodule Character_set = Odditty_kernel__character_set\
        \nmodule Control_functions = Odditty_kernel__control_functions\
        \nmodule Window = Odditty_kernel__window\
        \nmodule Character_attributes = Odditty_kernel__character_attributes")))
     (inputs ())
     (outputs (.dbuild/native/odditty_kernel/generated/odditty_kernel.ml))) |}];
;;

let file_exists file =
  match Core.Std.Unix.access file [`Exists] with
  | Ok () -> true
  | Error _ -> false
;;

module Cache : sig
  type t [@@deriving compare]

  val load : unit -> t

  val create : Build_graph.t -> t

  (* If you've just eval'd the node, recache the digests of the output files.
   * Assumes [Buid_graph.node] was part of the [Build_graph.t] used to
   * construct [t], and may raise if not. *)
  val update_node : t -> Build_graph.node -> t

  (* If [action_digest] is provided, [t]'s entry for [file] should contain the
   * given action digest. Otherwise, raises. *)
  val update_file : t -> ?action_digest:int option -> File_name.t -> t * [`Changed | `Same]

  (* CR-soon datkin: It would be nice to return a reason why we should rebuild. *)
  val should_rebuild : t -> t -> Build_graph.node -> bool

  val anything_changed : t -> t -> [ `Yes of Info.t | `No ]

  val save : t -> unit
end = struct

  type file_digest = string [@@deriving compare]

  let sexp_of_file_digest str =
    [%sexp_of: string] (Digest.to_hex str)

  let file_digest_of_sexp = function
    | Sexp.Atom str -> Digest.from_hex str
    | List _ -> assert false

  type entry = {
    file_digest : file_digest option; (* None if file didn't exist *)
    action_digest : int option; (* None if the file was a leaf *)
  } [@@deriving sexp, compare, fields]

  (* CR-someday datkin: If we also save the hash of the whole build graph, then
   * we can slightly optimize: if none of the leaf files changed (and trusting
   * none of the interior files was modified?) we can just short circuit
   * building anything. *)
  type t = entry File_name.Map.t [@@deriving sexp, compare]

  let anything_changed t1 t2 =
    match
      Map.symmetric_diff t1 t2 ~data_equal:[%equal: entry]
      |> Sequence.to_list
    with
    | [] -> `No
    | diff ->
      `Yes (Info.create_s [%message
        (diff : (File_name.t, entry) Map.Symmetric_diff_element.t list)
      ])

  let file = ".dbuild-cache"

  let digest_file file =
    if file_exists (File_name.to_string file)
    then Some (Digest.file (File_name.to_string file))
    else None

  let create bg =
    Build_graph.by_file bg
    |> Map.mapi ~f:(fun ~key:file ~data:{ needs; provides = _; } ->
      let file_digest = digest_file file in
      let action_digest =
        Option.map needs ~f:(fun { action; _ } ->
          Action.hash action)
      in
      { file_digest; action_digest; })

  let update_file t ?action_digest:expected_action_digest file =
    let new_digest = digest_file file in
    match Map.find t file with
    | None ->
      begin
        match expected_action_digest with
        | None ->
          let entry = { file_digest = new_digest; action_digest = None; } in
          Map.add t ~key:file ~data:entry, `Changed
        | Some _ -> failwith "Expected file to exist in the cache"
      end
    | Some { file_digest; action_digest; } ->
      begin
        match expected_action_digest with
        | None -> ()
        | Some expect -> [%test_result: int option] ~expect action_digest
      end;
      if [%equal: string option] file_digest new_digest
      then t, `Same
      else (
        let t =
          Map.add t ~key:file ~data:{ file_digest = new_digest; action_digest; }
        in
        t, `Changed
      )
  ;;

  let update_node t { Build_graph. action; outputs; inputs = _; } =
    let action_digest = Some (Action.hash action) in
    Set.fold outputs ~init:t ~f:(fun t file ->
      fst (update_file t ~action_digest file))
  ;;

  let should_rebuild t1 t2 { Build_graph. action = _; inputs; outputs; } =
    let inputs_changed =
      (* Do the inputs look the same? Doesn't even matter if they were built the
       * same way. *)
      Set.exists inputs ~f:(fun file ->
        [%equal: string option option]
          (Map.find t1 file |> Option.map ~f:file_digest)
          (Map.find t2 file |> Option.map ~f:file_digest)
        |> not
      )
    in
    let outputs_changed =
      (* The files may have changed, or the action may have changed. *)
      Set.exists outputs ~f:(fun file ->
        match Map.find t2 file with
        | None
        | Some { file_digest = None; _ }
          (* If the file doesn't exist, definitely need to rebuild. *)
          -> true
        | Some _ as t2_entry ->
          not ([%equal: entry option] (Map.find t1 file) t2_entry)
      )
    in
    inputs_changed || outputs_changed
  ;;

  let load () =
    if file_exists file
    then Sexp.load_sexp_conv_exn file [%of_sexp: t]
    else File_name.Map.empty

  let save t = Sexp.save_hum file ([%sexp_of: t] t)
end

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

open Core.Std
open Async.Std

let get_deps dir ~basename =
  (* CR-someday datkin: Add '-ppx'? *)
  (* CR-soon datkin: This isn't necessarily running in the right opam env. *)
  let cmd = sprintf !"ocamldep -one-line -I %{Lib_name} %{Lib_name}/%s" dir dir basename in
  let output = Core.Unix.open_process_in cmd |> In_channel.input_lines in
  (* eprintf !"> %s\n< %{sexp#mach:string list}\n" cmd output; *)
  parse_deps output
;;

let pruned_build_graph project_spec ~roots =
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
      Core.Unix.open_process_in cmd
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

let file_arg = Command.Arg_type.create File_name.of_string

let () = Core.Unix.create_process_backend := `spawn_vfork

let project_spec_param () =
  let open Command.Let_syntax in
  [%map_open
    let file = flag "spec" (optional file) ~doc:"file Project spec sexp file" in
    match file with
    | None -> project_spec
    | Some file -> Sexp.load_sexp_conv_exn file [%of_sexp: Project_spec.t]
  ]
;;

let dot_cmd =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"Output a dot file for the graph spec. You can compile with, e.g.  `dot -Tpng /tmp/x.dot  > /tmp/x.png`."
    [%map_open
      let roots = anon (sequence ("target" %: file_arg))
      and project_spec = project_spec_param () in
      fun () ->
        printf "digraph deps {\n%!";
        printf "  rankdir=LR;\n";
        printf "  splines=line;\n"; (* also "polyline"? *)
        printf "  edge[samehead=x sametail=y];\n";
        (pruned_build_graph project_spec ~roots
         |> Build_graph.nodes
         |> List.iter ~f:(fun node ->
           Set.iter node.Build_graph.outputs ~f:(fun output ->
             Set.iter node.Build_graph.inputs ~f:(fun input ->
               printf !{|  "%{File_name}" -> "%{File_name}";|} input output;
               printf "\n";
             )))
        );
        printf "}\n";
        Deferred.unit]

let mkdirs files =
  let dirs =
    Set.to_list files
    |> List.map ~f:(fun f -> Filename.dirname (File_name.to_string f))
    |> List.dedup ~compare:String.compare
  in
  List.iter dirs ~f:Core.Unix.mkdir_p;
;;

let sandbox_file ~sandbox file_name =
  File_name.of_string (sprintf !"%s/%{File_name}" sandbox file_name)

module Node_result = struct
  type t =
    | Cached
    | Generated of { elapsed : Time_ns.Span.t; }
    | Ran of {
      output : Process.Output.t;
      elapsed : Time_ns.Span.t;
    }
  [@@deriving sexp_of]

  let to_unit_or_error = function
    | Cached
    | Generated _ -> Ok ()
    | Ran { output = { exit_status; _ }; _ } -> Unix.Exit_or_signal.or_error exit_status

  let succeeded t = Result.is_ok (to_unit_or_error t)
end

let run_node ~working_dir { Build_graph. action; inputs = _; outputs; } : Node_result.t Or_error.t Deferred.t =
  mkdirs (File_name.Set.map outputs ~f:(sandbox_file ~sandbox:working_dir));
  let start = Time_ns.now () in
  begin
    match action with
    | Write_file { file; contents; } ->
      (*Core.Std.printf !"%{Action}\n%!" action;*)
      Writer.with_file (working_dir ^/ File_name.to_string file) ~f:(fun writer ->
        Writer.write writer contents;
        Deferred.unit)
      >>= fun () ->
      let elapsed = Time_ns.diff (Time_ns.now ()) start in
      Deferred.return (Ok (Node_result.Generated { elapsed; }))
    | Cmd { Cmd. exe; args; opam_switch; } ->
      let env = Option.map opam_switch ~f:get_opam_env in
      (*Core.Std.printf !"%{Action}\n%!" (*?env*) action;*)
      Process.create
        ?env:(Option.map env ~f:(fun env -> `Replace env))
        ~working_dir
        ~prog:exe
        ~args:(List.concat args)
        ()
      >>=? fun process ->
      Process.collect_output_and_wait process
      >>| fun output ->
      let elapsed = Time_ns.diff (Time_ns.now ()) start in
      Ok (Node_result.Ran { elapsed; output; })
  end

let assert_empty ~sandbox =
  let cmd = sprintf "find %s -type f" sandbox in
  match Core.Unix.open_process_in cmd |> In_channel.input_lines with
  | [] -> ()
  | files ->
    raise_s [%message "Sandbox not empty"
      (sandbox : string)
      (files : string list)
    ]

(* Copies files to sandbox, returns the sandbox dir. *)
let prep_sandbox =
  let n = ref 0 in
  fun { Build_graph. action = _; inputs; outputs; } : string Deferred.t ->
    let sandbox = sprintf ".dbuild-sandbox/%d" !n in (* For parallel build, ensure sandboxes are unique. *)
    incr n;
    Core.Unix.mkdir_p sandbox;
    assert_empty ~sandbox; (* It may have already existed and left unclean *)
    let sandbox_inputs = File_name.Set.map inputs ~f:(sandbox_file ~sandbox) in
    mkdirs sandbox_inputs;
    Set.iter inputs ~f:(fun file ->
      begin
      if not (file_exists (File_name.to_string file))
      then failwithf !"Can't build %{sexp:File_name.Set.t}, required file %{File_name} is missing"
             outputs file ()
      end;
      let cmd =
        sprintf !"cp %{File_name} %{File_name}" file (sandbox_file ~sandbox file)
      in
      Core.Unix.system cmd
      |> Core.Unix.Exit_or_signal.or_error
      |> Or_error.ok_exn
    );
    return sandbox

let run_in_sandbox ~sandbox ({ Build_graph. action = _; inputs; outputs; } as node) =
  (* CR datkin: We should actually do this in terms of the project root... *)
  let cwd = Core.Unix.getcwd () in
  (* Prep the output dirs so they're there before the renames.  *)
  mkdirs outputs;
  Monitor.protect (fun () ->
    run_node ~working_dir:sandbox node
    >>=? fun node_result ->
    begin
      if Node_result.succeeded node_result
      then
        Set.iter outputs ~f:(fun file ->
          Core.Unix.rename
            ~src:(File_name.to_string (sandbox_file ~sandbox file))
            ~dst:(File_name.to_string file))
      else ()
    end;
    return (Ok node_result))
  ~finally:(fun () ->
    (* If anything goes wrong, cleanup the sandbox, but don't require that
     * everything actually exists. *)
    Set.union inputs outputs
    |> File_name.Set.map ~f:(sandbox_file ~sandbox)
    |> Set.iter ~f:(fun file ->
      if file_exists (File_name.to_string file)
      then Core.Unix.unlink (File_name.to_string file)
      else ());
    assert_empty ~sandbox;
    Deferred.unit)
;;

let build_cmd =
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary:"Build the specified targets"
    [%map_open
      let use_sandbox = flag "sandbox" no_arg ~doc:" Build in sandbox"
      and stop_before_build = flag "stop" no_arg ~doc:" Stop right before the target"
      and targets = anon (sequence ("target" %: file_arg))
      and project_spec = project_spec_param ()
      in
      fun () ->
        let target_set = File_name.Set.of_list targets in
        pruned_build_graph project_spec ~roots:targets
        |> Build_graph.nodes
        |> Deferred.List.fold ~init:(Ok ()) ~f:(fun result node ->
          Deferred.return result
          >>=? fun () ->
          let stop_now =
            stop_before_build
            && not (Set.is_empty (Set.inter node.outputs target_set))
          in
          begin
            if stop_now
            then Core.Std.printf !"%{Action}\n%!" node.action;
          end;
          if use_sandbox
          then (
            prep_sandbox node
            >>= fun sandbox ->
            if stop_now
            then Deferred.return (Or_error.errorf "Stopped, sandbox in %s" sandbox)
            else
              run_in_sandbox ~sandbox node
              >>=? fun result ->
              Deferred.return (Node_result.to_unit_or_error result)
          )
          else (
            if stop_now
            then Deferred.return (Or_error.error_string "stopped")
            else
              run_node ~working_dir:"." node
              >>=? fun result ->
              Deferred.return (Node_result.to_unit_or_error result)
          )
        )
    ]
;;

module Job_pool : sig
  type 'a t

  val create : unit -> _ t

  (* Raises if [t] is closed. *)
  val add_exn : 'a t -> 'a Deferred.t -> unit

  (* Raises if [t] is closed. Only one call may be pending at a time. *)
  val next_exn : 'a t -> [ `No_jobs | `Ok of 'a Deferred.t ]
end = struct
  type 'a t = {
    jobs : 'a Deferred.t Bag.t;
    mutable next : [ `Listener of 'a Ivar.t | `Done of 'a list ];
  } [@@deriving sexp_of]

  let create () = {
    jobs = Bag.create ();
    next = `Done [];
  }

  let add_exn t job =
    let elt = Bag.add t.jobs job in
    don't_wait_for (
      job
      >>| fun result ->
      Bag.remove t.jobs elt;
      match t.next with
      | `Listener ivar ->
        Ivar.fill ivar result;
        t.next <- `Done [];
      | `Done finished ->
        t.next <- `Done (result :: finished);
    )
  ;;

  let next_exn t =
    match t.next with
    | `Listener _ ->
      raise_s [%message "Another call to [next_exn] is pending"]
    | `Done queue ->
      match queue with
      | [] ->
        if Bag.is_empty t.jobs
        then `No_jobs
        else
          let result =
            Deferred.create (fun ivar ->
              t.next <- `Listener ivar)
          in
          `Ok result
    | result :: finished ->
      t.next <- `Done finished;
      `Ok (return result)
  ;;
end

(* Returns the cache up to what we've built, and the result. *)
let incremental_parallel_build ~old_cache ~new_cache bg
  : (Cache.t * (unit, Error.t list * File_name.Set.t) Result.t) Deferred.t =
  (* CR datkin: Factor the parallel logic out so we can test it. *)
  let pool = Job_pool.create () in
  let build cache node =
    if Cache.should_rebuild old_cache cache node
    then (
      Core.Std.printf !"Building %s\n%!"
        (abbreviated_files_to_string node.outputs |> String.concat ~sep:" ");
      prep_sandbox node
      >>= fun sandbox ->
      run_in_sandbox ~sandbox node
      >>=? fun node_result ->
      Deferred.return (Ok node_result)
    )
    else (
      Deferred.return (Ok Node_result.Cached)
    )
  in
  let build cache node =
    let job = build cache node >>| fun r -> (node, r) in
    Job_pool.add_exn pool job
  in
  let newly_built_files =
    Build_graph.by_file bg
    |> Map.filter ~f:(fun per_file ->
        Option.is_none per_file.Build_graph.needs)
    |> Map.keys
    |> File_name.Set.of_list
  in
  let unbuilt = Build_graph.by_file bg |> Map.keys |> File_name.Set.of_list in
  let unbuilt = Set.fold newly_built_files ~init:unbuilt ~f:Set.remove in
  let ready_nodes ~newly_built_files ~unbuilt =
    assert (not (Set.is_empty newly_built_files));
    Set.fold newly_built_files ~init:Set.Poly.empty ~f:(fun next file ->
      let maybe_ready =
        match Map.find (Build_graph.by_file bg) file with
        | None -> []
        | Some per_file -> Build_graph.provides per_file
      in
      List.fold maybe_ready ~init:next ~f:(fun next node ->
        let any_pending = Set.exists node.inputs ~f:(Set.mem unbuilt) in
        if any_pending
        then next
        else Set.add next node))
  in
  begin
    Build_graph.nodes bg
    |> List.filter ~f:(fun node -> Set.is_empty node.Build_graph.inputs)
    |> List.iter ~f:(build new_cache)
  end;
  Set.iter (ready_nodes ~newly_built_files ~unbuilt) ~f:(build new_cache); (* enqueue the initial jobs *)
  (* CR datkin: Check that [unbuilt] isn't empty and/or that the pool isn't
   * empty. *)
  Deferred.repeat_until_finished (new_cache, ([] : Error.t list), unbuilt) (fun (new_cache, failures, unbuilt) ->
    if Set.is_empty unbuilt
    then (
      assert (List.is_empty failures);
      assert (Job_pool.next_exn pool = `No_jobs);
      return (`Finished (new_cache, Ok ()))
    )
    else (
      match Job_pool.next_exn pool with
      | `No_jobs -> return (`Finished (new_cache, Error (failures, unbuilt)))
      | `Ok result ->
        result
        >>= fun (node, build_result) ->
        match build_result with
        | Error err ->
          (* NB: We don't bother to update the cache in this case. *)
          let err = Error.tag_arg err "node" node [%sexp_of: Build_graph.node] in
          return (`Repeat (new_cache, err :: failures, unbuilt))
        | Ok (node_result : Node_result.t) ->
          let step_succeeded =
            match node_result with
            | Cached -> Ok `Cached
            | Generated { elapsed } -> Ok (`Built elapsed)
            | Ran { output = { stdout; stderr; exit_status }; elapsed } ->
              Core.Std.printf
                !"%{Build_graph.Node}\n= stdout =\n%s= stderr =\n%s\n%!"
                node
                stdout
                stderr;
              Unix.Exit_or_signal.or_error exit_status
              |> Result.map ~f:(fun () -> `Built elapsed)
          in
          match step_succeeded with
          | Error err ->
            let err = Error.tag_arg err "targets" node.outputs [%sexp_of: File_name.Set.t] in
            return (`Repeat (new_cache, err :: failures, unbuilt))
          | Ok build_info ->
            begin
              match build_info with
              | `Cached (*-> ()*)
              | `Built _ ->
                Core.Std.printf
                  !"%{sexp:[`Built of Time_ns.Span.t|`Cached]} %s\n%!"
                  build_info
                  (abbreviated_files_to_string node.outputs |> String.concat ~sep:" ");
            end;
            let new_cache = Cache.update_node new_cache node in
            let newly_built_files = Build_graph.outputs node in
            let unbuilt = Set.fold newly_built_files ~init:unbuilt ~f:Set.remove in
            Set.iter (ready_nodes ~newly_built_files ~unbuilt) ~f:(build new_cache);
            return (`Repeat (new_cache, failures, unbuilt))
    )
  )
;;


let parallel_build_cmd =
  let open Command.Let_syntax in
  Command.async_or_error'
    ~summary:"Build the specified targets"
    [%map_open
      let use_sandbox = flag "sandbox" no_arg ~doc:" Build in sandbox"
      and stop_before_build = flag "stop" no_arg ~doc:" Stop right before the target"
      and poll = flag "poll" no_arg ~doc:" Poll continually (don't exit on first error/success)"
      and targets = anon (sequence ("target" %: file_arg))
      and project_spec = project_spec_param ()
      in
      fun () ->
        let bg =
          let target_set = File_name.Set.of_list targets in
          pruned_build_graph project_spec ~roots:targets
        in
        let rec loop ~old_cache ~first_call =
          let new_cache = Cache.create bg in
          let wait =
            poll
            &&
            match Cache.anything_changed old_cache new_cache with
            | `No -> true
            | `Yes what_changed ->
              Core.Std.printf !"Rebuilding %{sexp#mach:Info.t}\n%!" what_changed;
              false
          in
          if wait
          then (
            if first_call
            then (
              Core.Std.printf "No changes since last build, polling\n%!";
            )
            else ();
            Clock.after (sec 0.5)
            >>= fun () ->
            loop ~old_cache ~first_call:false
          )
          else (
            incremental_parallel_build ~old_cache ~new_cache bg
            >>= fun (cache, result) ->
            Cache.save cache;
            if poll
            then (
              begin
                match result with
                | Ok () -> Core.Std.printf "Done, yay!\n%!"
                | Error (errors, unbuilt) ->
                  Core.Std.printf !"Build failed, %d errors, %d unbuilt\n%!"
                    (List.length errors)
                    (Set.length unbuilt);
                  List.iter errors ~f:(fun err ->
                    Core.Std.printf !"  %{sexp#mach:Error.t}\n%!" err
                  )
              end;
              Clock.after (sec 0.5)
              >>= fun () ->
              loop ~old_cache:cache ~first_call:false
            )
            else (
              Result.map_error result ~f:(fun (failures, _unbuilt) ->
                Error.of_list failures)
              |> Deferred.return
            )
          )
        in
        let old_cache = Cache.load () in
        loop ~old_cache ~first_call:true
    ]


let dump_cmd =
  let open Command.Let_syntax in
  let build_graph_cmd =
    Command.async'
      ~summary:"Dump sexp represention of the build graph."
      [%map_open
        let roots = anon (sequence ("target" %: file_arg))
        and project_spec = project_spec_param ()
        in
        fun () ->
          printf !"%{sexp#hum:Build_graph.node list}\n"
            (Build_graph.nodes (pruned_build_graph project_spec ~roots));
          Deferred.unit]
  in
  let project_spec_cmd =
    Command.async'
      ~summary:"Dump sexp represention of the project spec."
      [%map_open
        let project_spec = project_spec_param () in
        fun () ->
          printf !"%{sexp#hum:Project_spec.t}\n" project_spec;
          Deferred.unit]
  in
  Command.group
    ~summary:"Print stuff" [
      "build-graph", build_graph_cmd;
      "project-spec", project_spec_cmd;
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
    Command.group
      ~summary:"Build commands" [
      "dot-graph", dot_cmd;
      "build", build_cmd;
      "parallel-build", parallel_build_cmd;
      "dump", dump_cmd;
    ]
    |> Command.run
;;
