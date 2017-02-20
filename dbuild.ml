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
open Async.Std

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
    packages : Package_name.Set.t [@default Package_name.Set.empty]; (* i.e. findlib packages *)
    libs : Lib_name.Set.t [@default Lib_name.Set.empty]; (* i.e. other non-findlib packages *)
  } [@@deriving sexp]

  type library = {
    dir : Lib_name.t;
    modules_in_dep_order : Module_name.t list;
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
  } [@@deriving sexp]
end

module Build_graph = struct
  type node = {
    cmd : Cmd.t;
    inputs : File_name.Set.t;
    outputs : File_name.Set.t;
  } [@@deriving sexp]

  type per_file = {
    needs : node list; (* single node? *)
    provides : node list;
  }

  type t = {
    nodes : node list;
    by_file : per_file File_name.Map.t;
  }

  let of_nodes nodes =
    let by_file =
      List.concat_map nodes ~f:(fun ({ cmd = _; inputs; outputs; } as node) ->
        let needs =
          Set.to_list outputs |> List.map ~f:(fun output -> output, `needs node)
        in
        let provides =
          Set.to_list inputs |> List.map ~f:(fun input -> input, `provides node)
        in
        List.concat_no_order [needs; provides;]
      )
      |> File_name.Map.of_alist_multi
      |> Map.map ~f:(fun nodes ->
          let needs, provides =
            List.partition_map nodes ~f:(function
              | `needs node -> `Fst node
              | `provides node -> `Snd node)
          in
          { needs; provides; })
    in
    { nodes; by_file; }
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
        exe = "gcc";
        args = [
          "-I"; "$(ocamlc -where)";
          "-c"; input;
          "-o"; output;
        ];
      }
    in
    { Build_graph.
      cmd;
      inputs = f [ input ];
      outputs = f [ output ];
    }

  let archive lib_name ~c_bases =
    let inputs =
      List.map c_bases ~f:(fun c_base ->
        sprintf !"%s/%s.o" (build_dir Native `c lib_name) c_base)
    in
    let output = sprintf !"%s/%{Lib_name}.a" (build_dir Native `c lib_name) lib_name in
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
      cmd;
      inputs = f inputs;
      outputs = f [ output ];
    }

  let%expect_test _ =
    printf !"%{sexp:Build_graph.node}\n%!" (compile (Lib_name.of_string "foo") ~c_base:"bar");
    printf !"%{sexp:Build_graph.node}\n%!" (archive (Lib_name.of_string "foo") ~c_bases:["bar"; "baz"]);
    [%expect {|
      ((cmd
        ((exe gcc)
         (args (-I "$(ocamlc -where)" -c foo/bar.c -o .dbuild/native/foo/c/bar.o))
         (opam_switch (4.03.0))))
       (inputs (foo/bar.c)) (outputs (.dbuild/native/foo/c/bar.o)))
      ((cmd
        ((exe ar)
         (args
          (cr .dbuild/native/foo/c/foo.a .dbuild/native/foo/c/bar.o
           .dbuild/native/foo/c/baz.o))
         (opam_switch ())))
       (inputs (.dbuild/native/foo/c/bar.o .dbuild/native/foo/c/baz.o))
       (outputs (.dbuild/native/foo/c/foo.a))) |}];
  ;;
end

(* Special "library" name for executables *)
let bin_lib = Lib_name.of_string "bin"

module Ocaml_compiler : sig
  val compile
    :  kind
    -> Package_name.Set.t (* package dependencies *)
    -> Lib_name.Set.t (* library dependencies *)
    -> Lib_name.t
    -> Module_name.Set.t (* module dependencies in this lib *)
    -> Module_name.t
    -> [ `ml | `mli ]
    -> Build_graph.node

  val pack : kind -> modules_in_dep_order:Module_name.t list -> Lib_name.t -> Build_graph.node

  val archive : kind -> c_stubs:string list -> Lib_name.t -> Build_graph.node

  val link
    : kind -> Package_name.Set.t -> Lib_name.Set.t -> Module_name.t -> Build_graph.node
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

  let compile kind pkgs libs lib_name modules module_name which_file =
    let maybe_js_ppx =
      match kind with
      | Native -> []
      | Js -> ["-ppx"; "$(opam config var lib)/js_of_ocaml/ppx_js"]
    in
    let extra_includes =
      Set.to_list libs
      |> List.concat_map ~f:(fun lib ->
          [ "-I"; build_dir kind `pack lib; ])
    in
    (* CR datkin: If we instead look at the modules in the `modules dir, we might
     * get more parallelism? *)
    let extra_inputs =
      Set.to_list libs
      |> List.map ~f:(fun lib ->
          sprintf !"%s/%{Lib_name}.%s" (build_dir kind `pack lib) lib (ext kind `mli))
    in
    let build_dir = build_dir kind `modules lib_name in
    let output =
      let ext = ext kind which_file in
      sprintf !"%s/%{Module_name}.%s" build_dir module_name ext
    in
    let module_deps =
      (* The build dir needs to have the cmi's of the modules we depend on
       * compiled. *)
      Set.to_list modules
      |> List.map ~f:(fun module_name ->
        let ext = ext kind `mli in
        sprintf !"%s/%{Module_name}.%s" build_dir module_name ext)
    in
    let input =
      sprintf !"%{Lib_name}/%{Module_name}.%s" lib_name module_name (match which_file with | `ml -> "ml" | `mli -> "mli")
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
      @ maybe_js_ppx
      @ [
        "-ppx"; sprintf !"ppx-jane -as-ppx -inline-test-lib %{Lib_name}" lib_name;
        "-thread";
        "-package"; Set.to_list pkgs |> List.map ~f:Package_name.to_string |> String.concat ~sep:",";
      ]
      @ extra_includes
      @ [
        "-I"; build_dir;
        "-for-pack"; Lib_name.to_string lib_name |> String.capitalize;
        "-c"; input;
        "-o"; output;
      ];
    }
    in
    (* CR datkin: In some cases this outputs the cmi too, I think. Check. E.g.,
     * if there's no mli, and maybe even if there is? *)
    { Build_graph.
      cmd;
      inputs = f (input :: extra_inputs @ module_deps);
      outputs = f [ output ];
    }

  let%expect_test _ =
    let pkgs = List.map ["a"; "b"] ~f:Package_name.of_string |> Package_name.Set.of_list in
    let libs = List.map ["x"; "y"] ~f:Lib_name.of_string |> Lib_name.Set.of_list in
    let mods = List.map ["flub"; "blub"] ~f:Module_name.of_string |> Module_name.Set.of_list in
    printf !"%{sexp:Build_graph.node}"
      (compile Native pkgs libs (Lib_name.of_string "foo") mods (Module_name.of_string "bar") `ml);
    [%expect {|
      ((cmd
        ((exe ocamlfind)
         (args
          (ocamlopt -w +a-40-42-44 -g -ppx "ppx-jane -as-ppx -inline-test-lib foo"
           -thread -package a,b -I .dbuild/native/x/pack -I .dbuild/native/y/pack
           -I .dbuild/native/foo/modules -for-pack Foo -c foo/bar.ml -o
           .dbuild/native/foo/modules/bar.cmx))
         (opam_switch (4.03.0))))
       (inputs
        (.dbuild/native/foo/modules/blub.cmi .dbuild/native/foo/modules/flub.cmi
         .dbuild/native/x/pack/x.cmi .dbuild/native/y/pack/y.cmi foo/bar.ml))
       (outputs (.dbuild/native/foo/modules/bar.cmx))) |}];
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
        List.map [`ml; `mli] ~f:(fun file_kind ->
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
      cmd;
      inputs = f inputs;
      outputs = f [ output; output_cmi ];
    }

  let%expect_test _ =
    let modules_in_dep_order = List.map ["x"; "y"] ~f:Module_name.of_string in
    printf !"%{sexp:Build_graph.node}" (pack Native ~modules_in_dep_order (Lib_name.of_string "foo"));
    [%expect {|
      ((cmd
        ((exe ocamlfind)
         (args
          (ocamlopt -pack .dbuild/native/foo/modules/x.cmx
           .dbuild/native/foo/modules/x.cmi .dbuild/native/foo/modules/y.cmx
           .dbuild/native/foo/modules/y.cmi -o .dbuild/native/foo/pack/foo.cmx))
         (opam_switch (4.03.0))))
       (inputs
        (.dbuild/native/foo/modules/x.cmi .dbuild/native/foo/modules/x.cmx
         .dbuild/native/foo/modules/y.cmi .dbuild/native/foo/modules/y.cmx))
       (outputs (.dbuild/native/foo/pack/foo.cmi .dbuild/native/foo/pack/foo.cmx))) |}];
  ;;

  let archive kind ~c_stubs lib_name =
    let input =
      sprintf !"%s/%{Lib_name}.%s" (build_dir kind `pack lib_name) lib_name (ext kind `ml)
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
        ] @
        c_opts
        @ [
          input;
          "-o"; output;
        ];
      }
    in
    { Build_graph.
      cmd;
      (* I don't think this actually does anything with the c archive... so it's
       * not actually a dependency? *)
      inputs = f [ input ];
      outputs = f [ output ];
    }

  let%expect_test _ =
    printf !"%{sexp:Build_graph.node}" (archive Native ~c_stubs:["blah"] (Lib_name.of_string "foo"));
    [%expect {|
      ((cmd
        ((exe ocamlfind)
         (args
          (ocamlopt -a -ccopt -L.dbuild/native/foo/c -cclib -lfoo
           .dbuild/native/foo/pack/foo.cmx -o .dbuild/native/foo/archive/foo.cmxa))
         (opam_switch (4.03.0))))
       (inputs (.dbuild/native/foo/pack/foo.cmx))
       (outputs (.dbuild/native/foo/archive/foo.cmxa))) |}];
  ;;

  let link kind pkgs libs module_name =
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
      Set.to_list libs
      |> List.map ~f:(fun lib ->
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
          "-package"; packages;
        ] @ input_archives
        @ [
          input_module;
          "-o"; output;
        ];
      }
    in
    { Build_graph.
      cmd;
      inputs = f (input_module :: input_archives);
      outputs = f [ output ];
    }

  let%expect_test _ =
    let pkgs = List.map ["a"; "b"] ~f:Package_name.of_string |> Package_name.Set.of_list in
    let libs = List.map ["x"; "y"] ~f:Lib_name.of_string |> Lib_name.Set.of_list in
    let module_name = Module_name.of_string "main" in
    printf !"%{sexp:Build_graph.node}" (link Native pkgs libs module_name);
    [%expect {|
      ((cmd
        ((exe ocamlfind)
         (args
          (ocamlopt -linkpkg -package a,b .dbuild/native/x/archive/x.cmxa
           .dbuild/native/y/archive/y.cmxa .dbuild/native/bin/modules/main.cmx -o
           .dbuild/native/bin/linked/main.native))
         (opam_switch (4.03.0))))
       (inputs
        (.dbuild/native/bin/modules/main.cmx .dbuild/native/x/archive/x.cmxa
         .dbuild/native/y/archive/y.cmxa))
       (outputs (.dbuild/native/bin/linked/main.native))) |}];
  ;;

end

let fold_closure ~key_set ~roots ~direct_deps ~init ~f =
  let rec loop worklist key_set acc =
    match worklist with
    | [] -> acc
    | node :: worklist ->
      let key_set = Set.add key_set node in
      let acc = f node acc in
      let worklist =
        List.fold ~init:worklist (direct_deps node) ~f:(fun worklist node ->
          if Set.mem key_set node
          then worklist
          else node :: worklist)
      in
      loop worklist key_set acc
  in
  assert (Set.is_empty key_set);
  loop roots key_set init
;;

let spec_to_nodes ~file_exists { Project_spec. libraries; binaries; } : Build_graph.node list =
  let of_lib { Project_spec. dir; modules_in_dep_order; c_stub_basenames; direct_deps = { packages; libs; }; } =
    let file module_name ext =
      sprintf !"%{Lib_name}/%{Module_name}.%s" dir module_name ext
    in
    let _, mli =
      List.fold modules_in_dep_order ~init:(Module_name.Set.empty, []) ~f:(fun (module_deps, mlis) module_name ->
        let mli = file module_name "mli" in
        if not (file_exists mli)
        then (Set.add module_deps module_name, mlis)
        else
          (* CR datkin: Confirm that the compiler kind doesn't matter here. *)
          let mli = Ocaml_compiler.compile Native packages libs dir module_deps module_name `mli in
          let module_deps = Set.add module_deps module_name in
          (module_deps, mli :: mlis))
    in
    let ml =
      List.concat_map [Native; Js;] ~f:(fun kind ->
        let _, mls =
          List.fold
            modules_in_dep_order
            ~init:(Module_name.Set.empty, [])
            ~f:(fun (module_deps, mls) module_name ->
              let ml =
                Ocaml_compiler.compile kind packages libs dir module_deps module_name `ml
              in
              let module_deps = Set.add module_deps module_name in
              (module_deps, ml :: mls))
        in
        let pack = Ocaml_compiler.pack kind ~modules_in_dep_order dir in
        let archive = Ocaml_compiler.archive kind ~c_stubs:c_stub_basenames dir in
        pack :: archive :: mls)
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
    List.concat_no_order [ mli; ml; c; ]
  in
  let deps_by_lib_name = List.map libraries ~f:(fun { Project_spec. dir; direct_deps; _ } ->
      dir, direct_deps)
    |> Lib_name.Map.of_alist_exn
  in
  let of_bin { Project_spec. module_name; direct_deps = { packages; libs; }; output; } =
    let { packages = extra_pkgs; libs; } : Project_spec.direct_deps =
      fold_closure
        ~key_set:Lib_name.Set.empty
        ~roots:(Set.to_list libs)
        ~direct_deps:(fun lib ->
          match Map.find deps_by_lib_name lib with
          | Some { Project_spec. packages = _; libs; } -> Set.to_list libs
          | None -> assert false)
        ~init:{ Project_spec.
            packages = Package_name.Set.empty;
            libs = Lib_name.Set.empty;
          }
        ~f:(fun lib { Project_spec. packages; libs; } ->
          match Map.find deps_by_lib_name lib with
          | Some { Project_spec. packages = p; libs = l; } ->
            { Project_spec.
              packages = Set.union packages p;
              libs = Set.union libs l;
            }
          | None -> assert false)
    in
    let packages = Set.union packages extra_pkgs in
    let kind = match output with `native -> Native | `js -> Js in
    [
      Ocaml_compiler.compile kind packages libs bin_lib Module_name.Set.empty module_name `ml;
      Ocaml_compiler.link kind packages libs module_name;
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
        (modules_in_dep_order (
          character_attributes
          character_set
          terminfo
          dec_private_mode
          control_functions
          window
        ))
        (direct_deps (
          (packages (core_kernel async_kernel))
        ))
      )
      (
        (dir odditty)
        (modules_in_dep_order (
          pty
          terminfo
        ))
        (direct_deps (
          (packages (core async))
          (libs (odditty_kernel))
        ))
      )
      (
        (dir web)
        (modules_in_dep_order (
          main
        ))
        (direct_deps (
          (packages (js_of_ocaml js_of_ocaml.async async_js virtual_dom))
          (libs ())
        ))
      )
      (
        (dir server)
        (modules_in_dep_order (
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
          (libs (odditty_kernel odditty))
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
    | "odditty_kernel/character_set.mli" -> false
    | _ -> true
  in
  printf !"%{sexp:Build_graph.node list}" (spec_to_nodes ~file_exists project_spec);
  [%expect {||}];
;;

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
    Command.async'
      ~summary:"X"
      [%map_open
        let () = return () in
        fun () ->
          Deferred.return ()
      ]
    |> Command.run
;;
