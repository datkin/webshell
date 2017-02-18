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

module Package_name = String_id.Make (struct
  let module_name = "Package_name"
end) ()

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
    packages : Package_name.Set.t; (* i.e. findlib packages *)
    libs : Lib_name.Set.t; (* i.e. other non-findlib packages *)
  } [@@deriving sexp]

  type library = {
    dir : Lib_name.t;
    modules_in_dep_order : Module_name.t list;
    direct_deps : direct_deps;
  } [@@deriving sexp]

  type binary = {
    file : string;
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

module Ocaml_compiler : sig
  val compile
    :  kind
    -> Package_name.Set.t
    -> Lib_name.Set.t
    -> Lib_name.t
    -> Module_name.t
    -> [ `ml | `mli ]
    -> Cmd.t

    (*
  val pack : unit

  val archive : unit

  val link : unit
  *)
end = struct

  let opam_switch = function
    | Native -> opam_native
    | Js -> opam_js

  let ocamlc = function
    | Native -> "ocamlopt"
    | Js -> "ocamlc"

  let build_dir kind lib_name =
    sprintf !".dbuild/%{sexp:kind}/%{Lib_name}/modules/" kind lib_name
    |> String.lowercase

  let compile kind pkgs libs lib_name module_name which_file =
    let ext =
      match kind, which_file with
      | Native, `ml -> "cmx"
      | Js, `ml -> "cmo"
      | _, `mli -> "cmi"
    in
    let maybe_js_ppx =
      match kind with
      | Native -> []
      | Js -> ["-ppx"; "$(opam config var lib)/js_of_ocaml/ppx_js"]
    in
    let extra_includes =
      Set.to_list libs
      |> List.concat_map ~f:(fun lib ->
          [ "-I"; build_dir kind lib; ])
    in
    let build_dir = build_dir kind lib_name in
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
        "-for-pack"; Lib_name.to_string lib_name |> String.uppercase;
        "-c"; sprintf !"%{Lib_name}/%{Module_name}.%s" lib_name module_name (match which_file with | `ml -> "ml" | `mli -> "mli");
        "-o"; sprintf !"%s/%{Module_name}.%s" build_dir module_name ext;
      ];
    }
end

let spec_to_nodes { Project_spec. libraries; binaries; } : Build_graph.node list =
  (* CR datkin: Need to track library dep closure. *)
  let of_lib { Project_spec. dir; modules_in_dep_order; direct_deps = { packages; libs; }; } =
    ignore dir;
    ignore modules_in_dep_order;
    ignore packages;
    ignore libs;
    []
  in
  let of_bin { Project_spec. file; direct_deps = { packages; libs; }; output; } =
    ignore file;
    ignore packages;
    ignore libs;
    ignore output;
    []
  in
  let libs = List.concat_map libraries ~f:of_lib in
  let bins = List.concat_map binaries ~f:of_bin in
  List.concat_no_order [libs; bins;]
;;

let project_spec =
  [%of_sexp: Project_spec.t] (Sexp.of_string {|
  (
    (libraries (
    ))
    (binaries (
    ))
  )
  |})

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
