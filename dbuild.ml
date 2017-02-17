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

(*
type file_cache : md5sum Filename.Map.t

module Build_graph : sig
  type t
end
*)

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

module Project_spec = struct
  type deps = {
    direct_packages_deps : Package_name.Set.t; (* i.e. findlib packages *)
    direct_lib_deps : Lib_name.Set.t; (* i.e. other non-findlib packages *)
  } [@@deriving sexp]

  type library = {
    dir : Lib_name.t;
    modules_in_dep_order : Module_name.t list;
    deps : deps;
  } [@@deriving sexp]

  type binary = {
    file : string;
    deps : deps;
    output : [`native | `js];
  } [@@deriving sexp]

  type t = {
    libraries : library list;
    binaries : binary list;
  } [@@deriving sexp]
end

(*
val build : Build_graph.t -> file_cache -> unit Deferred.Or_error.t

val to_graph : Project_spec.t -> Build_graph.t Deferred.Or_error.t
*)

let%expect_test _ =
  [%expect "X"];
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
