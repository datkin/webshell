open Core.Std
open Async.Std

type value =
  | Bool of bool (* True if present? *)
  | Number of int
  | String of string
[@@deriving sexp]

type t = {
  names : string * string list;
  capabilities : value String.Map.t;
} [@@deriving sexp]

let parse _db = {
  names = "foo", [];
  capabilities = String.Map.empty;
}

let load name =
  Process.create ~prog:"infocmp" ~args:[name] ()
  >>=? fun proc ->
  Process.collect_output_and_wait proc
  >>= fun output ->
  match output.exit_status with
  | Ok () -> return (Ok (parse output.stdout))
  | Error _ ->
    return (Or_error.error "infocmp failed" (name, output) [%sexp_of: (string * Process.Output.t)])
