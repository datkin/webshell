open Core.Std
open Async.Std

type value =
  | Bool of bool (* True if present? *)
  | Number of int
  | String of string
[@@deriving sexp]

type t [@@deriving sexp]

val capabilities : t -> value String.Map.t

val load : string -> t Or_error.t Deferred.t
