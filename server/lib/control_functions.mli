open Core.Std

type dir =
  | Up
  | Down
  | Left
  | Right
[@@deriving sexp, compare]

type up_or_down =
  | Up
  | Down
[@@deriving sexp, compare]

type t (*=
  | Ack
  | Bell
  | Insert_blank of int
  | Move_cursor of dir * int
  | Start_of_line of up_or_down * int
  *)
[@@deriving sexp, compare]

val parser
  : unit
  -> (char -> [`literal of char | `func of t | `junk of string | `pending]) Staged.t

open Async.Std

val parse : Reader.t -> [`literal of char | `func of t | `junk of string] Pipe.Reader.t
