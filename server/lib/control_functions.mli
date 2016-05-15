open Core.Std

module Parser : sig
  type state
end

val default_parser : Parser.state

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

type coord = {
  x : int;
  y : int;
} [@@deriving sexp, compare]

type t =
  | Ack
  | Bell
  | Insert_blank of int
  | Cursor_rel of dir * int
  | Start_of_line_rel of up_or_down * int
  | Cursor_abs of coord
[@@deriving sexp, compare]

val parser
  : Parser.state
  -> (char -> [`literal of char | `func of t | `junk of string | `pending]) Staged.t

open Async.Std

val parse : Reader.t -> Parser.state -> [`literal of char | `func of t | `junk of string] Pipe.Reader.t
