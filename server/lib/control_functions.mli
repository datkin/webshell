open Core.Std

module Parser : sig
  type state

  val of_terminfo : Terminfo.t -> state

  val default : state (* Some incomplete thing used for testing *)
  val xterm : state
end

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
  | Erase_line_including_cursor of [ `Left | `Right | `All ] (* http://www.vt100.net/docs/vt510-rm/EL.html *)
  | Set_scrolling_region of { top : int option; bottom : int option } (* http://www.vt100.net/docs/vt510-rm/DECSTBM.html *)
  | Other of (string list * int option list)
[@@deriving sexp, compare]

type parse_result = [
  | `literal of char
  | `func of (t * string)
  | `junk of string
  | `pending
] [@@deriving sexp]

val parser : Parser.state -> (char -> parse_result) Staged.t

open Async.Std

val parse : Reader.t -> Parser.state -> parse_result Pipe.Reader.t
