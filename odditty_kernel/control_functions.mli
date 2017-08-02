open Core_kernel.Std

module Parser : sig
  type state

  val of_terminfo : Terminfo.t -> state

  val default : state (* Some incomplete thing used for testing *)
  val xterm : state
end

type dir =
  | Down
  | Right
[@@deriving sexp, compare]

type coord = {
  y : int;
  x : int;
} [@@deriving sexp, compare]

type t =
  | Ack
  | Bell
  | Insert_blank of int
  | Cursor_rel of dir * int
  | Start_of_line_rel of [`Down] * int
  | Cursor_abs of coord
  | Delete_chars of int
  | Erase_line_including_cursor of [ `Left | `Right | `All ] (* http://www.vt100.net/docs/vt510-rm/EL.html *)
  | Erase_display_including_cursor of [ `From_start | `To_end | `All ] (* http://www.vt100.net/docs/vt510-rm/ED.html *)
  | Set_scrolling_region of { top : int option; bottom : int option } (* http://www.vt100.net/docs/vt510-rm/DECSTBM.html *)
  | Dec_mode of [ `set | `clear ] * Dec_private_mode.t list
  | Designate_char_set of { g : int; character_set : Character_set.t }
  | Send_device_attribute of [ `primary | `secondary ]
  | Other of (string list * int option list)
[@@deriving sexp, compare]

type parse_result = [
  | `literal of char
  | `func of (t * string)
  | `junk of string
  | `pending
] [@@deriving sexp]

val parser : Parser.state -> (char -> parse_result) Staged.t

open Async_kernel

val parse : string Pipe.Reader.t -> Parser.state -> parse_result Pipe.Reader.t
