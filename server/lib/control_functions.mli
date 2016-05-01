open Core.Std

type dir =
  | Up
  | Down
  | Left
  | Right

type t =
  | Ack
  | Bell
  | Insert_blank of int
  | Cursor of dir * int

val parse : Reader.t -> [`literal of char | `function of t | `junk of string] Pipe.Reader.t
