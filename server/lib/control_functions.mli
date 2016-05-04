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

val parser
  : unit
  -> (char -> [`literal of char | `func of t | `junk of string | `pending]) Staged.t

open Async.Std

val parse : Reader.t -> [`literal of char | `func of t | `junk of string] Pipe.Reader.t
