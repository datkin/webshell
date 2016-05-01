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

val parse : char list -> [ `code of t | `not_a_code | `need_more ]
