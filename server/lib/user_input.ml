open Core_kernel.Std

type modifiers = {
  control : bool;
  alt : bool;
  meta : bool;
} [@@deriving sexp, compare]

type key =
  | Arrow_up
  | Arrow_down
  | Arrow_left
  | Arrow_right
  | Page_up
  | Page_down
  | Home
  | End
  | Backspace
  | Delete
  | Insert
  (* CR datkin: How will unicode characters work? Lul. Sigh. *)
  | Char of Char.t

type t = {
  modifiers : modifiers;
  key : char;
} [@@deriving sexp, compare]
