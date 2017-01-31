open Core_kernel.Std

type key =
  | Ctrl_left
  | Ctrl_right
  | Alt_left
  | Alt_right
  | Meta_left
  | Meta_right
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

type t =
  | Key_down of key
  | Key_up of key
  [@@deriving sexp, bin_io, compare]
