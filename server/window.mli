open Core_kernel.Std

type dim = {
  width : int;
  height : int;
}

type coord = {
  x : int;
  y : int;
}

type t

val create : dim -> t

val set_dimensions : t -> dim -> unit

(* CR datkin: Return deltas to the screen? *)
val update : t -> string -> units

val cursor : t -> coord

(* CR datkin: Add style information. *)
(* Returns null byte for out of bounds coords. *)
val get : t -> coord -> Char.t
