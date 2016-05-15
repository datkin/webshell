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

val create : dim -> Control_functions.Parser.state -> t

val set_dimensions : t -> dim -> unit

(* CR datkin: Return deltas to the screen? *)
val update : t -> string -> unit

val cursor : t -> coord

(* CR datkin: Add style information. *)
(* Returns null byte for out of bounds coords. *)
val get : t -> coord -> Char.t

val render : t -> out_channel -> unit
