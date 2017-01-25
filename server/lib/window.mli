open Core_kernel.Std

type dim = {
  width : int;
  height : int;
}

(* Convert "<width>x<height>" *)
val dim_of_string : string -> dim

(* The origin is the top-left corner. *)
type coord = {
  y : int;
  x : int;
}

type t

val create : dim -> Control_functions.Parser.state -> t

val set_dimensions : t -> dim -> unit

(* CR-soon datkin: Perhaps [Window.t] should just take the actual parse results,
 * rather than doing the parsing. It's unclear if the window needs to interact
 * with the parser or not. *)
(* CR-someday datkin: Return deltas to the screen? *)
(* Returns the parse_result but also applies the change. *)
val update : t -> char -> (Control_functions.parse_result * string option)

val cursor : t -> coord

(*
(* CR datkin: Add style information. *)
(* Returns null byte for out of bounds coords. *)
val get : t -> coord -> Char.t
*)

(* CR-someday datkin: Perhaps this should take a write callback of some sort. *)
val render : t -> string
val render_html : t -> string
