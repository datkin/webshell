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

(* Translate key presses and other user events into actual bytes to send to the
 * pty. Eventually we'll probably need to model the input stream using something
 * more descriptive than strings, but this should do for now.
 *
 * We could in theory also have some of clock tick function, to determine how
 * the terminal behaves when a key is held down. However, in practice I think
 * the terminal behavior is always:
 *  - emit a key press immediately on key down
 *  - emit more key presses if the key continues to be held down for a period
 * This could all be handled in the driver that calls [from_user].
 *)
val from_user : t -> string -> string

val cursor : t -> coord

(*
(* CR datkin: Add style information. *)
(* Returns null byte for out of bounds coords. *)
val get : t -> coord -> Char.t
*)

(* CR-someday datkin: Perhaps this should take a write callback of some sort. *)
val render : t -> string
val render_html : t -> string
