open Core_kernel.Std

(* Terminfo defines a bunch of standard capabilities of terminals (the follow
 * info is mostly gleaned from `man 5 terminfo`). There are three kinds of
 * properties:
 *
 *   (1) boolean capabilities, i.e. behaviours that a terminal might have. E.g.
 *
 *         auto_right_margin (am): Does the terminal wrap to the next line when
 *         you overflow the right margin?
 *
 *         In the file you'd see: ..., am, ...
 *
 *   (2) numeric capabilities, i.e. static properties of terminal. E.g.
 *
 *         columns (cols): How many characters wide is the terminal?
 *
 *         In the file you'd see: ..., cols#80, ...
 *
 *   (3) string capabilities, i.e. what string do you send to the terminal to
 *       make it perform a certain operation? An operation may have certain
 *       parameters, and terminfo assumes the parameters are passed on a stack.
 *       E.g.
 *
 *        parm_left_cursor (cub): move cursor #1 characters to the left
 *
 *        In the file you'd see: ..., cub=\E[%p1%dD, ...
 *
 *       You can think of [parm_left_cursor] as function which takes parameters
 *       via some stack. The right-hand-side of the expression is a printf-like
 *       rule for constucting the string an application would send to the
 *       terminal to have that operation performed. In practice, applications
 *       usually ask a library like [ncurses] to perform the generic operation,
 *       and then ncurses renders the proper sequence to send the terminal.
 *
 *       E.g., for the cub capability in the example above, calling
 *       [parm_left_cursor 3] (to move the cursor left by 3), you'd send the
 *       terminal '\E3D'.
 *
 *       The format specification language is fairly rich.
 *)
type value =
  | Bool of bool (* True if present? *) (* CR datkin: Just change this to [Available]? *)
  | Number of int
  | String of string
[@@deriving sexp]

type t [@@deriving sexp]

val capabilities : t -> value String.Map.t

val parse_entry : string -> (string * value) Or_error.t

(* Parse the raw content of `infocmp` command output. *)
val parse : string -> t Or_error.t
