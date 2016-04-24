open Core.Std

type pty_child = {
  fd : Core.Std.Unix.File_descr.t;
  pid : Pid.t;
  name : string;
} (*[@@deriving sexp_of]*)

val fork_in_pty : cwd:string -> exe:string -> argv:string array -> env:string array -> pty_child
