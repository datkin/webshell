open Core.Std
open Async.Std

type pty_child = {
  fd : Unix.File_descr.t;
  pid : Pid.t;
} (*[@@deriving sexp_of]*)

external fork_in_pty :
  exe:string -> argv:string array -> env:string array -> pty_child =
    "fork_in_pty"
