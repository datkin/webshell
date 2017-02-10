open Core.Std
open Odditty_kernel_x

type pty_child = {
  fd : Unix.File_descr.t;
  pid : Pid.t;
  name : string;
} [@@deriving sexp_of]

external fork_in_pty
  :  cwd:string
  -> exe:string
  -> argv:string array
  -> env:string array
  -> Window.dim
  -> pty_child
  = "fork_in_pty"
