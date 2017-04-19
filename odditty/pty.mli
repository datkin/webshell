open Core.Std
open Odditty_kernel

type pty_child = {
  fd : Unix.File_descr.t;
  pid : Pid.t;
  name : string;
} [@@deriving sexp_of]

val fork_in_pty
  :  cwd:string
  -> exe:string
  -> argv:string array
  -> env:string array
  -> Window.dim
  -> pty_child

open Async.Std

type t

val window : t -> Window.t

val changed : t -> unit Deferred.t

val create
  :  cwd:string
  -> exe:string
  -> argv:string array
  -> env:string array
  -> Window.dim
  -> scrollback:int
  -> t

val from_user : t -> string -> unit Deferred.t
