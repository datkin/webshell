open Core.Std
open Async.Std

type pty_child = {
  fd : Core.Std.Unix.File_descr.t;
  pid : Pid.t;
} (*[@@deriving sexp_of]*)

external fork_in_pty :
  cwd:string -> exe:string -> argv:string array -> env:string array -> pty_child =
    "fork_in_pty"

let command =
  Command.async
    ~summary:"X"
    Command.Spec.(
      empty
    )
    (fun () ->
      let { fd; pid; } =
        fork_in_pty
          ~cwd:"/tmp"
          ~exe:"/usr/bin/clear"
          ~argv:[| "foo" |]
          ~env:[|"TERM=xterm"|]
      in
      (* CR datkin: Get name and return it for info. *)
      let reader = Reader.create (Fd.create Char fd (Info.of_string "term")) in
      Pipe.iter_without_pushback (Reader.pipe reader) ~f:(fun str ->
        String.iter str ~f:(fun char ->
          printf " %02x (%c)" (Char.to_int char) char)
      )
      >>= fun () ->
      Core.Std.printf "\n";
      Unix.waitpid_exn pid)

let () = Command.run command
