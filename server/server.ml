open Core.Std
open Async.Std

type pty_child = {
  fd : Core.Std.Unix.File_descr.t;
  pid : Pid.t;
  name : string;
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
      let { fd; pid; name } =
        fork_in_pty
          ~cwd:"/tmp"
          ~exe:"/bin/bash"
          ~argv:[| "bash"; "-l"; |]
          ~env:[|"TERM=xterm"; "PATH=/bin";|]
      in
      Core.Std.printf "name: %s\n%!" name;
      (* CR datkin: Get name and return it for info. *)
      let fd = Fd.create Char fd (Info.of_string "term") in
      let reader = Reader.create fd in
      let writer = Writer.create fd in
      Writer.write writer "\n";
      Writer.write writer "ls\n";
      Pipe.iter_without_pushback (Reader.pipe reader) ~f:(fun str ->
        (*Core.Std.printf "%s" str;*)
        String.iter str ~f:(fun char ->
          printf " %02x (%c)" (Char.to_int char) char)
      )
      >>= fun () ->
      Core.Std.printf "\n";
      Unix.waitpid_exn pid)

let () = Command.run command
