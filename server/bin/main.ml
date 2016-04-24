open Core.Std
open Async.Std

open Server_lib

let command =
  Command.async
    ~summary:"X"
    Command.Spec.(
      empty
    )
    (fun () ->
      let { Pty. fd; pid; name } =
        Pty.fork_in_pty
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
