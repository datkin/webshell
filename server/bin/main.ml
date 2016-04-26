open Core.Std
open Async.Std

(* CR datkin: It seems that we also happen to have access directly to the
 * server_lib modules when compiling this (probably from the `include` rule in
 * _tags). See if we can fix that. *)
open Server_lib

let command =
  Command.async
    ~summary:"X"
    Command.Spec.(
      empty
    )
    (fun () ->
      let dim = { Window. width = 20; height = 15; } in
      let { Pty. fd; pid; name } =
        Pty.fork_in_pty
          ~cwd:"/tmp"
          ~exe:"/bin/bash"
          ~argv:[| "bash"; "-l"; |]
          ~env:[|"TERM=xterm"; "PATH=/bin";|]
          dim
      in
      Core.Std.printf "name: %s\n%!" name;
      (* CR datkin: Get name and return it for info. *)
      let fd = Fd.create Char fd (Info.of_string "term") in
      let reader = Reader.create fd in
      let writer = Writer.create fd in
      Clock.after (sec 0.1)
      >>= fun () ->
      Writer.write writer "\n";
      Writer.write writer "ls -1\n";
      Writer.write writer "echo \"hello\"";
      Clock.after (sec 0.1)
      >>= fun () ->
      Writer.write writer "\n";
      let window = Window.create dim in
      Pipe.iter_without_pushback (Reader.pipe reader) ~f:(fun str ->
        String.iter str ~f:(fun char ->
          Core.Std.printf " %02x" (Char.to_int char);
          (*
          if Char.is_alphanum char
          then Core.Std.printf " (%c)" char;
          *)
        );
        Core.Std.printf "\n%!";
        Window.update window str;
        Window.render window Out_channel.stdout;
      )
      >>= fun () ->
      Core.Std.printf "\n";
      Unix.waitpid_exn pid)

let () = Command.run command
