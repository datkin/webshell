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
      +> flag "cwd" (required string) ~doc:"dir cwd"
      +> flag "exe" (required string) ~doc:"exe exe"
      +> flag "env" (listed string)
        ~doc:"VAR=val environment variable to export"
      +> flag "--" escape ~doc:"args"
    )
    (fun cwd exe env args () ->
      let dim = { Window. width = 20; height = 15; } in
      let { Pty. fd; pid; name } =
        Pty.fork_in_pty
          ~cwd
          ~exe
          ~argv:(Array.of_list (Option.value args ~default:[]))
          ~env:(Array.of_list env)
          dim
      in
      Core.Std.printf "name: %s\n%!" name;
      (* CR datkin: Get name and return it for info. *)
      let fd = Fd.create Char fd (Info.of_string "term") in
      let reader = Reader.create fd in
      let writer = Writer.create fd in
      let window = Window.create dim in
      don't_wait_for (
        Pipe.iter_without_pushback (Reader.lines (force Reader.stdin))
          ~f:(fun str ->
            let str =
              if str = ""
              then "\n"
              else str
            in
            Core.Std.printf "writing: ";
            String.iter str ~f:(fun char ->
              Core.Std.printf " %02x" (Char.to_int char));
            Core.Std.printf "\n";
            Writer.write writer str)
      );
      Pipe.iter_without_pushback (Reader.pipe reader) ~f:(fun str ->
        Core.Std.printf "receiving: ";
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
