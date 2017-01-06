open Core.Std
open Async.Std

(* CR datkin: It seems that we also happen to have access directly to the
 * server_lib modules when compiling this (probably from the `include` rule in
 * _tags). See if we can fix that. *)
open Server_lib

let tty_cmd =
  Command.async
    ~summary:"X"
    Command.Spec.(
      empty
      +> flag "term" (optional string) ~doc:"name Terminfo name"
      +> flag "cwd" (required string) ~doc:"dir cwd"
      +> flag "exe" (required string) ~doc:"exe exe"
      +> flag "env" (listed string)
        ~doc:"VAR=val environment variable to export"
      +> flag "--" escape ~doc:"args"
    )
    (fun term cwd exe env args () ->
      let dim = { Window. width = 20; height = 15; } in
      let { Pty. fd; pid; name } =
        Pty.fork_in_pty
          ~cwd
          ~exe
          ~argv:(Array.of_list (Option.value args ~default:[]))
          ~env:(Array.of_list env)
          dim
      in
      (* CR datkin: If the program fails to exec (e.g. b/c you give an [exe]
       * that can't be found, you get an error like this, and weirdly
       * (presumably because the fd's don't get repointed?) this error message
       * goes into stdin...
       * (monitor.ml.Error
       *  ("child process didn't exit with status zero" (child_pid 38270)
       *    (exit_or_signal (Error (Exit_non_zero 1))))
       *     ("Called from file \"src/deferred1.ml\", line 20, characters 40-45"
       *       "Called from file \"src/job_queue.ml\", line 159, characters 6-47"))
       *)
      Core.Std.printf "name: %s\n%!" name;
      (* CR datkin: Get name and return it for info. *)
      let fd = Fd.create Char fd (Info.of_string "term") in
      let reader = Reader.create fd in
      let writer = Writer.create fd in
      begin
        match term with
        | None -> return Control_functions.Parser.default
        | Some term ->
          Terminfo.load term
          >>= fun terminfo ->
          let terminfo = Or_error.ok_exn terminfo in
          return (Control_functions.Parser.of_terminfo terminfo)
      end
      >>= fun parser ->
      let window = Window.create dim parser in
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
        Core.Std.printf " (%s)" (String.escaped str);
        Core.Std.printf "\n%!";
        Window.update window str;
        Window.render window Out_channel.stdout;
      )
      >>= fun () ->
      Core.Std.printf "\n";
      Unix.waitpid_exn pid)

let terminfo_cmd =
  Command.async_or_error
    ~summary:"Get terminfo"
    Command.Spec.(empty +> anon ("term" %: string))
    (fun term () ->
      Terminfo.load term
      >>=? fun info ->
      Core.Std.printf !"%{sexp:Terminfo.t}\n%!" info;
      return (Ok ()))
;;

let () =
  Command.group
    ~summary:"tty tools" [
      "tty", tty_cmd;
      "terminfo", terminfo_cmd;
    ]
  |> Command.run
