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
      let parser =
        Control_functions.default_parser
      in
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
