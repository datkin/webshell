open Core.Std
open Async.Std

(* CR datkin: It seems that we also happen to have access directly to the
 * server_lib modules when compiling this (probably from the `include` rule in
 * _tags). See if we can fix that. *)
open Server_lib

let split_by_last_rendered
  (steps : (char * Control_functions.parse_result) list)
  : (char * Control_functions.parse_result) list * char list =
  let rec loop acc = function
    | (chr, `pending) :: rest -> loop (chr :: acc) rest
    | rest -> List.rev rest, List.rev acc
  in
  loop [] (List.rev steps)
;;

let tty_cmd =
  Command.async
    ~summary:"X"
    Command.Spec.(
      let dim = Arg_type.create Window.dim_of_string in
      empty
      +> flag "term" (optional string) ~doc:"name Terminfo name"
      +> flag "html" (optional string) ~doc:"file Write html files to given location"
      +> flag "dim" (optional dim) ~doc:"<width>x<height> Dimensions of terminal"
      +> flag "cwd" (required string) ~doc:"dir cwd"
      +> flag "exe" (required string) ~doc:"exe exe"
      +> flag "env" (listed string)
        ~doc:"VAR=val environment variable to export"
      +> flag "include-this-env" no_arg
        ~doc:" Use the current environment as the base environment"
      +> flag "--" escape ~doc:"args"
    )
    (fun term html dim cwd exe env include_this_env args () ->
      let dim = Option.value dim ~default:{ Window. width = 20; height = 15; } in
      let base_env =
        if include_this_env
        then Core.Std.Unix.environment () |> Array.to_list
        else []
      in
      let { Pty. fd; pid; name } =
        Pty.fork_in_pty
          ~cwd
          ~exe
          ~argv:(Array.of_list (Option.value args ~default:[]))
          ~env:(Array.of_list (base_env @ env))
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
        | None -> return Control_functions.Parser.xterm
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
            if String.is_empty str
            then ()
            else
            match Scanf.sscanf str "%S" (fun unescaped_str ->
              (* In theory, the unescaped str will look identical to the
               * string we just typed... *)
              Core.Std.printf "writing: %S\n%!" unescaped_str;
              (*
              String.iter str ~f:(fun char ->
                Core.Std.printf " %02x" (Char.to_int char));
              Core.Std.printf "\n";
              *)
              Writer.write writer unescaped_str)
            with
            | () -> ()
            | exception exn ->
              Core.Std.eprintf !"Error parsing '%s': %{Exn}\n%!" str exn
          )
      );
      Pipe.iter (Reader.pipe reader) ~f:(fun str ->
        (* The idea here is we want to show each character that was read, and
         * after each interpretable sequence, we print the interpretation.
         * We'd also like to render the screen periodically. The most sensible
         * time to render is at the end of each batch of input we read
         * (rendering more often is excessive, b/c we'll immediately re-render,
         * and rendering less often is bad b/c the screen will be stale while we
         * wait for input). Of course, we'd like all the bytes going into a
         * parse-result to be printe contiguously, w/o a render intervening in
         * the middle. Since we know that bytes that result in "pending" can't
         * actually have any impact on the screen, we'll actually print the
         * render results before we print the tail of the input buffer that
         * couldn't be immediately interpreted as an action. *)
        let pre_render_steps, post_render_steps =
          String.to_list str
          |> List.map ~f:(fun chr ->
            (* Core.Std.printf ">  '%s'\n%!" (String.escaped (Char.to_string chr)); *)
            let (parse_result, to_send) = Window.update window chr in
            begin
              match to_send with
              | None -> ()
              | Some str ->
                Core.Std.printf "Sending '%s'\n%!" (String.escaped str);
                Writer.write writer str
            end;
            chr, parse_result)
          |> split_by_last_rendered
        in
        let count = ref 0 in
        List.iter pre_render_steps ~f:(fun (_chr, parse_result) ->
          incr count;
          (*
          Core.Std.printf "  %02x (%s)\n%!"
            (Char.to_int chr)
            (String.escaped (Char.to_string chr));
            *)
          begin
            match parse_result with
            | `pending -> ()
            | `literal c ->
              Core.Std.printf "'%c', %!" c;
              if (!count % 20) = 0
              then Core.Std.printf "\n%!"
              else ()
            | _ ->
              Core.Std.printf !"%{sexp:Control_functions.parse_result}\n%!"
                parse_result
          end);
        begin
          if List.is_empty pre_render_steps
          then Deferred.unit (* the screen didn't upgade, don't redraw it *)
          else begin
            Core.Std.printf "=== start ===\n%s=== stop ===\n%!" (Window.render window);
            Option.value_map html ~default:Deferred.unit ~f:(fun file ->
              Writer.save ~temp_file:(file ^ ".tmp")
                file ~contents:(Window.render_html window))
          end
        end;
        >>| fun () ->
        List.iter post_render_steps ~f:(fun chr ->
          Core.Std.printf "  %02x (%s)\n%!"
            (Char.to_int chr)
            (String.escaped (Char.to_string chr)));
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

let test_cmd =
  Command.basic
    ~summary:"Set some termios settings and grab input."
    Command.Spec.(empty)
    (fun () ->
      let terminfo = Core.Std.Unix.Terminal_io.tcgetattr Core.Std.Unix.stdin in
      let newterminfo =
        { terminfo with
        c_icanon = false;
        c_istrip = false;
        c_isig = false;
        c_echo = false;
        c_vmin = 0;
        c_vtime = 0;
      } in
      Core.Std.Unix.Terminal_io.tcsetattr newterminfo Core.Std.Unix.stdin ~mode:TCSAFLUSH;
      at_exit (fun _ ->
        (* reset stdin when you quit *)
        Core.Std.Unix.Terminal_io.tcsetattr terminfo Core.Std.Unix.stdin ~mode:TCSAFLUSH
      );
      Core.Std.printf "Ok, waiting one second for input.\n%!";
      Core.Std.Unix.sleep 1;
      (*let c = In_channel.input_char In_channel.stdin |> Option.value_exn in*)
      let data = In_channel.input_all In_channel.stdin in
      Core.Std.printf "%s\n%!" (String.escaped data);
      )
;;

let () =
  Command.group
    ~summary:"tty tools" [
      "tty", tty_cmd;
      "terminfo", terminfo_cmd;
      "test", test_cmd;
    ]
  |> Command.run
