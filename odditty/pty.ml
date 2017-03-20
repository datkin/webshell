open Core.Std
open Odditty_kernel

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

open Async.Std

type t = {
  window : Window.t;
  result : Unix.Exit_or_signal.t Deferred.t;
  writer : Writer.t;
  changed : unit Bvar.t;
}

let from_user t str =
  let to_send = Window.from_user t.window str in
  Writer.write t.writer to_send;
  Writer.flushed t.writer
;;

let changed t = Bvar.wait t.changed;;

let create ~cwd ~exe ~argv ~env dim =
  let changed = Bvar.create () in
  let { fd; pid; name } =
    fork_in_pty
      ~cwd
      ~exe
      ~argv
      ~env
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
  let result = Unix.waitpid pid in
  let fd = Fd.create Char fd (Info.create_s [%message "terminal" name]) in
  let reader = Reader.create fd in
  let writer = Writer.create fd in
  let parser = Control_functions.Parser.xterm in
  let window = Window.create dim parser in
  don't_wait_for (
    Pipe.iter_without_pushback (Reader.pipe reader) ~f:(fun str ->
      String.to_list str
      |> List.iter ~f:(fun chr ->
        let (parse_result, to_send) = Window.update window chr in
        begin
          match to_send with
          | None -> ()
          | Some str -> Writer.write writer str
        end;
        begin
          match parse_result with
          | `pending -> ()
          | `literal c ->
            Bvar.broadcast changed ();
            Core.Std.printf "'%c', %!" c;
          | _ ->
            Bvar.broadcast changed ();
            Core.Std.printf !"%{sexp:Control_functions.parse_result}\n%!"
              parse_result
        end;
      )
    )
  );
  {
    changed;
    window;
    result;
    writer;
  }
;;

let window t = t.window
