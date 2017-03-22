open Core.Std
open Async.Std

type opcode = Websocket_async.Frame.Opcode.t =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong
  | Ctrl of int
  | Nonctrl of int
[@@deriving sexp]

type frame = Websocket_async.Frame.t = {
  opcode : opcode;
  extension : int;
  final : bool;
  content : string;
} [@@deriving sexp]

let run ~ws_port ~http_port =
  let pty =
    Odditty.Pty.create
      ~cwd:"/Users/datkin"
      ~exe:"/bin/bash"
      ~argv:[| "bash"; |]
      ~env:[| "TERM=xterm"; "HOME=/Users/datkin"; |]
      (* CR datkin: Increasing window size leads to async kernel stack
       * overflows... *)
      { Odditty_kernel.Window. width = 50; height = 30; }
  in
  Tcp.Server.create
    (Tcp.on_port ws_port)
    (fun address reader writer ->
      let from_ws_r, from_ws_w = Pipe.create () in
      let to_ws_r, to_ws_w = Pipe.create () in
      don't_wait_for (
        Pipe.iter from_ws_r ~f:(fun frame ->
          let chr = Char.of_string frame.content in
          let data = String.of_char_list [ chr ] in
          Odditty.Pty.from_user pty data
        )
      );
      Deferred.forever () (fun () ->
        Odditty.Pty.changed pty
        >>= fun () ->
        let chrs = Odditty.Pty.window pty |> Odditty_kernel.Window.to_lists in
        let frame : Websocket_async.Frame.t = {
          opcode = Text;
          extension = 0;
          final = true;
          content = Sexp.to_string ([%sexp_of: char list list] chrs);
        }
        in
        Pipe.write to_ws_w frame
      );
      Log.Global.set_level `Debug;
      Websocket_async.server
        ~log:(force Log.Global.log)
        ~app_to_ws:to_ws_r
        ~ws_to_app:from_ws_w
        ~reader
        ~writer
        (address :> Socket.Address.t))
  >>= fun server ->
  Tcp.Server.close_finished server

let run_cmd =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"Run the web server!"
    [%map_open
      let ws_port =
        flag "ws-port" (optional_with_default 8081 int) ~doc:"int ws:// port"
      and http_port =
        flag "http-port" (optional_with_default 8080 int) ~doc:"int http:// port"
      in
      fun () -> run ~ws_port ~http_port]
;;

let command =
  Command.group
    ~summary:"Web server commands" [
      "run", run_cmd;
  ]
