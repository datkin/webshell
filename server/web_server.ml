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
    ~on_handler_error:(`Call (fun addr exn ->
      (* CR datkin: Should presumably send signal to the process? Or save it for
       * reconnection? *)
      Log.Global.sexp [%message "Connection error" (addr : Socket.Address.Inet.t) (exn : Exn.t)]
    ))
    (fun address reader writer ->
      let from_ws_r, from_ws_w = Pipe.create () in
      let to_ws_r, to_ws_w = Pipe.create () in
      don't_wait_for (
        Pipe.iter from_ws_r ~f:(fun frame ->
          Odditty.Pty.from_user pty frame.content
        )
      );
      don't_wait_for (
        Deferred.repeat_until_finished () (fun () ->
          let rendered = Odditty.Pty.window pty |> Odditty_kernel.Window.render in
          let size =
            Bin_prot.Utils.size_header_length
            + [%bin_writer: Odditty_kernel.Window.Rendered.t].size rendered
          in
          let buf = Bigstring.create size in
          let len =
            Bigstring.write_bin_prot
            buf ~pos:0 [%bin_writer: Odditty_kernel.Window.Rendered.t] rendered
          in
          assert (len = size);
          printf !"sending data length %d\n%!" size;
          let frame : Websocket_async.Frame.t = {
            opcode = Text;
            extension = 0;
            final = true;
            content = B64.encode (Bigstring.to_string buf);
          }
          in
          if Pipe.is_closed to_ws_w
          then return (`Finished ())
          else
            Pipe.write to_ws_w frame
            >>= fun () ->
            Odditty.Pty.changed pty
            >>= fun () ->
            return (`Repeat ())
        )
      );
      Log.Global.set_level `Debug;
      don't_wait_for (
        Websocket_async.server
          ~log:(force Log.Global.log)
          ~app_to_ws:to_ws_r
          ~ws_to_app:from_ws_w
          ~reader
          ~writer
          (address :> Socket.Address.t)
      );
      Reader.close_finished reader
      >>= fun () ->
      Pipe.close_read to_ws_r;
      Deferred.unit)
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
