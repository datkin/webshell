open Core.Std
open Async.Std

let run_cmd =
  let open Command.Let_syntax in
  Command.async'
    ~summary:"Run the web server!"
    [%map_open
      let arg = flag "x" (optional string) ~doc:"str Some random string"
      in
      fun () ->
        printf "Hello, goodbye, %s\n%!" (Option.value arg ~default:"mumble");
        Deferred.unit
    ]
;;

let command =
  Command.group
    ~summary:"Web server commands" [
      "run", run_cmd;
  ]
