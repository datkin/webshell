open Core_kernel.Std
open Async_kernel.Std

let () = Async_js.init ()

let view (_ : 'a) =
  let open Virtual_dom.Vdom in
  Node.div [
    Attr.class_ "terminal";
  ] [
    Node.text "&nbsp;";
    Node.text "foobar";
  ]
;;

let vdom_loop () =
  let open Virtual_dom.Vdom in
  Dom_html.window##.onload := Dom.handler (fun _ ->
    Firebug.console##log (Js.string "onload callback");
    let k    = ref 0 in
    let vdom = ref (view !k) in
    let elt  = ref (Virtual_dom.Vdom.Node.to_dom !vdom :> Dom.element Js.t) in
    Dom.appendChild Dom_html.document##.body !elt;
    Dom_html.window##setInterval (Js.wrap_callback (fun _ ->
      incr k;
      let new_vdom = view !k in
      elt := Node.Patch.apply (Node.Patch.create ~previous:!vdom ~current:new_vdom) !elt;
      vdom := new_vdom
    )) 100. |> ignore;
    Js._false
  )

let make_ws ~url =
  let from_ws_r, from_ws_w = Pipe.create () in
  let to_ws_r, to_ws_w = Pipe.create () in
  let ws = new%js WebSockets.webSocket (Js.string url) in
  ws##.onmessage :=
    Dom.handler (fun message ->
      let data = (message##.data) |> Js.to_string in
      Pipe.write_without_pushback from_ws_w data;
      Js._true);
  Deferred.create (fun connected ->
    ws##.onopen :=
      Dom.handler (fun _ ->
        Ivar.fill connected ();
        Js._true))
  >>= fun () ->
  don't_wait_for (
    Pipe.iter_without_pushback to_ws_r ~f:(fun str ->
      ws##send (Js.string str))
  );
  return (from_ws_r, to_ws_w)
;;

type event =
  | Message of WebSockets.webSocket WebSockets.messageEvent Js.t
  | Key of Dom_html.keyboardEvent Js.t


let log_and_send_time () =
  Firebug.console##log (Js.string "started");
  don't_wait_for (
    make_ws ~url:"ws://localhost:8081"
    >>= fun (from_ws, to_ws) ->
    Clock_ns.every (Time_ns.Span.of_int_sec 1) (fun () ->
      let ns_since_epoch = Time_ns.to_int63_ns_since_epoch (Time_ns.now ()) in
      let message = sprintf !"%{Int63}" ns_since_epoch in
      Firebug.console##log (Js.string ("sending: " ^ message));
      Pipe.write_without_pushback to_ws message;
    );
    Pipe.iter_without_pushback from_ws ~f:(fun str ->
      Firebug.console##log (Js.string ("received: " ^ str)))
  )
;;
