open Core_kernel.Std
open Async_kernel.Std

let log msg =
  Firebug.console##log (Js.string msg);
;;

let view (rows : char list list) =
  (* Per
   * http://stackoverflow.com/questions/19810122/how-do-i-add-a-non-breaking-whitespace-in-javascript-without-using-innerhtml
   * \xC2\xA0 = &nbsp;
   * *)
  let open Virtual_dom.Vdom in
  let br = Node.create "br" [] [] in
  let rows : Virtual_dom.Vdom.Node.t list =
    List.concat_map rows ~f:(fun row ->
      List.map row ~f:(fun chr ->
        if Char.(=) chr '\000'
        then Node.text "\xC2\xA0"
        else Node.text (Char.to_string chr))
      @ [br]
    )
  in
  Node.div [
    Attr.class_ "terminal";
  ] rows
;;

let make_ws ~url =
  let from_ws_r, from_ws_w = Pipe.create () in
  let to_ws_r, to_ws_w = Pipe.create () in
  let ws = new%js WebSockets.webSocket (Js.string url) in
  ws##.onmessage :=
    Dom.handler (fun message ->
      let data = (message##.data) |> Js.to_string in
      log (sprintf "data length %d" (String.length data));
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

let keys =
  let r, w = Pipe.create () in
  Dom_html.document##.onkeyup := Dom_html.handler (fun ev ->
    let key =
      match Js.Optdef.to_option ev##.key |> Option.map ~f:Js.to_string with
      | Some key when 1 = String.length key -> String.get key 0
      | _ ->
        let code =
          match Js.Optdef.to_option ev##.charCode with
          | Some code -> code
          | None -> ev##.keyCode
        in
        try
          Char.of_int_exn (ev##.keyCode)
        with Invalid_argument _ -> '\000'
    in
    Pipe.write_without_pushback w key;
    Js._true);
  r
;;

let run () : unit Deferred.t =
  let open Virtual_dom.Vdom in
  Dom_html.window##.onload := Dom.handler (fun _ ->
    Firebug.console##log (Js.string "onload callback");
    don't_wait_for (
      make_ws ~url:"ws://localhost:8081"
      >>= fun (from_ws, to_ws) ->
      don't_wait_for (
        Pipe.iter_without_pushback keys ~f:(fun key ->
          let message = Char.to_string key in
          log (sprintf "sending %d" (Char.to_int key));
          Pipe.write_without_pushback to_ws message;
        )
      );
      let k    = ref [] in
      let vdom = ref (view !k) in
      let elt  = ref (Virtual_dom.Vdom.Node.to_dom !vdom :> Dom.element Js.t) in
      Dom.appendChild Dom_html.document##.body !elt;
      Pipe.iter_without_pushback from_ws ~f:(fun str ->
        (*
        let chrs = [%of_sexp: char list list] (Sexp.of_string str) in
*)
        let (chrs, _num_bytes_read) =
          Bigstring.of_string (B64.decode str)
          |> (fun buf ->
              Bigstring.read_bin_prot buf ~pos:0 [%bin_reader: char list list])
          |> Or_error.ok_exn
        in
        let new_vdom = view chrs in
        elt := Node.Patch.apply (Node.Patch.create ~previous:!vdom ~current:new_vdom) !elt;
        vdom := new_vdom;
        (*
        Firebug.console##log (Js.string (sprintf !"received %{sexp:char list list}" chrs))
      *)
      )
    );
    Js._false
  );
  Deferred.never ()
;;
