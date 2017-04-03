open Core_kernel.Std
open Async_kernel.Std

let log msg =
  Firebug.console##log (Js.string msg);
;;

let view { Odditty_kernel.Window.Rendered. chars; cursor; } =
  (* Per
   * http://stackoverflow.com/questions/19810122/how-do-i-add-a-non-breaking-whitespace-in-javascript-without-using-innerhtml
   * \xC2\xA0 = &nbsp;
   * *)
  let open Virtual_dom.Vdom in
  log (sprintf !"Cursor at %{sexp:Odditty_kernel.Window.coord}\n%!" cursor);
  let br = Node.create "br" [] [] in
  let rows : Virtual_dom.Vdom.Node.t list =
    List.concat_mapi chars ~f:(fun row_idx row ->
      List.mapi row ~f:(fun col_idx chr ->
        let node =
          if Char.(=) chr '\000'
          then Node.text "\xC2\xA0"
          else Node.text (Char.to_string chr)
        in
        let pos : Odditty_kernel.Window.coord = { x = col_idx; y = row_idx; } in
        if [%equal: Odditty_kernel.Window.coord] pos cursor
        then Node.span [Attr.create "class" "cursor";] [node]
        else node)
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
  let chr_to_str chr = String.of_char_list [ chr ] in
  let r, w = Pipe.create () in
  (* The key handlers return false to ensure the keys don't propogate elsewhere
   * (e.g. chrome handles 'tab'). *)
  Dom_html.document##.onkeydown := Dom_html.handler (fun ev ->
    let key =
      match Js.Optdef.to_option ev##.key |> Option.map ~f:Js.to_string with
      | Some "ArrowUp"    -> Some "\027[A"
      | Some "ArrowDown"  -> Some "\027[B"
      | Some "ArrowRight" -> Some "\027[C"
      | Some "ArrowLeft"  -> Some "\027[D"
      | Some ("Escape" | "Backspace" | "Space" | "Tab") ->
        let code =
          match Js.Optdef.to_option ev##.charCode with
          | Some code -> code
          | None -> ev##.keyCode
        in
        Option.try_with (fun () -> chr_to_str (Char.of_int_exn (ev##.keyCode)))
      | _ -> None
    in
    match key with
    | Some key -> Pipe.write_without_pushback w key; Js._false
    | None -> Js._true);
  (* Key press only works for keys that produce a "character" (regardless of
   * what modifies are pressed, it seems? *)
  Dom_html.document##.onkeypress := Dom_html.handler (fun ev ->
    let key =
      match Js.Optdef.to_option ev##.key |> Option.map ~f:Js.to_string with
      (*
      | Some ("Shift" | "Ctrl" | "Alt") -> None
      *)
      | Some key when 1 = String.length key ->
        if Js.to_bool ev##.ctrlKey
        then
          let key = String.get (String.uppercase key) 0 in
          if Char.(<=) '@' key && Char.(<=) key '_'
          then Some (chr_to_str (Char.to_int key - Char.to_int '@' |> Char.of_int_exn))
          else None
        else Some key
      | _ ->
        let code =
          match Js.Optdef.to_option ev##.charCode with
          | Some code -> code
          | None -> ev##.keyCode
        in
        Option.try_with (fun () -> chr_to_str (Char.of_int_exn (ev##.keyCode)))
    in
    match key with
    | Some key -> Pipe.write_without_pushback w key; Js._false
    | None -> Js._true);
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
        Pipe.iter_without_pushback keys ~f:(fun data ->
          log (sprintf "sending '%S'" data);
          Pipe.write_without_pushback to_ws data;
        )
      );
      let k = ref {
        Odditty_kernel.Window.Rendered.
        cursor = { x = 0; y = 0; };
        chars = [[]];
      }
      in
      let vdom = ref (view !k) in
      let elt = ref (Virtual_dom.Vdom.Node.to_dom !vdom :> Dom.element Js.t) in
      Dom.appendChild Dom_html.document##.body !elt;
      Pipe.iter_without_pushback from_ws ~f:(fun str ->
        let (rendered, _num_bytes_read) =
          Bigstring.of_string (B64.decode str)
          |> (fun buf ->
              Bigstring.read_bin_prot buf ~pos:0 [%bin_reader: Odditty_kernel.Window.Rendered.t])
          |> Or_error.ok_exn
        in
        let new_vdom = view rendered in
        elt := Node.Patch.apply (Node.Patch.create ~previous:!vdom ~current:new_vdom) !elt;
        vdom := new_vdom;
      )
    );
    Js._false
  );
  Deferred.never ()
;;
