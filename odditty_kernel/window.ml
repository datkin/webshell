open Core_kernel.Std

type dim = {
  width : int;
  height : int;
} [@@deriving sexp, compare]

let dim_of_string str =
  match String.split str ~on:'x' with
  | [width; height] ->
    { width = Int.of_string width; height = Int.of_string height; }
  | _ -> failwithf {|dim_of_string "%S"|} str ()
;;

type coord = Control_functions.coord = {
  y : int;
  x : int;
} [@@deriving sexp, compare]

let%test_unit _ =
  let coords = [
    { y = 0; x = 20; };
    { y = 5; x = 4; };
    { y = 5; x = 5; };
    { y = 5; x = 6; };
    { y = 20; x = 0; };
  ]
  in
  assert (List.is_sorted ~compare:compare_coord coords);
;;

let origin = { x = 0; y = 0; }

let null_byte = '\000'

module Attributes : sig
  type t [@@deriving sexp]

  val plain : t

  val clear : t -> unit

  val copy : src:t -> dst:t -> unit
end = struct
  type t = unit [@@deriving sexp]

  let plain = ()

  let clear () = ()

  let copy ~src:() ~dst:() = ()
end

module Cell : sig
  type t [@@deriving sexp]

  val init : unit -> t

  val create : Char.t -> Attributes.t -> t

  val code : t -> Char.t
  val attributes : t -> Attributes.t

  val clear : t -> unit
  val clear_code : t -> unit
  val set_code : t -> Char.t -> unit
end = struct
  type t = {
    mutable code : Char.t;
    attributes : Attributes.t;
  } [@@deriving sexp, fields]

  let init () = { code = null_byte; attributes = Attributes.plain; }
  let create code attributes = { code; attributes; }

  let clear_code t =
    t.code <- null_byte;
  ;;

  let clear t =
    clear_code t;
    Attributes.clear t.attributes;
  ;;

  let set_code t chr = t.code <- chr
end

module Grid : sig
  type t [@@deriving sexp_of]

  val create : dim -> t

  val dim : t -> dim
  val set_dim : t -> dim -> unit

  val get : t -> coord -> Cell.t
  (*
  val set : t -> coord -> Cell.t -> unit
  *)

  val clear_all : t -> unit
  (*
  val clear_cell : t -> coord -> unit
  *)

  val fold : t -> init:'a -> f:('a -> x:int -> y:int -> Cell.t -> 'a) -> 'a

  val scroll : t -> int -> unit
end = struct
  type t = {
    mutable dim : dim;
    mutable data : Cell.t array array; (* outer array is row *)
    mutable first_row : int;
    mutable num_rows : int;
    current_attributes : Attributes.t;
  } [@@deriving sexp_of, fields]

  let create dim =
    let t = {
      dim;
      (* This is a bit dumb/confusing. We're doing our array row-major, not column
       * major. But here [dimx] corresponds to the major dimension. *)
      data = Array.make_matrix ~dimx:dim.height ~dimy:dim.width (Cell.init ());
      first_row = 0;
      num_rows = 0;
      current_attributes = Attributes.plain;
    }
    in
    for y = 0 to t.dim.height - 1 do
      for x = 0 to t.dim.width - 1 do
        t.data.(y).(x) <- Cell.init ();
      done
    done;
    t

  let invariant t =
    assert (Array.length t.data = t.dim.height);
    for y = 0 to t.dim.height - 1; do
      assert (Array.length t.data.(y) = t.dim.width);
    done;
    assert (t.first_row >= 0);
    assert (t.first_row < t.dim.height);
    assert (t.num_rows >= 0);
    assert (t.num_rows < t.dim.height);
  ;;

  let set_dim _t _dim = assert false

  let%test_unit "invariant" =
    invariant (create { height = 10; width = 5; })

  let clear_all t =
    t.first_row <- 0;
    t.num_rows <- 0;
  ;;

  let reset t =
    clear_all t;
    Attributes.clear t.current_attributes;
  ;;

  let assert_in_bounds t ~x ~y =
    assert (y >= 0);
    assert (x >= 0);
    assert (y < t.dim.height);
    assert (x < t.dim.width);
  ;;

  let translate_y t ~y =
    (t.first_row + y) % t.dim.height

  let clear_row t ~y =
    let y = translate_y t ~y in
    for x = 0 to t.dim.width - 1 do
      Cell.clear t.data.(y).(x);
    done;
  ;;

  let grow_rows t ~max_y =
    assert (max_y < t.dim.height);
    for y = t.num_rows to max_y do
      clear_row t ~y
    done;
    t.num_rows <- max_y + 1;
  ;;

  let get t { x; y; } =
    assert_in_bounds t ~x ~y;
    if y >= t.num_rows
    then grow_rows t ~max_y:y;
    assert (y < t.num_rows);
    t.data.(translate_y t ~y).(x);
  ;;

  let fold_internal t ~init ~f =
    let acc = ref init in
    for y = 0 to t.num_rows - 1 do
      for x = 0 to t.dim.width - 1 do
        acc := f !acc ~x ~y t.data.(translate_y t ~y).(x)
      done
    done;
    !acc
  ;;

  let fold t ~init ~f =
    let acc = ref init in
    for y = 0 to t.dim.height - 1 do
      for x = 0 to t.dim.width - 1 do
        acc := f !acc ~x ~y (get t { x; y })
      done
    done;
    !acc
  ;;

  (* CR datkin: Check/test the scrolling logic for scrolling up (w/ scroll
   * back). *)
  let scroll t n =
    if abs n >= t.dim.height
    then clear_all t
    else begin
      (* If scrolling down, we move the pointer back, and clear earlier rows.
       * Then, we increase the number of rows.
       *
       * If scrolling up, we just move the pointer down, and decrease the number
       * of rows. We don't have to clear anything b/c we always clear when we
       * allocate new rows below. *)
      for y = n to -1 do
        clear_row t ~y
      done;
      t.first_row <- (t.first_row + n) % t.dim.height;
      t.num_rows <- max 0 (min t.dim.height (t.num_rows - n));
    end
  ;;

  let find_all t target =
    fold_internal t ~init:[] ~f:(fun acc ~x ~y elt ->
      if Char.(=) target (Cell.code elt)
      then { x; y; } :: acc
      else acc)
    |> List.rev

  let%test_unit _ =
    let dim = { width = 10; height = 5; } in
    let t = create dim in
    (*
    [%test_result: int]
      ~expect:(dim.width * dim.height)
      (fold t ~init:0 ~f:(fun size ~x:_ ~y:_ _ -> size + 1));
      *)
    Cell.set_code (get t origin) 'x';
    [%test_result: coord list] ~expect:[origin] (find_all t 'x');
    scroll t (-1);
    [%test_result: coord list] ~expect:[{x = 0; y = 1}] (find_all t 'x');
    invariant t;
    scroll t 1;
    [%test_result: coord list] ~expect:[{x = 0; y = 0}] (find_all t 'x');
    invariant t;
    Cell.set_code (get t { x = 2; y = 3; }) 'y';
    [%test_result: coord list] ~expect:[{x = 2; y = 3}] (find_all t 'y');
    invariant t;
    scroll t 2;
    [%test_result: coord list] ~expect:[{x = 2; y = 1}] (find_all t 'y');
    invariant t;
  ;;
end

type t = {
  (* Each row is an array. Last element list is always the top row. The list
   * will only have elements for the top N populated rows. So if the bottom of
   * the screen is empty, there will be no rows. *)
  mutable grid : Grid.t;
  mutable cursor : coord;
  (* CR-soon datkin: [scroll_region] doesn't do anything at the moment. *)
  mutable scroll_region : (int * int);
  mutable keypad : [ `Application | `Numeric ];
  mutable cursor_keys : [ `Application | `Normal ];
  parse : (char -> Control_functions.parse_result);
}

let create dim spec = {
  grid = Grid.create dim;
  cursor = origin;
  scroll_region = (0, dim.height);
  keypad = `Numeric;
  cursor_keys = `Application;
  parse = Control_functions.parser spec |> unstage;
}

let dim t =
  Grid.dim t.grid

(* Set all locations to a parcitular character, useful for testing *)
let paint t chr =
  Grid.fold t.grid ~init:() ~f:(fun () ~x:_ ~y:_ cell ->
    Cell.set_code cell chr)

let invariant t =
  assert (t.cursor.x <= (dim t).width && t.cursor.y <= (dim t).height);
;;

let%test_unit "invariant on create" =
  invariant
    (create
      { width = 10; height = 10; }
      Control_functions.Parser.default)

let set_dimensions t dim =
  Grid.set_dim t.grid dim;
  (* CR-someday datkin: If someone resizes the window, this clears the scroll
   * region, which is almost certainly wrong. *)
  t.scroll_region <- (0, dim.height);
  (* CR datkin: Update cursor position. *)
;;

let get t coord = Grid.get t.grid coord

let incr { x; y; } { width; height; } =
  let next = (y * width) + x + 1 in
  let y = (next / width) % height in
  let x = next % width in
  { x; y; }

let decr { x; y; } { width; height; } =
  let prev = (y * width) + x - 1 in
  let y = (prev / width) % height in
  let x = prev % width in
  { x; y; }

let%test_unit "incr coord" =
  [%test_result: coord]
    ~expect:origin
    (incr { x = 6; y = 10; } { width = 7; height = 11; })
;;

(* Write char to the current cursor and move the cursor. *)
let putc t chr =
  match chr with
  | '\n' ->
    let y = t.cursor.y + 1 in
    if y = (dim t).height
    then Grid.scroll t.grid 1
    else t.cursor <- { t.cursor with y }
  | '\r' ->
    t.cursor <- { t.cursor with x = 0 }
  | '\b' ->
    let cursor' = decr t.cursor (dim t) in
    let cell = Grid.get t.grid cursor' in
    Cell.clear_code cell;
    t.cursor <- cursor';
    (*
  | '\t' ->
      *)
  | _ ->
    let cell = Grid.get t.grid t.cursor in
    Cell.set_code cell chr;
    let cursor' = incr t.cursor (dim t) in
    if cursor' = origin
    then (Grid.scroll t.grid 1; t.cursor <- { t.cursor with x = 0; })
    else t.cursor <- cursor'
    (*
  (* This means we wrapped, which we shouldn't have. *)
  assert (t.cursor <> origin || (dim t).height = 1);
  *)
;;

let clear t =
  Grid.clear_all t.grid;
  t.cursor <- origin;
;;

let erase_in_display t which =
  let last = { x = (dim t).width - 1; y = (dim t).height - 1 } in
  let start, stop =
    match which with
    | `From_start -> (origin, t.cursor)
    | `To_end -> (t.cursor, last)
    | `All -> (origin, last)
    in
    let rec loop coord =
      (* We're always guaranteed to delete at least the cursor *)
      Cell.clear (Grid.get t.grid coord);
      let next = incr coord (dim t) in
      if compare coord stop >= 0 (* coord > stop *)
      || compare coord next > 0 (* coord > next, b/c [incr] wraps *)
      then ()
      else loop next
  in
  loop start

let bound ~min x ~max =
  Int.max min (Int.min x max)

let%expect_test _ =
  [%test_result: int] (bound ~min:0 ~max:5 (-1)) ~expect:0;
  [%test_result: int] (bound ~min:0 ~max:5   6 ) ~expect:5;
  [%test_result: int] (bound ~min:0 ~max:5   3 ) ~expect:3;
;;

let handle t parse_result =
  match parse_result with
  | `literal chr -> putc t chr; None
  | `pending -> None
  | `junk "\027[m" ->
    (* I believe this is a sgr for xterm where none of the settings have been
     * explicitly passed. The only source I've found that suggests that the
     * "default" value is 0 is here:
     * http://bjh21.me.uk/all-escapes/all-escapes.txt
     * See "Sequence: CSI Ps ... m" *)
    None
  | `junk _ -> None
  | `func (f, _data) ->
    match (f : Control_functions.t) with
    | Ack -> None
    | Bell -> None
    | Insert_blank _ -> None
    | Cursor_rel (dir, n) ->
      let cursor =
        match dir with
        | Down  ->
          let y = bound ~min:0 (t.cursor.y+n) ~max:((dim t).height - 1) in
          let scroll = (t.cursor.y+n) - y in
          Grid.scroll t.grid scroll;
          { t.cursor with y }
        | Right ->
          (* CR datkin: Line wrap? *)
          { t.cursor with
          x = bound ~min:0 (t.cursor.x+n) ~max:((dim t).width - 1) }
      in
      t.cursor <- cursor;
      None
    | Start_of_line_rel (`Down, n) ->
      Grid.scroll t.grid n; None
    | Cursor_abs { x=col; y=row; } ->
      t.cursor <- { x=col-1; y=row-1 }; None
    | Erase_line_including_cursor which ->
      let (min_x, max_x) =
        match which with
        | `Left -> 0, t.cursor.x
        | `Right -> t.cursor.x, (dim t).width - 1
        | `All -> 0, (dim t).width - 1
      in
      for x = min_x to max_x; do
        Cell.clear (Grid.get t.grid { t.cursor with x })
      done;
      None
    | Erase_display_including_cursor which ->
      erase_in_display t which;
      None
    | Set_scrolling_region { top; bottom } ->
      (* http://www.vt100.net/docs/vt510-rm/DECSTBM.html *)
      let top = Option.value top ~default:0 in
      let bottom = Option.value bottom ~default:(dim t).height in
      t.cursor <- { x = top; y = 0; };
      t.scroll_region <- (top, bottom);
      None
    | Dec_mode (set_or_clear, options) ->
      List.iter options ~f:(function
        | Application_cursor_keys ->
          t.cursor_keys <- (match set_or_clear with | `set -> `Application | `clear -> `Normal)
        | Application_keypad ->
          t.keypad <- (match set_or_clear with | `set -> `Application | `clear -> `Numeric)
        | _ ->
            ());
      None
    | Designate_char_set { g = _; character_set = _ }
    | Send_device_attribute `primary
      -> None
    | Send_device_attribute `secondary ->
      Some "\027[>1;95;0c"
    | Other (["VPA"], [Some row]) ->
      t.cursor <- { t.cursor with y = row-1; }; None
    | Other (["CUP"], []) -> t.cursor <- origin; None
    | Other (["CUP"], [Some row; Some col]) -> t.cursor <- { x=col-1; y=row-1 }; None
    | Other (["DECSET"], [Some 1049]) ->
      clear t; None
    | Other (["ED"], [Some 2]) -> Grid.clear_all t.grid; None
    | Other ([ "smcup"; ], []) ->
      (* CR datkin: Actually implement the two buffer modes. *)
      (* This is "start cursor addressing mode". In xterm it means "switch to
       * the alternate buffer". I.e., leave scroll back mode. *)
      clear t; None
    | Other ([ "smkx"; ], [])
      (* smkx = "start application keypad mode"
       * for emulating within a terminal emulator, I think we need to send
       * this on to the emulator. It detemrines how keys on the numpad are
       * interpreted?
       * https://ttssh2.osdn.jp/manual/en/usage/tips/appkeypad.html *)
    | Other (["csr"], [_; _]) ->
      (* Set Scrolling Region/Set Margins/"DECSTBM" *)
      None
    | Other (["ccvis"], []) ->
      (* Set cursor "very" visible (12 = show, 25 = blink) *)
      None
    | Other (["cnorm"], []) ->
      (* Opposite of ccvis; hide cursor, stop blinking *)
      None
    | Other (["rmso"], []) ->
      (* Exit "standout" mode *)
      None
    | Other _ ->
      None
;;

let from_user t str =
  let rec loop chars =
    match chars with
    | '\027' :: '[' :: ('A' | 'B' | 'C' | 'D' as x) :: chars ->
      let y =
        match t.cursor_keys with
        | `Application -> 'O'
        | `Normal      -> '['
      in
      '\027' :: y :: x :: (loop chars)
    | chr :: chars -> chr :: (loop chars)
    | [] -> []
  in
  loop (String.to_list str) |> String.of_char_list
;;

let update t chr =
  let parse_result = t.parse chr in
  printf !"%{sexp:Control_functions.parse_result}\n%!" parse_result;
  let to_send = handle t parse_result in
  (parse_result, to_send)
;;

  (* 1b 5b 37 35 32 32 3b 31 48 7e 
   *
   *   [  7  5  2  2  ;  1  H ~
   *
   * *)

let cursor t = t.cursor

let height t = (Grid.dim t.grid).height
let width t = (Grid.dim t.grid).width

let render t =
  (* Each line will be 1 + 5*width + 1 (for '|' + 5 * '    |' + '\n') *)
  let buf = String.create ((height t) * (1 + 3 * (width t) + 1)) in
  let idx = ref 0 in
  let output_char chr = String.set buf !idx chr; Core_kernel.Std.incr idx in
  let newline () = output_char '\n' in
  let output_string str = String.iter str ~f:output_char in
  for y = 0 to (dim t).height - 1 do
    let prev_was_cursor = ref false in
    let output_bar coord =
      if !prev_was_cursor
      then (output_char ']'; prev_was_cursor := false)
      else if coord = (Some t.cursor)
      then (output_char '['; prev_was_cursor := true)
      else output_char '|'
    in
    for x = 0 to (dim t).width - 1 do
      let coord = { x; y; } in
      output_bar (Some coord);
      let chr = Grid.get t.grid coord |> Cell.code in
      let chr = if chr = null_byte then ' ' else chr in
      (* output_string (sprintf "%02x" (Char.to_int chr) *)
      output_string (sprintf "% 2s" (Char.escaped chr)
      );
    done;
    output_bar None;
    newline ();
  done;
  buf
;;

let%expect_test _ =
  let t = create { width = 5; height = 3; } Control_functions.Parser.default in
  printf !"%s" (render t);
  [%expect {|
    [  ]  |  |  |  |
    |  |  |  |  |  |
    |  |  |  |  |  | |}];
  putc t 'A';
  printf !"%s" (render t);
  [%expect {|
    | A[  ]  |  |  |
    |  |  |  |  |  |
    |  |  |  |  |  | |}];
  putc t '\n';
  putc t '\n';
  printf !"%s" (render t);
  [%expect {|
    | A|  |  |  |  |
    |  |  |  |  |  |
    |  [  ]  |  |  | |}];
  t.cursor <- { x = 4; y = 1; };
  printf !"%s" (render t);
  [%expect {|
    | A|  |  |  |  |
    |  |  |  |  [  ]
    |  |  |  |  |  | |}];
  putc t 'A';
  printf !"%s" (render t);
  [%expect {|
    | A|  |  |  |  |
    |  |  |  |  | A|
    [  ]  |  |  |  | |}];
;;

let%expect_test "Erase Display (ED)" =
  let t = create { width = 5; height = 3; } Control_functions.Parser.default in
  paint t 'X';
  printf !"%s" (render t);
  [%expect {|
    [ X] X| X| X| X|
    | X| X| X| X| X|
    | X| X| X| X| X| |}];
  t.cursor <- { y = 1; x = 2; };
  erase_in_display t `From_start;
  printf !"%s" (render t);
  [%expect {|
    |  |  |  |  |  |
    |  |  [  ] X| X|
    | X| X| X| X| X| |}];
  paint t 'X';
  printf !"%s" (render t);
  [%expect {|
    | X| X| X| X| X|
    | X| X[ X] X| X|
    | X| X| X| X| X| |}];
  t.cursor <- { y = 1; x = 2; };
  erase_in_display t `To_end;
  printf !"%s" (render t);
  [%expect {|
    | X| X| X| X| X|
    | X| X[  ]  |  |
    |  |  |  |  |  | |}];
  paint t 'X';
  printf !"%s" (render t);
  [%expect {|
    | X| X| X| X| X|
    | X| X[ X] X| X|
    | X| X| X| X| X| |}];
  t.cursor <- { y = 1; x = 2; };
  erase_in_display t `All;
  printf !"%s" (render t);
  [%expect {|
    |  |  |  |  |  |
    |  |  [  ]  |  |
    |  |  |  |  |  | |}];
;;

(* CR datkin: In theory there are two screen modes, "regular"(?) and "alternate
 * buffer". We want scrollback in regular, but presumably not in alternate. *)
let%expect_test "Scrolling" =
  let t = create { width = 3; height = 3; } Control_functions.Parser.default in
  paint t 'X';
  let handle (cf : Control_functions.parse_result) = ignore (handle t cf : string option) in
  handle (`literal 'A');
  printf !"%s" (render t);
  [%expect {|
    | A[ X] X|
    | X| X| X|
    | X| X| X| |}];
  handle (`func (Cursor_abs { x = 1; y = 3 }, ""));
  handle (`literal '\n');
  handle (`literal 'Y');
  handle (`literal '\n');
  handle (`literal 'Y');
  printf !"%s" (render t);
  [%expect {|
    | X| X| X|
    | Y|  |  |
    |  | Y[  ] |}];
  handle (`func (Cursor_abs { x = 1; y = 1 }, ""));
  handle (`func (Cursor_rel (Down, (-1)), ""));
  printf !"%{sexp:coord}\n%!" t.cursor;
  [%expect "((y 0) (x 0))"];
  handle (`literal 'X');
  printf !"%s" (render t);
  [%expect {|
    | X[  ]  |
    | X| X| X|
    | Y|  |  | |}];
  handle (`func (Cursor_rel (Down, 3), ""));
  printf !"%s" (render t);
  [%expect {|
    | X| X| X|
    | Y|  |  |
    |  [  ]  | |}];
;;

let html_pre = {|
<html>
  <head>
    <style>
      .terminal {
        background-color: black;
        color: white;
        display: inline-block;
        border-style: solid;
        border-color: black;
        padding: 0.2em;
        letter-spacing: 0.1em;
        font-size: 10pt;
        font-family: "Monaco", "Courier New", sans-serif;
      }
      .cursor {
        background-color: white;
        color: black;
      }
    </style>

    <script type="text/javascript">
      setTimeout(function(){
           window.location.reload(1);
      }, 200);
    </script>

  </head>
  <body>
    <div class="terminal">
|}

let html_post = {|
    </div>
  </body>
</html>
|}

let render_html t =
  let buf = ref "" in
  let output str = buf := !buf ^ str in
  let newline () = output "<br/>\n" in
  for y = 0 to (dim t).height - 1 do
    for x = 0 to (dim t).width - 1 do
      let coord = { x; y; } in
      let chr = Grid.get t.grid coord |> Cell.code in
      let chr =
        match chr with
        | '\000'
        | ' ' -> "&nbsp;"
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '&' -> "&amp;"
        | _ -> Char.escaped chr
      in
      output chr
    done;
    newline ();
  done;
  html_pre ^ !buf ^ html_post
;;
