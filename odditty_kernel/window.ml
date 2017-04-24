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
} [@@deriving sexp, bin_io, compare]

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

  val _copy : src:t -> dst:t -> unit
end = struct
  type t = unit [@@deriving sexp]

  let plain = ()

  let clear () = ()

  let _copy ~src:() ~dst:() = ()
end

module Cell : sig
  type t [@@deriving sexp]

  type t_as_char = t [@@deriving sexp_of]

  val init : unit -> t

  val code : t -> Char.t
  val attributes : t -> Attributes.t

  val clear : t -> unit
  val set_code : t -> Char.t -> unit
end = struct
  type t = {
    mutable code : Char.t;
    attributes : Attributes.t;
  } [@@deriving sexp, fields]

  type t_as_char = t

  let sexp_of_t_as_char t =
    Sexp.Atom (Char.to_string (if t.code = '\000' || t.code = ' ' then '_' else t.code))

  let init () = { code = null_byte; attributes = Attributes.plain; }

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

  val create : dim -> scrollback:int -> t

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

  (* Scroll *down* n lines (i.e. negative numbers scroll up).
   * Only lines within the given scroll region are scrolled (i.e. lines outside
   * the scroll region are fixed.).
   * *)
  val scroll : t -> int -> scroll_region:(int * int) -> unit
end = struct
  type t = {
    mutable dim : dim;
    mutable data : Cell.t array array; (* outer array is row *)
    mutable first_row : int;
    mutable num_rows : int;
    current_attributes : Attributes.t;
  } [@@deriving sexp_of, fields]

  let create dim ~scrollback =
    assert (scrollback >= 0);
    let height = dim.height + scrollback in
    let t = {
      dim;
      (* This is a bit dumb/confusing. We're doing our array row-major, not column
       * major. But here [dimx] corresponds to the major dimension. *)
      data = Array.make_matrix ~dimx:height ~dimy:dim.width (Cell.init ());
      first_row = 0;
      num_rows = 0;
      current_attributes = Attributes.plain;
    }
    in
    for y = 0 to height - 1 do
      for x = 0 to t.dim.width - 1 do
        t.data.(y).(x) <- Cell.init ();
      done
    done;
    t

  let invariant t =
    assert (Array.length t.data >= t.dim.height); (* may be > b/c of scrollback *)
    for y = 0 to Array.length t.data - 1; do
      assert (Array.length t.data.(y) = t.dim.width);
    done;
    assert (t.first_row >= 0);
    assert (t.first_row < Array.length t.data);
    assert (t.num_rows >= 0);
    assert (t.num_rows < t.dim.height);
  ;;

  let set_dim _t _dim = assert false

  let%test_unit "invariant" =
    invariant (create { height = 10; width = 5; } ~scrollback:0)

  let clear_all t =
    t.first_row <- 0;
    t.num_rows <- 0;
  ;;

  let _reset t =
    clear_all t;
    Attributes.clear t.current_attributes;
  ;;

  let assert_in_bounds t ~x ~y =
    assert (y >= 0);
    assert (x >= 0);
    assert (y < t.dim.height);
    assert (x < t.dim.width);
  ;;

  (* Translate a y coordinate on the abstract grid to an actual array index. *)
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

  let scrollback t =
    Array.length t.data - t.dim.height

  (* CR datkin: Check/test the scrolling logic for scrolling up (w/ scroll
   * back). *)
  let scroll t n ~scroll_region:(top_margin, bottom_margin) =
    assert (top_margin >= 0);
    assert (bottom_margin > top_margin);
    assert (bottom_margin < t.dim.height);
    if top_margin = 0
    && bottom_margin = t.dim.height - 1
    then (
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
    ) else (
      (* For now this assumes that we're using the whole grid... *)
      assert (t.num_rows = t.dim.height);
      (* ...and no scrollback (I'm not sure if this one actually matters). *)
      assert (scrollback t = 0);
      if n = 0
      then ()
      else if n > 0
      then (
        (* Do everything in (abstract) grid coordinates, and then update
         * [t.first_row] at the very end. We don't update the num rows b/c we assume
         * we'e using the full screen both before and after (at least for now).
         * *)
        let num_moves = top_margin + (t.dim.height - bottom_margin) + n in
        let rec loop t ~top_margin ~bottom_margin ~n ~displaced_data ~displaced_y ~moves =
          let dst =
            let shift =
              if displaced_y >= top_margin && displaced_y <= bottom_margin
              then
                (* Rows from the top of the current scroll region are shifted to
                 * the bottom of the new scroll region (where they will be
                 * erased). *)
                (1 + bottom_margin - top_margin)
              else n
            in
            (displaced_y + shift) % t.dim.height
          in
          Core_kernel.Std.printf !"%d -> %d: %{sexp:Cell.t_as_char Array.t}\n%!"
            displaced_y dst displaced_data;
          let newly_displaced_data = t.data.(translate_y t ~y:dst) in
          t.data.(translate_y t ~y:dst) <- displaced_data;
          if dst = top_margin
          then moves (* We've made a full circle, and [newly_displaced_data] has already been dealt with. *)
          else loop t ~n ~top_margin ~bottom_margin ~displaced_data:newly_displaced_data ~displaced_y:dst ~moves:(moves + 1)
        in
        (* CR datkin: Depending on some details, you may get into a loop that
         * doesn't actually touch all the rows we need. Maybe if we just keep
         * touching row n+1 until we've done all the moves we need, it'll work?
         * *)
        let rec loop' ~moves =
          loop t ~n ~top_margin ~bottom_margin ~displaced_data:(t.data.(translate_y t ~y:top_margin)) ~displaced_y:top_margin ~moves:1;
        in
        t.first_row <- (t.first_row + n) % t.dim.height;
        for idx = 0 to n - 1; do
          clear_row t ~y:(bottom_margin - idx);
        done
      ) else if n < 0
      then (
        assert false;
      )
    )
  ;;

  let find_all t target =
    fold_internal t ~init:[] ~f:(fun acc ~x ~y elt ->
      if Char.(=) target (Cell.code elt)
      then { x; y; } :: acc
      else acc)
    |> List.rev

  let%test_unit _ =
    let dim = { width = 10; height = 5; } in
    let scroll_region = (0, 4) in
    let t = create dim ~scrollback:1 in
    (*
    [%test_result: int]
      ~expect:(dim.width * dim.height)
      (fold t ~init:0 ~f:(fun size ~x:_ ~y:_ _ -> size + 1));
      *)
    Cell.set_code (get t origin) 'x';
    [%test_result: coord list] ~expect:[origin] (find_all t 'x');
    scroll t (-1) ~scroll_region;
    [%test_result: coord list] ~expect:[{x = 0; y = 1}] (find_all t 'x');
    invariant t;
    scroll t 1 ~scroll_region;
    [%test_result: coord list] ~expect:[{x = 0; y = 0}] (find_all t 'x');
    invariant t;
    Cell.set_code (get t { x = 2; y = 3; }) 'y';
    [%test_result: coord list] ~expect:[{x = 2; y = 3}] (find_all t 'y');
    invariant t;
    scroll t 2 ~scroll_region;
    [%test_result: coord list] ~expect:[{x = 2; y = 1}] (find_all t 'y');
    invariant t;
  ;;
end

let%test_module _ = (module struct
  let init ?(scrollback=0) str =
    let lines =
      String.strip str
      |> String.split ~on:'\n'
      |> List.map ~f:(fun line ->
          match String.split line ~on:'|' with
          | [ _; data; _ ] -> data
          | _ -> assert false)
      |> List.map ~f:String.to_list
      |> List.map ~f:Array.of_list
    in
    let width =
      match lines with
      | [] -> assert false
      | row :: _ -> Array.length row
    in
    let lines = Array.of_list lines in
    assert (Array.for_all lines ~f:(fun row -> width = Array.length row));
    let dim = { width; height = Array.length lines; } in
    let t = Grid.create dim ~scrollback in
    for y = 0 to dim.height - 1; do
      for x = 0 to dim.width - 1; do
        let cell = Grid.get t { x; y; } in
        Cell.set_code cell lines.(y).(x);
      done
    done;
    t
  ;;

  let print t =
    let dim = Grid.dim t in
    List.init dim.height ~f:(fun y ->
      let line =
        List.init dim.width ~f:(fun x ->
          let cell = Grid.get t { x; y } in
          let code = Cell.code cell in
          if code = '\000'
          then ' '
          else code)
        |> String.of_char_list
      in
      sprintf "|%s|" line)
    |> String.concat ~sep:"\n"
    |> Core_kernel.Std.print_endline

  let diagonal ?scrollback () = init ?scrollback {|
      |x    |
      | x   |
      |  x  |
      |   x |
      |    x|
  |}

  let%expect_test _ =
    print (diagonal ());
    [%expect {|
      |x    |
      | x   |
      |  x  |
      |   x |
      |    x|
      |}];
  ;;

  let%expect_test _ =
    let open Expect_test_helpers_kernel in
    let t = diagonal ~scrollback:3 () in
    let scroll_region = (0, 4) in
    show_allocation (fun () ->
      Grid.scroll t 3 ~scroll_region);
    print t;
    show_allocation (fun () ->
      (* Note: scrollback doesn't work yet *)
      Grid.scroll t (-3) ~scroll_region);
    print t;
    show_allocation (fun () ->
      Grid.scroll t 5 ~scroll_region);
    print t;
    [%expect {|
      (allocated
        (minor_words 0)
        (major_words 0))
      |   x |
      |    x|
      |     |
      |     |
      |     |
      (allocated
        (minor_words 0)
        (major_words 0))
      |     |
      |     |
      |     |
      |   x |
      |    x|
      (allocated
        (minor_words 0)
        (major_words 0))
      |     |
      |     |
      |     |
      |     |
      |     |
      |}];
  ;;

  let%expect_test _ =
    let open Expect_test_helpers_kernel in
    let t = diagonal () in
    let scroll_region = (1, 3) in
    show_allocation (fun () ->
      Grid.scroll t 1 ~scroll_region);
    print t;
    [%expect {|
      (allocated
        (minor_words 0)
        (major_words 0))
      |x    |
      |  x  |
      |   x |
      |     |
      |    x|
      |}];
  ;;

  let%expect_test _ =
    let dump scroll scroll_region =
      let t = diagonal () in
      Grid.scroll t scroll ~scroll_region;
      print t;
    in
    dump 2 (0, 2);
    [%expect {|
      |  x  |
      |     |
      |     |
      |   x |
      |    x| |}];
    dump 2 (2, 4);
    [%expect {|
      |x    |
      | x   |
      |    x|
      |     |
      |     | |}];
    dump 2 (1, 3);
    [%expect {| |}];
  ;;
end)

(* CR-soon datkin: We need to save scrollback in normal mode. *)
type screen_mode =
  | Normal
  | Alternate of { normal : Grid.t; cursor : coord; }

type t = {
  mutable grid : Grid.t;
  mutable screen_mode : screen_mode;
  mutable cursor : coord;
  mutable scroll_region : (int * int); (* rows, zero indexed, inclusive *)
  mutable keypad : [ `Application | `Numeric ];
  mutable cursor_keys : [ `Application | `Normal ];
  mutable show_cursor : bool;
  parse : (char -> Control_functions.parse_result);
}

let create dim ~scrollback spec = {
  grid = Grid.create dim ~scrollback;
  screen_mode = Normal;
  cursor = origin;
  scroll_region = (0, dim.height - 1);
  keypad = `Numeric;
  cursor_keys = `Application;
  show_cursor = true;
  parse = Control_functions.parser spec |> unstage;
}

let dim t =
  Grid.dim t.grid

(* Set all locations to a parcitular character, useful for testing *)
let paint t chr =
  Grid.fold t.grid ~init:() ~f:(fun () ~x:_ ~y:_ cell ->
    Cell.set_code cell chr)

let invariant t =
  (* CR datkin: Shouldn't x/y be <, not <=? *)
  assert (t.cursor.x < (dim t).width && t.cursor.y < (dim t).height);
;;

let%test_unit "invariant on create" =
  invariant
    (create
      { width = 10; height = 10; }
      ~scrollback:0
      Control_functions.Parser.default)

let set_dimensions t dim =
  Grid.set_dim t.grid dim;
  (* CR-someday datkin: If someone resizes the window, this clears the scroll
   * region, which is almost certainly wrong. *)
  t.scroll_region <- (0, dim.height - 1);
  (* CR datkin: Update cursor position. *)
;;

let _get t coord = Grid.get t.grid coord

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

let get_margin t which =
  match which with
  | `top -> fst t.scroll_region
  | `bottom -> snd t.scroll_region
;;

(* Write char to the current cursor and move the cursor. *)
let putc t chr =
  match chr with
  | '\n' ->
    if t.cursor.y = get_margin t `bottom
    then (
      let scroll_region = t.scroll_region in
      Grid.scroll t.grid 1 ~scroll_region
    )
    else (
      let y = t.cursor.y + 1 in
      t.cursor <- { t.cursor with y }
    )
  | '\r' ->
    t.cursor <- { t.cursor with x = 0 }
  | '\b' ->
    let cursor' = decr t.cursor (dim t) in
    t.cursor <- cursor';
    (*
  | '\t' ->
      *)
  | _ ->
    let cell = Grid.get t.grid t.cursor in
    Cell.set_code cell chr;
    let cursor' = incr t.cursor (dim t) in
    if cursor' = origin
    then (
      let scroll_region = t.scroll_region in
      Grid.scroll t.grid 1 ~scroll_region;
      t.cursor <- { t.cursor with x = 0; }
    )
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

let shift_chars t n direction =
  (* See:
    * http://vt100.net/docs/vt510-rm/ICH.html
    * http://vt100.net/docs/vt220-rm/chapter4.html
    *)
  let width = (Grid.dim t.grid).width in
  match direction with
  | `Left ->
    for x = t.cursor.x to width - 1; do
      let dst = { t.cursor with x } in
      let value, attribute_change =
        if x + n < width
        then Grid.get t.grid { t.cursor with x = x + n } |> Cell.code, `same_attributes
        else '\000', `clear_attributes
      in
      let dst_cell = Grid.get t.grid dst in
      Cell.set_code dst_cell value;
      begin
        match attribute_change with
        | `same_attributes -> ()
        | `clear_attributes -> Cell.attributes dst_cell |> Attributes.clear
      end;
    done;
  | `Right ->
    for x = width - 1 downto t.cursor.x; do
      let dst = { t.cursor with x } in
      let value =
        if (x - t.cursor.x) < n
        then '\000'
        else Grid.get t.grid { t.cursor with x = x - n } |> Cell.code
      in
      Cell.set_code (Grid.get t.grid dst) value
    done;
;;

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
    | Cursor_rel (dir, n) ->
      let cursor =
        match dir with
        | Down  ->
          let y = bound ~min:0 (t.cursor.y+n) ~max:((dim t).height - 1) in
          let scroll = (t.cursor.y+n) - y in
          let scroll_region = t.scroll_region in
          Grid.scroll t.grid scroll ~scroll_region;
          { t.cursor with y }
        | Right ->
          (* CR datkin: Line wrap? *)
          { t.cursor with
            x = bound ~min:0 (t.cursor.x+n) ~max:((dim t).width - 1) }
      in
      t.cursor <- cursor;
      None
    | Start_of_line_rel (`Down, n) ->
      let scroll_region = t.scroll_region in
      Grid.scroll t.grid n ~scroll_region;
      None
    | Cursor_abs { x=col; y=row; } ->
      (* This allows you to move the cursor to any position, even outside the
       * scrolling region. *)
      t.cursor <- { x=col-1; y=row-1 }; None
    | Insert_blank n -> (* ICH *)
      shift_chars t n `Right; None
    | Delete_chars n -> (* DCH *)
      shift_chars t n `Left; None
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
      (* http://www.vt100.net/docs/vt510-rm/DECSTBM.html
       * http://vt100.net/docs/vt220-rm/chapter4.html#S4.13
       *)
      let top = Option.value top ~default:1 in
      let bottom = Option.value bottom ~default:(dim t).height in
      assert (top < bottom);
      (* CR-someday datkin: The code formerly did this, but I don't this it's
       * part of the expected behavior. I'm not sure why I added it.
      t.cursor <- { x = top; y = 0; };
      *)
      t.scroll_region <- (top - 1, bottom - 1);
      None
    | Dec_mode (set_or_clear, options) ->
      List.iter options ~f:(function
        | Application_cursor_keys ->
          t.cursor_keys <- (match set_or_clear with | `set -> `Application | `clear -> `Normal)
        | Application_keypad ->
          t.keypad <- (match set_or_clear with | `set -> `Application | `clear -> `Numeric)
        | Show_cursor ->
          t.show_cursor <- (match set_or_clear with | `set -> true | `clear -> false)
        | Save_cursor_as_in_DECSC_and_use_alternate_screen_buffer ->
          begin
            match set_or_clear, t.screen_mode with
            | `set, Normal ->
              t.screen_mode <- Alternate { normal = t.grid; cursor = t.cursor; };
              t.grid <- Grid.create (Grid.dim t.grid) ~scrollback:0;
            | `clear, Alternate { normal; cursor } ->
              t.grid <- normal;
              t.cursor <- cursor;
              t.screen_mode <- Normal;
            | `clear, Normal
            | `set, Alternate _
            -> ()
          end
          (* CR-soon datkin: Implement the alternate buffer. *)
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
    (* In order: Up, Down, Right, Left *)
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

(* CR datkin: Issues in bash:
  *  - moving back on the line to edit it (w/ either left arrow or ctrl-a) is
  *    busted
  *)
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

let render_string t =
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
  let t = create { width = 5; height = 3; } ~scrollback:0 Control_functions.Parser.default in
  printf !"%s" (render_string t);
  [%expect {|
    [  ]  |  |  |  |
    |  |  |  |  |  |
    |  |  |  |  |  | |}];
  putc t 'A';
  printf !"%s" (render_string t);
  [%expect {|
    | A[  ]  |  |  |
    |  |  |  |  |  |
    |  |  |  |  |  | |}];
  putc t '\n';
  putc t '\n';
  printf !"%s" (render_string t);
  [%expect {|
    | A|  |  |  |  |
    |  |  |  |  |  |
    |  [  ]  |  |  | |}];
  t.cursor <- { x = 4; y = 1; };
  printf !"%s" (render_string t);
  [%expect {|
    | A|  |  |  |  |
    |  |  |  |  [  ]
    |  |  |  |  |  | |}];
  putc t 'A';
  printf !"%s" (render_string t);
  [%expect {|
    | A|  |  |  |  |
    |  |  |  |  | A|
    [  ]  |  |  |  | |}];
  (* '\b' backs things up one, but doesn't rubout. *)
  putc t '\b';
  printf !"%s" (render_string t);
  [%expect {|
    | A|  |  |  |  |
    |  |  |  |  [ A]
    |  |  |  |  |  | |}];
  (* CR datkin: How does the \004 from the user get translated into the delete,
   * again? *)
  List.iter (String.to_list "\bB\b") ~f:(fun char ->
    ignore (update t char));
  t.cursor <- { t.cursor with x = 0 };
  printf !"%s" (render_string t);
  [%expect {|
    (literal "\b")
    (literal B)
    (literal "\b")
    | A|  |  |  |  |
    [  ]  |  | B| A|
    |  |  |  |  |  | |}];
  ignore (handle t (`func (Control_functions.Delete_chars 1, ())));
  printf !"%s" (render_string t);
  [%expect {|
    | A|  |  |  |  |
    [  ]  | B| A|  |
    |  |  |  |  |  | |}];
  ignore (handle t (`func (Control_functions.Delete_chars 2, ())));
  printf !"%s" (render_string t);
  [%expect {|
    | A|  |  |  |  |
    [ B] A|  |  |  |
    |  |  |  |  |  | |}];
  ignore (handle t (`func (Control_functions.Insert_blank 2, ())));
  printf !"%s" (render_string t);
  [%expect {|
    | A|  |  |  |  |
    [  ]  | B| A|  |
    |  |  |  |  |  | |}];
;;

let%expect_test "Erase Display (ED)" =
  let t = create { width = 5; height = 3; } ~scrollback:1 Control_functions.Parser.default in
  paint t 'X';
  printf !"%s" (render_string t);
  [%expect {|
    [ X] X| X| X| X|
    | X| X| X| X| X|
    | X| X| X| X| X| |}];
  t.cursor <- { y = 1; x = 2; };
  erase_in_display t `From_start;
  printf !"%s" (render_string t);
  [%expect {|
    |  |  |  |  |  |
    |  |  [  ] X| X|
    | X| X| X| X| X| |}];
  paint t 'X';
  printf !"%s" (render_string t);
  [%expect {|
    | X| X| X| X| X|
    | X| X[ X] X| X|
    | X| X| X| X| X| |}];
  t.cursor <- { y = 1; x = 2; };
  erase_in_display t `To_end;
  printf !"%s" (render_string t);
  [%expect {|
    | X| X| X| X| X|
    | X| X[  ]  |  |
    |  |  |  |  |  | |}];
  paint t 'X';
  printf !"%s" (render_string t);
  [%expect {|
    | X| X| X| X| X|
    | X| X[ X] X| X|
    | X| X| X| X| X| |}];
  t.cursor <- { y = 1; x = 2; };
  erase_in_display t `All;
  printf !"%s" (render_string t);
  [%expect {|
    |  |  |  |  |  |
    |  |  [  ]  |  |
    |  |  |  |  |  | |}];
;;

(* CR datkin: In theory there are two screen modes, "regular"(?) and "alternate
 * buffer". We want scrollback in regular, but presumably not in alternate. *)
let%expect_test "Scrolling" =
  let t = create { width = 3; height = 3; } ~scrollback:1 Control_functions.Parser.default in
  paint t 'X';
  let handle (cf : Control_functions.parse_result) = ignore (handle t cf : string option) in
  handle (`literal 'A');
  printf !"%s" (render_string t);
  [%expect {|
    | A[ X] X|
    | X| X| X|
    | X| X| X| |}];
  handle (`func (Cursor_abs { x = 1; y = 3 }, ""));
  handle (`literal '\n');
  handle (`literal 'Y');
  handle (`literal '\n');
  handle (`literal 'Y');
  printf !"%s" (render_string t);
  [%expect {|
    | X| X| X|
    | Y|  |  |
    |  | Y[  ] |}];
  handle (`func (Cursor_abs { x = 1; y = 1 }, ""));
  handle (`func (Cursor_rel (Down, (-1)), ""));
  printf !"%{sexp:coord}\n%!" t.cursor;
  [%expect "((y 0) (x 0))"];
  handle (`literal 'X');
  printf !"%s" (render_string t);
  [%expect {|
    | X[  ]  |
    | X| X| X|
    | Y|  |  | |}];
  handle (`func (Cursor_rel (Down, 3), ""));
  printf !"%s" (render_string t);
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


module Rendered = struct
  type t = {
    cursor : coord option;
    chars : char list list;
  } [@@deriving bin_io]
end

let render t =
  let chars =
    let rows = ref [] in
    for y = 0 to (dim t).height - 1 do
      let row = ref [] in
      for x = 0 to (dim t).width - 1 do
        let coord = { x; y; } in
        let chr = Grid.get t.grid coord |> Cell.code in
        row := chr :: !row
      done;
      rows := (List.rev !row) :: !rows
    done;
    List.rev !rows
  in
  let cursor = Option.some_if t.show_cursor (cursor t) in
  { Rendered. chars; cursor; }
;;

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
