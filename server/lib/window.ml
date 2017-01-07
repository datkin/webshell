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

type coord = {
  x : int;
  y : int;
} [@@deriving sexp, compare]

let origin = { x = 0; y = 0; }

let null_byte = '\000'

module Grid : sig
  type t [@@deriving sexp_of]

  val create : dim -> t

  val dim : t -> dim
  val set_dim : t -> dim -> unit

  val get : t -> coord -> Char.t
  val set : t -> coord -> Char.t -> unit

  (*
  val clear : t -> unit
  *)

  val scroll : t -> int -> unit
end = struct
  type t = {
    mutable dim : dim;
    mutable data : Char.t array array; (* row major *)
    mutable first_row : int;
    mutable num_rows : int;
  } [@@deriving sexp_of, fields]

  let create dim = {
    dim;
    (* This is a bit dumb/confusing. We're doing our array row-major, not column
     * major. But here [dimx] corresponds to the major dimension. *)
    data = Array.make_matrix ~dimx:dim.height ~dimy:dim.width null_byte;
    first_row = 0;
    num_rows = 0;
  }

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

  let clear t =
    t.first_row <- 0;
    t.num_rows <- 0;
  ;;

  let assert_in_bounds t ~x ~y =
    assert (y >= 0);
    assert (x >= 0);
    assert (y < t.dim.height);
    assert (x < t.dim.width);
  ;;

  let translate_y t ~y =
    (t.first_row + y) % t.dim.height

  let get t { x; y; } =
    assert_in_bounds t ~x ~y;
    if y >= t.num_rows
    then null_byte
    else t.data.(translate_y t ~y).(x)

  let clear_row t ~y =
    let y = translate_y t ~y in
    for x = 0 to t.dim.width - 1 do
      t.data.(y).(x) <- null_byte;
    done;
  ;;

  let grow_rows t ~max_y =
    assert (max_y < t.dim.height);
    for y = t.num_rows to max_y do
      clear_row t ~y
    done;
    t.num_rows <- max_y + 1;
  ;;

  let set t { x; y; } chr =
    assert_in_bounds t ~x ~y;
    if y >= t.num_rows
    then grow_rows t ~max_y:y;
    assert (y < t.num_rows);
    t.data.(translate_y t ~y).(x) <- chr

  let scroll t n =
    if abs n >= t.dim.height
    then clear t
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

  let fold t ~init ~f =
    let acc = ref init in
    for y = 0 to t.num_rows - 1 do
      for x = 0 to t.dim.width - 1 do
        acc := f !acc ~x ~y t.data.(translate_y t ~y).(x)
      done
    done;
    !acc

  let find_all t target =
    fold t ~init:[] ~f:(fun acc ~x ~y elt ->
      if Char.(=) target elt
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
    set t origin 'x';
    [%test_result: coord list] ~expect:[origin] (find_all t 'x');
    scroll t (-1);
    [%test_result: coord list] ~expect:[{x = 0; y = 1}] (find_all t 'x');
    invariant t;
    scroll t 1;
    [%test_result: coord list] ~expect:[{x = 0; y = 0}] (find_all t 'x');
    invariant t;
    set t { x = 2; y = 3; } 'y';
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
  parse : (char -> [`literal of char | `func of Control_functions.t | `junk of string | `pending]);
}

let create dim spec = {
  grid = Grid.create dim;
  cursor = origin;
  parse = Control_functions.parser spec |> unstage;
}

let dim t = Grid.dim t.grid

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
  (* CR datkin: Update cursor position. *)
;;

let get t coord = Grid.get t.grid coord

let incr { x; y; } { width; height; } =
  let next = (y * width) + x + 1 in
  let y = (next / width) % height in
  let x = next % width in
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
    (*
  | '\t' ->
      *)
  | _ ->
    Grid.set t.grid t.cursor chr;
    let cursor' = incr t.cursor (dim t) in
    if cursor' = origin
    then (Grid.scroll t.grid 1; t.cursor <- { t.cursor with x = 0; })
    else t.cursor <- cursor'
    (*
  (* This means we wrapped, which we shouldn't have. *)
  assert (t.cursor <> origin || (dim t).height = 1);
  *)
;;

let update t str =
  String.iter str ~f:(fun chr ->
    match t.parse chr with
    | `literal chr -> putc t chr
    | `pending -> ()
    | `junk str ->
      Core.Std.eprintf "Bad input: [%s]\n%!"
        (String.to_list str |> List.map ~f:(fun x -> Char.to_int x |> sprintf "%02x") |> String.concat ~sep:" ")
    | `func f ->
      match (f : Control_functions.t) with
      | Ack -> ()
      | Bell -> ()
      | Insert_blank _
      | Cursor_rel (_, _)
      | Start_of_line_rel (_, _)
      | Cursor_abs { x = _; y = _; }
      | Other _ ->
        Core.Std.printf !"%{sexp:Control_functions.t}\n%!" f)

  (* 1b 5b 37 35 32 32 3b 31 48 7e 
   *
   *   [  7  5  2  2  ;  1  H ~
   *
   * *)

let cursor t = t.cursor

let render t out =
  Out_channel.output_string out "-- start --\n";
  for y = 0 to (dim t).height - 1 do
    let prev_was_cursor = ref false in
    for x = 0 to (dim t).width - 1 do
      let coord = { x; y; } in
      begin
        if !prev_was_cursor
        then (Out_channel.output_char out ']'; prev_was_cursor := false)
        else if coord = t.cursor
        then (Out_channel.output_char out '['; prev_was_cursor := true)
        else Out_channel.output_char out '|'
      end;
      let chr = Grid.get t.grid coord in
      let chr = if chr = null_byte then ' ' else chr in
      (* Out_channel.output_string out (sprintf "%02x" (Char.to_int chr) *)
      Out_channel.output_string out (sprintf "% 4s" (Char.escaped chr)
      );
    done;
    Out_channel.newline out;
  done;
  Out_channel.output_string out "-- stop --\n";
  Out_channel.flush out;
;;
