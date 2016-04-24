open Core_kernel.Std

type dim = {
  width : int;
  height : int;
} [@@deriving sexp, compare]

type coord = {
  x : int;
  y : int;
} [@@deriving sexp, compare]

let origin = { x = 0; y = 0; }

let null_byte = '\000'

module Grid : sig
  type t [@@deriving sexp_of]

  val create : dim -> t

  val get : t -> coord -> Char.t
  val set : t -> coord -> Char.t -> unit

  val clear : t -> unit

  val scroll : t -> int -> unit
end = struct
  type t = {
    dim : dim;
    data : Char.t array array; (* row major *)
    mutable first_row : int;
    mutable num_rows : int;
  } [@@deriving sexp_of]

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
    (t.first_row + y) mod t.dim.height

  let get t { x; y; } =
    assert_in_bounds t ~x ~y;
    if y >= t.num_rows
    then null_byte
    else t.data.(translate_y t ~y).(x)

  let clear_row t ~y =
    let y = translate_y t ~y in
    for x = 0 to t.dim.width - 1 do
      t.data.(y).(x) <- null_byte;
    done

  let grow_rows t ~max_y =
    assert (max_y < t.dim.height);
    for y = t.num_rows to max_y do
      clear_row t ~y
    done;
    t.num_rows <- max_y;
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
      for y = n to 0 do
        clear_row t ~y
      done;
      t.first_row <- (t.first_row + n) mod t.dim.height;
      t.num_rows <- max 0 (min t.dim.height (t.num_rows - n));
    end
  ;;
end

type t = {
  (* Each row is an array. Last element list is always the top row. The list
   * will only have elements for the top N populated rows. So if the bottom of
   * the screen is empty, there will be no rows. *)
  mutable rows : Char.t Array.t List.t;
  mutable num_rows : int;
  mutable dim : dim;
  mutable cursor : coord;
}

let create dim = {
  rows = [];
  num_rows = 0;
  dim;
  cursor = origin;
}

let invariant t =
  assert (List.length t.rows = t.num_rows);
  assert (t.num_rows <= t.dim.height);
  assert (t.cursor.x <= t.dim.width && t.cursor.y <= t.dim.height);
;;

let%test_unit "invariant on create" =
  invariant (create { width = 10; height = 10; })

let set_dimensions t dim = t.dim <- dim

let select_row_exn t y =
  let rec loop y rows =
    match t.num_rows - y, rows with
    | 0, row :: _ -> row
    | _, _ :: rows -> loop (y-1) rows
    | _, [] -> assert false
  in
  assert (y >= 0);
  (* CR datkin: bounds check on height. *)
  assert (y < t.num_rows);
  loop y t.rows

(* Just set the character at the given coordinate. *)
let set t { x; y; } chr =
  if y >= t.dim.height
  then ()
  else
    let row = select_row_exn t y in
    if x >= Array.length row
    then ()
    else Array.set row x chr

let get t { x; y; } =
  if y >= t.dim.height
  then null_byte
  else
    let row = select_row_exn t y in
    if x >= Array.length row
    then null_byte
    else Array.get row x

(* CR datkin: What happens at out of bounds? *)
let incr { x; y; } { width; height; } =
  let next = (y * width) + x + 1 in
  let y = (next / width) mod height in
  let x = next mod width in
  { x; y; }

let%test_unit "incr coord" =
  [%test_result: coord]
    ~expect:origin
    (incr { x = 6; y = 10; } { width = 7; height = 11; })
;;

(* CR datkin: This doesn't get run properly. *)
let%expect_test "expect test" =
  printf "hi\n%!";
  [%expect "bye"];
;;

(* Write char to the current cursor and move the cursor. *)
let putc t chr =
  set t t.cursor chr;
  t.cursor <- incr t.cursor t.dim;
  ()

let update t buf =
  let rec copy i =
    if i < String.length buf
    then begin
      let chr = String.get buf i in
      begin
        if chr = '\n'
        then t.cursor <- incr t.cursor t.dim
        else putc t chr
      end;
      copy (i+1)
    end
  in
  copy 0

let cursor t = t.cursor

let render _t _out =
  assert false
