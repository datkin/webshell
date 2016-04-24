open Core_kernel.Std

type dim = {
  width : int;
  height : int;
}

type coord = {
  x : int;
  y : int;
} [@@deriving sexp, compare]

let origin = { x = 0; y = 0; }

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
  assert (t.dim.height <= t.num_rows);
  assert (t.cursor.x <= t.dim.width && t.cursor.y <= t.dim.height);
;;

let set_dimensions t dim = t.dim <- dim

let null_byte = Char.of_int_exn 0

let select_row_exn t y =
  let rec loop y rows =
    match t.num_rows - y, rows with
    | 0, row :: _ -> row
    | n, _ :: rows -> loop (y-1) rows
    | _, [] -> assert false
  in
  assert (y >= 0);
  assert (y < t.num_rows);
  loop y t.rows

(* Just set the character at the given coordinate. *)
let set t { x; y; } chr =
  if y >= t.num_rows
  then ()
  else
    let row = select_row_exn t y in
    if x >= Array.length row
    then ()
    else Array.set row x chr

let get t { x; y; } =
  if y >= t.num_rows
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

let render t out =
  assert false
