open Core_kernel.Std

type dim = {
  width : int;
  height : int;
}

type coord = {
  x : int;
  y : int;
}

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

let update _t _buf = assert false

let cursor t = t.cursor

let null_byte = Char.of_int_exn 0

let get t { x; y; } =
  if y >= t.num_rows
  then null_byte
  else
    let rec select_row y rows =
      match t.num_rows - y, rows with
      | 0, row :: _ -> row
      | n, _ :: rows -> select_row (y-1) rows
      | _, [] -> assert false
    in
    let row = select_row y t.rows in
    if x >= Array.length row
    then null_byte
    else Array.get row x
