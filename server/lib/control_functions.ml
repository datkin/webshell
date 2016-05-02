open Core.Std

type dir =
  | Up
  | Down
  | Left
  | Right

type t =
  | Ack
  | Bell
  | Insert_blank of int
  | Cursor of dir * int

module Spec = struct
  type helper =
    | Constant of char list
    | Number_optional
    | Number_required

  let c str = Constant (Char.to_list str)
  let csi = c "\x1b["
  let n = Number_optional

  let s t = (* simple *)
    function
      | [] -> t
      | _ -> assert false
  ;;

  let n' ctor default = (* single number arg *)
    function
      | [n] -> ctor n
      | [] -> ctor default
      | _ -> assert false
  ;;

  let spec = [
    [c "\x06"], s Ack;
    [c "\x07"], s Bell;
    [csi; n; c "@"], n' (fun x -> Insert_blank n) 1;
    [csi; n; c "A"], n' (fun x -> Cursor (Up, n)) 1;
    [csi; n; c "B"], n' (fun x -> Cursor (Down, n)) 1;
    [csi; n; c "C"], n' (fun x -> Cursor (Left, n)) 1;
    [csi; n; c "D"], n' (fun x -> Cursor (Right, n)) 1;
  ]

  type elt = {
    preceeding_number : [ `none | `optional | `required ];
    char : char;
  }

  type t = {
    first_char : char;
    elts : elt list;
  }

  let rec elts_of_helpers helpers =
    match helper with
    | [] -> []
    | [ Number_optional ]
    | [ Number_required ]
      -> assert false (* Numbers must be followed by a char *)
    | (Constant chars) :: rest ->
      let elts =
        List.map chars ~f:(fun char -> { preceeding_number = `none; char; })
      in
      elts @ (elts_of_helpers rest)
    | Number_optional :: (Constant (char :: chars)) :: rest ->
      let elts =
        { preceeding_number = `optional; char; }
        :: List.map chars ~f:(fun char -> { preceeding_number = `none; char; })
      in
      elts @ (elts_of_helpers rest)
    | Number_required :: (Constant (char :: chars)) :: rest ->
      let elts =
        { preceeding_number = `required; char; }
        :: List.map chars ~f:(fun char -> { preceeding_number = `none; char; })
      in
      elts @ (elts_of_helpers rest)
  ;;

  let of_helpers helpers =
    match helpers with
    | Constant (first_char :: chrs) :: rest ->
      let elts =
        ((Constant chrs) :: rest)
        |> elts_of_helpers
      in
      { first_char; elts; }
    | _ -> assert false (* Must start with a character. *)
  ;;

  let t =
    List.map specs ~f:(fun (helpers, fn) -> of_helper helpers, fn)
end

module Parser = struct
  type next =
    [ `done of (int list -> t) | `node of node ]
  and step = {
    preceeding_number : [ `none | `optional | `required ];
    next = next;
  }
  and node = {
    some_next_allows_number : bool;
    next : step Char.Map.t;
  }

  let empty = {
    some_next_allows_number = false; (* Should never be true for root. *)
    next = Char.Map.empty;
  }

  let rec init_stuff elts fn : next =
    match elts with
    | [] -> `done fn
    | { preceeding_number; char; } :: elts ->
      let some_next_allows_number =
        match preceeding_number with
        | `required | `optional -> true
        | `none -> false
      in
      let step = init_stuff elts fn in
      let next = Map.add Char.Map.empty ~key:char ~data:step in
      `node { some_next_allows_number; next; }
  and add_stuff node elts fn : step =
    match elts with
    | [] -> assert false (* trying to add a terminal where there's a non-terminal *)
    | { preceeding_number; char; } :: elts ->
      match Map.find node.next char with
      | None ->
        let some_next_allows_number =
          match preceeding_number with
          | `required | `optional -> true
          | `none -> node.some_next_allows_number
        in
        let next = Map.add node.next ~key:char ~data:(init_stuff elts fn) in
        { ...
      | Some next ->
  ;;

  let add root { first_char; elts; } fn =
    let step : step =
      match Map.find root.next first_char with
      | None ->
        let next = init_stuff elts fn in
        { preceeding_number = `none; next; }
      | Some { preceeding_number; next; } ->
        assert (preceeding_number = `none);
        match next with
        | `done _ -> assert false (* dupe definition *)
        | `node node -> add_stuff node elts fn
    in
    { root with next = Map.put root.next ~key:first_char ~data:step; }
  ;;

  type state = {
    node : node;
    current_number : int option;
    stack : int list;
    chars : char list;
  }

  let init_state root = {
    node = root;
    current_number = None;
    stack = [];
    chars = [];
  }

  let step state chr =
    let chars = chr :: state.chars in
    if state.node.some_next_allows_number
    && chr >= '0' && chr <= '9'
    then
      let digit = Char.to_int chr - Char.to_int 0 in
      let current_number =
        (Option.value state.current_number ~default:0) * 10 + digit
      in
      `keep_going { state with current_number; chars; }
    else
      match Map.find state.node.next chr with
      | None -> `no_match
      | Some { preceeding_number; next } ->
        match preceeding_number, state.current_number with
        | `none, Some _ -> `no_match
        | `required, None -> `no_match
        | _, _ ->
          let stack =
            match state.current_number with
            | None -> state.stack
            | Some n -> n :: state.stack
          in
          match next.step with
          | `done fn -> `ok (fn (List.rev stack))
          | `node node ->
            `keep_going { node; current_number = None; stack; chars; }
  ;;
end

let root = assert false

let parse reader =
  let init = Parser.empty_state root in
  Pipe.init (fun writer ->
    Reader.pipe reader
    |> Pipe.fold_without_pushback ~init ~f:(fun state str ->
      String.fold str ~init:state ~f:(fun state chr ->
        match Parser.step state chr wit
        | `keep_going state -> state
        | `ok value ->
          Pipe.write_without_pushback writer value;
          empty
        | `no_match ->
          let value =
            match state.chars with
            | [] -> `literal chr;
            | chars ->
              let str = chr :: chars |> List.rev |> String.of_list in
              `junk str
          in
          Pipe.write_without_pushback writer value;
          empty)))
  ;;
