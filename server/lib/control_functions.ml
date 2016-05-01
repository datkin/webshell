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
  type parser =
    | Constant of string
    | Number

  let c str = Constant str
  let csi = c "\x1b["
  let n = Number

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
end

module Parser = struct
  type step = {
    preceeding_number : [ `none | `optional | `required ];
    next = [ `done of (int list -> t) | `node of node ];
  }
  and node = {
    some_next_allows_number : bool;
    next : step Char.Map.t;
  }

  let empty = {
    some_next_allows_number = false; (* Should never be true for root. *)
    next = Char.Map.empty;
  }

  type state = {
    node : node;
    current_number : int option;
    stack : int list;
  }

  let init_state root = {
    node = root;
    current_number = None;
    stack = [];
  }

  let step state chr =
    if state.node.some_next_allows_number
    && chr >= '0' && chr <= '9'
    then
      let digit = Char.to_int chr - Char.to_int 0 in
      let current_number =
        (Option.value state.current_number ~default:0) * 10 + digit
      in
      `keep_going { state with current_number }
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
          | `node node -> `keep_going { node; current_number = None; stack; }
  ;;
end

let parse reader =
