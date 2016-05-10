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

  let c str = Constant (String.to_list str)
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
    [csi; n; c "@"], n' (fun x -> Insert_blank x) 1;
    [csi; n; c "A"], n' (fun x -> Cursor (Up, x)) 1;
    [csi; n; c "B"], n' (fun x -> Cursor (Down, x)) 1;
    [csi; n; c "C"], n' (fun x -> Cursor (Left, x)) 1;
    [csi; n; c "D"], n' (fun x -> Cursor (Right, x)) 1;
  ]

  type elt = {
    preceeding_number : [ `none | `optional | `required ];
    char : char;
  } [@@deriving sexp]

  type t = {
    first_char : char;
    elts : elt list;
  } [@@deriving sexp]

  let to_string { first_char; elts } =
    let elts_str =
      List.map elts ~f:(fun { preceeding_number; char; } ->
        let preceeding =
          match preceeding_number with
          | `none -> "" | `optional -> "<number?>" | `required -> "<number>"
        in
        sprintf "%s%c" preceeding char)
      |> String.concat ~sep:""
    in
    sprintf "%c%s" first_char elts_str

  let rec elts_of_helpers helpers =
    match helpers with
    | [] -> []
    | [ Constant [] ] -> []
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
    | (Number_required | Number_optional) :: (Number_required | Number_optional | Constant []) :: _ -> assert false
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
    List.map spec ~f:(fun (helpers, fn) -> of_helpers helpers, fn)
end

module Parser = struct
  type next =
    [ `finished of (int list -> t) | `node of node ]
  and step = {
    (* CR datkin: Hmm. This is tricky. If it's a branch point, some branches may
     * require a number, and others may not allow a number. Can we expect that
     * all prefixes will have the same number requirements here? *)
    preceeding_number : [ `none | `optional | `required ];
    next : next;
  }
  and node = {
    some_next_allows_number : bool;
    steps : step Char.Map.t;
  } [@@deriving sexp]

  let rec make_next elts fn : next =
    match elts with
    | [] -> `finished fn
    | { Spec. preceeding_number; char; } :: elts ->
      let step =
        let next = make_next elts fn in
        { preceeding_number; next; }
      in
      let steps = Map.add Char.Map.empty ~key:char ~data:step in
      let some_next_allows_number =
        match preceeding_number with
        | `required | `optional -> true
        | `none -> false
      in
      `node { some_next_allows_number; steps; }

  let rec update_step ~preceeding_number:pn' ~node:{ some_next_allows_number; steps; } elts fn : step =
    match elts with
    | [] -> assert false (* trying to add a terminal where there's a non-terminal *)
    | { Spec. preceeding_number; char; } :: elts ->
      begin
        let implied =
          match preceeding_number with
          | `required | `optional -> true
          | `none -> false
        in
        if implied <> some_next_allows_number
        then
          (* Again, assume they're the same. *)
          assert false
      end;
      let steps =
        Map.update steps char ~f:(function
          | None ->
            let next = make_next elts fn in
            { preceeding_number; next; }
          | Some { preceeding_number = pn''; next; } ->
            begin
              if pn'' <> preceeding_number
              then
                (* For now, assume that the path must always be the same.
                 * What are the counter examples? *)
                failwithf !"%{sexp:[`required|`none|`optional] list} at %c"
                  [pn''; preceeding_number] char ()
            end;
            match next with
            | `finished _ -> assert false (* adding terminal or dupe def *)
            | `node node ->
              update_step ~preceeding_number ~node elts fn)
      in
      let next = `node { some_next_allows_number; steps; } in
      { preceeding_number = pn'; next; }
  ;;

  let add root { Spec. first_char; elts; } fn =
    let step : step =
      match Map.find root.steps first_char with
      | None ->
        let next = make_next elts fn in
        { preceeding_number = `none; next; }
      | Some { preceeding_number; next; } ->
        assert (preceeding_number = `none);
        match next with
        | `finished _ -> assert false (* dupe definition *)
        | `node node -> update_step ~preceeding_number ~node elts fn
    in
    { root with steps = Map.add root.steps ~key:first_char ~data:step; }
  ;;

  let add root spec fn =
    try add root spec fn
    with exn ->
      failwithf !"[add] raised:\n%{sexp:Exn.t}\n%{Spec}\n%{sexp:node}" exn spec root ()

  let empty = {
    some_next_allows_number = false; (* Should never be true for root. *)
    steps = Char.Map.empty;
  }

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

  let step state chr : [`keep_going of state | `ok of t | `no_match] =
    let chars = chr :: state.chars in
    if state.node.some_next_allows_number
    && chr >= '0' && chr <= '9'
    then
      let digit = Char.to_int chr - Char.to_int '0' in
      let current_number =
        Some ((Option.value state.current_number ~default:0) * 10 + digit)
      in
      `keep_going { state with current_number; chars; }
    else
      match Map.find state.node.steps chr with
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
          match next with
          | `finished fn -> `ok (fn (List.rev stack))
          | `node node ->
            `keep_going { node; current_number = None; stack; chars; }
  ;;
end

let init =
  let parser =
    List.fold Spec.t ~init:Parser.empty ~f:(fun parser (spec, fn) ->
      Parser.add parser spec fn)
  in
  Parser.init_state parser
;;

let parser () =
  let state = ref init in
  stage (fun chr ->
    match Parser.step !state chr with
    | `keep_going s -> state := s; `pending
    | `ok value ->
      state := init;
      (`func value)
    | `no_match ->
      let value =
        match !state.chars with
        | [] -> `literal chr;
        | chars ->
          let str = (chr :: chars) |> List.rev |> String.of_char_list in
          `junk str
      in
      state := init;
      value)

open Async.Std

let parse reader =
  Pipe.init (fun writer ->
    Reader.pipe reader
    |> Pipe.fold_without_pushback ~init ~f:(fun state str ->
      String.fold str ~init:state ~f:(fun state chr ->
        match Parser.step state chr with
        | `keep_going state -> state
        | `ok value ->
          Pipe.write_without_pushback writer (`func value);
          init
        | `no_match ->
          let value =
            match state.chars with
            | [] -> `literal chr;
            | chars ->
              let str = (chr :: chars) |> List.rev |> String.of_char_list in
              `junk str
          in
          Pipe.write_without_pushback writer value;
          init))
    >>= fun (_ : Parser.state) ->
    Deferred.unit)
