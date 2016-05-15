open Core.Std

type dir =
  | Up
  | Down
  | Left
  | Right
[@@deriving sexp, compare]

type up_or_down =
  | Up
  | Down
[@@deriving sexp, compare]

type coord = {
  x : int;
  y : int;
} [@@deriving sexp, compare]

type t =
  | Ack
  | Bell
  | Insert_blank of int
  | Cursor_rel of dir * int
  | Start_of_line_rel of up_or_down * int
  | Cursor_abs of coord
  | Other of string
[@@deriving sexp, compare]

type func = t [@@deriving sexp, compare]

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
      | [n] -> ctor (Option.value n ~default)
      | _ -> assert false
  ;;

  let n'' ctor default = (* two args *)
    function
      | [n1; n2] -> ctor (Option.value n1 ~default, Option.value n2 ~default)
      | _ -> assert false
  ;;

  let spec = [
    [c "\x06"], s Ack;
    [c "\x07"], s Bell;
    [csi; n; c "@"], n' (fun x -> Insert_blank x) 1;
    [csi; n; c "A"], n' (fun x -> Cursor_rel (Up, x)) 1;
    [csi; n; c "B"], n' (fun x -> Cursor_rel (Down, x)) 1;
    [csi; n; c "C"], n' (fun x -> Cursor_rel (Left, x)) 1;
    [csi; n; c "D"], n' (fun x -> Cursor_rel (Right, x)) 1;
    [csi; n; c "E"], n' (fun x -> Start_of_line_rel (Down, x)) 1;
    [csi; n; c "F"], n' (fun x -> Start_of_line_rel (Up, x)) 1;
    [csi; n; c ";"; n; c "H"], n'' (fun (x, y) -> Cursor_abs {x; y}) 1;
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
    [ `finished of (int option list -> t) | `node of node ]
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
    stack : int option list;
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
        let stack_maybe =
          match preceeding_number, state.current_number with
          | `none, Some _ -> None
          | `required, None -> None
          | `none, None -> Some state.stack
          | `required, ((Some _) as n)
          | `optional, n -> Some (n :: state.stack)
        in
        match stack_maybe with
        | None -> `no_match
        | Some stack ->
          match next with
          | `finished fn -> `ok (fn (List.rev stack))
          | `node node ->
            `keep_going { node; current_number = None; stack; chars; }
  ;;

  let init spec =
    let parser =
      List.fold spec ~init:empty ~f:(fun parser (spec, fn) ->
        add parser spec fn)
    in
    init_state parser
  ;;

  let default = init Spec.t

  let of_terminfo terminfo =
    let specs =
      Terminfo.capabilities terminfo
      |> Map.to_alist
      |> List.filter_map ~f:(fun (key, value) ->
          match value with
          | String str -> Some (key, str)
          | _ -> None)
      |> List.map ~f:(fun (key, str) ->
          let fn = (fun _ -> Other str) in
          let helpers = [Spec.c key] in
          Spec.of_helpers helpers, fn)
    in
    init specs
end

let parser init =
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

let%test_unit _ =
  let f = unstage (parser Parser.default) in
  let test here chr expect =
    [%test_result: [`literal of char | `func of func | `junk of string | `pending]]
      ~here:[here]
      (f chr)
      ~expect
  in
  test [%here] 'a' (`literal 'a');
  test [%here] '\x06' (`func Ack);
  test [%here] '\x1b' `pending;
  test [%here] '[' `pending;
  test [%here] '5' `pending;
  test [%here] '1' `pending;
  test [%here] 'A' (`func (Cursor_rel (Up, 51)));
  test [%here] '\x1b' `pending;
  test [%here] '[' `pending;
  test [%here] 'X' (`junk "\x1b[X");
  let test_seq here str expect =
    let rec loop chrs =
      match chrs with
      | [] -> assert false
      | [chr] ->
        [%test_result: [`literal of char | `func of func | `junk of string | `pending]]
          ~here:[here]
          (f chr)
          ~expect:(`func expect)
      | chr :: chrs ->
        [%test_result: [`literal of char | `func of func | `junk of string | `pending]]
          ~here:[here]
          (f chr)
          ~expect:(`pending);
        loop chrs
    in
    loop (String.to_list str)
  in
  test_seq [%here] "\x1b[A" (Cursor_rel (Up, 1));
  test_seq [%here] "\x1b[;H" (Cursor_abs {x = 1; y = 1;});
  test_seq [%here] "\x1b[5;H" (Cursor_abs {x = 5; y = 1;});
  test_seq [%here] "\x1b[;5H" (Cursor_abs {x = 1; y = 5;});
  test_seq [%here] "\x1b[6;5H" (Cursor_abs {x = 6; y = 5;});
;;

open Async.Std

let parse reader init =
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
