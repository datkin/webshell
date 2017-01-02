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
  | Other of (string list * int option list)
[@@deriving sexp, compare]

type func = t [@@deriving sexp, compare]

module Spec = struct
  type helper =
    | Constant of char list
    | Number

  let c str = Constant (String.to_list str)
  let csi = c "\x1b["
  let n = Number

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
    preceeding_number : bool;
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
          | false -> "" | true -> "<number?>"
        in
        sprintf "%s%c" preceeding char)
      |> String.concat ~sep:""
    in
    sprintf "%c%s" first_char elts_str

  let rec elts_of_helpers helpers =
    match helpers with
    | [] -> []
    | [ Constant [] ] -> []
    | [ Number ]
      -> assert false (* Numbers must be followed by a char *)
    | (Constant chars) :: rest ->
      let elts =
        List.map chars ~f:(fun char -> { preceeding_number = false; char; })
      in
      elts @ (elts_of_helpers rest)
    | Number :: (Constant (char :: chars)) :: rest ->
      let elts =
        { preceeding_number = true; char; }
        :: List.map chars ~f:(fun char -> { preceeding_number = false; char; })
      in
      elts @ (elts_of_helpers rest)
    | Number :: (Number | Constant []) :: _ -> assert false
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
  type node = {
    value : (int option list -> t) option;
    next_number : node option;
    next : node Char.Map.t;
  } [@@deriving sexp]

  let rec make (elts : Spec.elt list) fn : node =
    match elts with
    | [] ->
      { value = Some fn; next_number = None; next = Char.Map.empty; }
    | { preceeding_number; char; } :: elts ->
      let for_char = {
        value = None;
        next_number = None;
        next =
          Char.Map.empty
          |> Char.Map.add ~key:char ~data:(make elts fn);
      }
      in
      if preceeding_number
      then {
        value = None;
        next = Char.Map.empty;
        next_number = Some for_char;
      }
      else for_char
  ;;

  let rec add' node elts fn : node =
    match (elts : Spec.elt list) with
    | [] ->
      begin
        match node.value with
        | None -> { node with value = Some fn; }
        | Some _ -> assert false
      end
    | { preceeding_number; char; } :: elts ->
      if not preceeding_number
      then
        (
        Map.update node.next char (function
          | None -> make elts fn
          | Some node -> add' node elts fn)
        ; assert false )
      else
        match node.next_number with
        | None ->
          { node with
            next_number = Some {
              value = None;
              next_number = None;
              next =
                Char.Map.empty
                |> Map.add ~key:char ~data:(make elts fn);
            };
          }
        | Some next_number ->
            (
          Map.update next_number.next char (function
            | None -> make elts fn
            | Some node -> add' node elts fn)
          ; assert false)
  ;;

  let add root { Spec. first_char; elts; } fn =
    let elts : Spec.elt list =
      { preceeding_number = false; char = first_char} :: elts
    in
    add' root elts fn

  let add root spec fn =
    try add root spec fn
    with exn ->
      failwithf !"[add] raised:\n%{sexp:Exn.t}\n%{Spec}\n%{sexp:node}" exn spec root ()

  type one_state = {
    node : node;
    current_number : int option;
    stack : int option list;
    chars : char list;
  }

  type state = one_state list

  let init_state root = [ {
    node = root;
    current_number = None;
    stack = [];
    chars = [];
  } ]

  let step' one_state chr : one_state list =
    let open Option.Monad_infix in
    let chars = chr :: one_state.chars in
    let states =
      List.filter_opt [
        begin
          one_state.node.next_number
          >>= fun node ->
          if chr >= '0' && chr <= '9'
          then
            let digit = Char.to_int chr - Char.to_int '0' in
            let current_number =
              Some ((Option.value one_state.current_number ~default:0) * 10 + digit)
            in
            (* We accumulated some digits, but we stay in this state. *)
            Some { one_state with current_number; chars; }
          else None
        end;
        begin
          one_state.node.next_number
          >>= fun node ->
          let stack =
            match one_state.current_number with
            | None -> one_state.stack
            | Some _ as x -> x :: one_state.stack
          in
          Some {
            node;
            current_number = None;
            stack;
            chars;
          }
        end;
        begin
          (* These transitions aren't allowed if we've accumulated numbers. *)
          match one_state.current_number with
          | Some _ -> None
          | None ->
            one_state.next chr
            >>| fun node ->
            {
              node;
              current_number = None;
              stack = one_state.stack;
              chars;
            }
        end;
      ]
    in
    assert false
  ;;

  let step state chr =
    let state =
      List.concat_map state ~f:(fun one_state ->
        step' one_state chr)
    in
    (* Each one of these could be an end state.
     * In theory we should only find one 'finished' state.
     * And if that 'finished' state has more outgoing transitions, we could emit
     * the function and then (potentially) continue traversing. I guess we'd:
     *  - if there's on completion:
          - emit that value,
          - prune [state] to be just that [one_state] plus the [init] state.
        - otherwise, if states is []
          - if there's only been one char, it's just that char literal
          - if there's > 1 char, it's junk
          *)
    match state with
    | [] -> `junk (* check if [state] going in was initial? *)
    | _ ->
      match
        List.filter_map state ~f:(fun one_state ->
          match one_state.finished with
          | None -> None
          | Some x -> Some (x, one_state))
      with
      | [ ] -> `keep_going state
      | [ (fn, one_state) ] ->
          (* CR datkin: Need to reset the [chars] history here so we know if the
           * next step is "junk"? *)
          assert false (* ok *)
      | _ -> assert false (* too many matches *)
  ;;

  let step state chr : [`keep_going of state | `ok of t | `no_match] =
    let chars = chr :: state.chars in
    if state.node.some_next_allows_number
    && chr >= '0' && chr <= '9'
    then
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
          | String seq -> Some (seq, key)
          | _ -> None)
      |> String.Map.of_alist_multi
      |> Map.to_alist
      |> List.map ~f:(fun (seq, values) ->
          (* Hacks... *)
          let fn = (fun args -> Other (values, args)) in
          (* We'll rely on \000 not being present to make splitting possible...  *)
          let special_digit_char = '\000' in
          let seq =
             (* Ignore the %i, %p1, %p2, etc semantics (see `man terminfo` --
              * %p[1-9] are numbered parameters on the stack, %i adds 1 to
              * some of the params?).
              * %p<n>%t -- pop the n'th element off the stack?
              *)
            seq
            |> String.substr_replace_all ~pattern:"%i" ~with_:""
            |> String.substr_replace_all ~pattern:"%p1" ~with_:""
            |> String.substr_replace_all ~pattern:"%p1" ~with_:""
            |> String.substr_replace_all ~pattern:"%d" ~with_:(Char.to_string special_digit_char)
          in
          let helpers =
            String.split seq ~on:special_digit_char
            |> List.map ~f:(fun str -> Spec.c str)
            |> List.intersperse ~sep:Spec.n
          in
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
