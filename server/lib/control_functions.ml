open Core.Std

(* CR datkin: This is needed to warnings on the generated sexp functions in this
 * file, I believe. Not sure why. But obviously it should be removed. *)
[@@@ocaml.warning "-4"]

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

(* Prefer referring to this below for clarity. *)
type control_function = t [@@deriving sexp, compare]

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
      | ns -> failwithf !"Called with %{sexp:int option list}" ns ()
  ;;

  let n' ctor default = (* single number arg *)
    function
      | [n] -> ctor (Option.value n ~default)
      | ns -> failwithf !"Called with %{sexp:int option list}" ns ()
  ;;

  let n'' ctor default = (* two args *)
    function
      | [n1; n2] -> ctor (Option.value n1 ~default, Option.value n2 ~default)
      | ns -> failwithf !"Called with %{sexp:int option list}" ns ()
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
      (* CR datkin: Check that [char] isn't a digit and also that the preceeding
       * char isn't a digit. *)
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
  (* CR datkin: In a prior version of this code, I thought that numbers were
   * sometimes optional arguments to control functions. Why did I think that?
   * Are there examples? The specs may allow it, but I suspect curses, at least,
   * doesn't do it? That's not good enough for every app, though. :/ *)
  (* CR datkin: Any case where [value] is some either [next_number] is some or
   * [next] is non-empty indicates an ambiguity. I need to look at some examples
   * and figure out how to handle them. *)
  (* The [node] represents all the possible [control_function]s that could
   * result from a sequence of input bytes.
   *
   * If [value] is some, then there's a control_function defined by the bytes
   * that have been entered so far.
   *
   * If [next_number] is some, then there's one or more control functions that
   * are the bytes entered so far, followed next by a numeric argument.
   *
   * The [next] map stores the [control_function]s that are the bytes entered so
   * far followed by the (non-digit) byte keys in the map. *)
  type node = {
    value : (int option list -> control_function) option;
    next_number : node option;
    next : node Char.Map.t;
  } [@@deriving sexp]

  let empty_node = {
    value = None;
    next_number = None;
    next = Char.Map.empty;
  }

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
    | { preceeding_number = false; char; } :: elts ->
      let next =
        Map.update node.next char ~f:(function
          | None -> make elts fn
          | Some node -> add' node elts fn)
      in
      { node with next }
    | { preceeding_number = true; char; } :: elts ->
      let next_number =
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
          let next_number's_next =
            Map.update next_number.next char ~f:(function
              | None -> make elts fn
              | Some node -> add' node elts fn)
          in
          { next_number with next = next_number's_next }
      in
      { node with next_number = Some next_number }
  ;;

  let add root { Spec. first_char; elts; } fn =
    let elts : Spec.elt list =
      { preceeding_number = false; char = first_char } :: elts
    in
    add' root elts fn

  let add root spec fn =
    try add root spec fn
    with exn ->
      failwithf !"[add] raised:\n%{sexp:Exn.t}\n%{Spec}\n%{sexp:node}" exn spec root ()

  type state = {
    node : node;
    current_number : int option;
    stack : int option list;
    chars : char list;
  } [@@deriving sexp]

  let init_state root = {
    node = root;
    current_number = None;
    stack = [];
    chars = [];
  }

  type result = [
    | `keep_going of state
    | `ok of control_function
    | `no_match of char list
  ]

  let step_char stack current_number all_chars node =
    let stack =
      (* We may have just finished a digit. *)
      match current_number with
      | None -> stack
      | Some n -> Some n :: stack
    in
    match node.value with
    | Some fn ->
      (* CR datkin: Check for ambiguities: if [next_node_by_char] has any
       * subsequent nodes. *)
      `ok (fn (List.rev stack))
    | None ->
      `keep_going {
        node;
        stack;
        current_number = None;
        chars = all_chars;
      }
  ;;

  (* Ideas for handling ambiguities?
   *  - if there's on completion:
        - emit that value,
        - prune [state] to be just that [one_state] plus the [init] state.
      - otherwise, if states is []
        - if there's only been one char, it's just that char literal
        - if there's > 1 char, it's junk
        *)
  (* CR datkin: In the current setup, the stack could just be an int list, not
   * an int option list *)
  let step state chr : result =
    let next_by_char =
      Option.map (Map.find state.node.next chr) ~f:(fun node ->
        step_char state.stack state.current_number (state.chars @ [chr]) node)
    in
    let digit =
      if chr >= '0' && chr <= '9'
      then Some (Char.to_int chr - Char.to_int '0')
      else None
    in
    let next_by_char_skipping_number =
      if Option.is_some digit
      then None
      else
      Option.bind state.node.next_number ~f:(fun node ->
        Option.map (Map.find node.next chr) ~f:(fun node ->
        let stack =
          (* Finish the number that was in progress. *)
          match state.current_number with
          | None -> state.stack
          | Some n ->
            (* CR datkin: If we his this case things are *very* werid. *)
            Some n :: state.stack
        in
        let stack =
          (* We're looking *past* the next_number node, so push a [None] on the
           * stack. *)
          None :: stack
        in
        step_char stack None (state.chars @ [chr]) node))
    in
    let next_continuing_number =
      Option.both state.current_number digit
      |> Option.map ~f:(fun (current_number, digit) ->
        let current_number = Some ((current_number * 10) + digit) in
        `keep_going { state with
          current_number;
          chars = state.chars @ [chr];
        })
    in
    let next_new_number =
      Option.both state.node.next_number digit
      |> Option.map ~f:(fun (node, digit) ->
        `keep_going { state with
          node;
          current_number = Some digit;
          chars = state.chars @ [chr];
        })
    in
    let next =
      List.filter_opt [
        next_new_number;
        next_continuing_number;
        next_by_char;
        next_by_char_skipping_number;
      ]
    in
    match next with
    | [] -> `no_match (state.chars @ [chr])
    | [ next ] -> next
    | next :: _ :: _ ->
      (* Ambiguity: We're in the middle of parsing a number, but the next
       * node says that we could also have a number. This definitely
       * shouldn't happen. It would represent a control sequence like
       * '%p1%d%p2%d'. *)
      Core.Std.eprintf !"Ambiguity at %{sexp:Source_code_position.t}\n%!" [%here];
      next
  ;;

  let init spec =
    let parser =
      List.fold spec ~init:empty_node ~f:(fun parser (spec, fn) ->
        add parser spec fn)
    in
    init_state parser
  ;;

  let default = init Spec.t

  let of_capabilities caps_alist =
    let specs =
      List.filter_map caps_alist ~f:(fun (key, value) ->
          match (value : Terminfo.value) with
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

  let of_terminfo terminfo =
    Terminfo.capabilities terminfo
    |> Map.to_alist
    |> of_capabilities
end

let parser init =
  (* printf !"\n<starting> %{sexp:Parser.state}\n" init; *)
  let state = ref init in
  stage (fun chr ->
    match Parser.step !state chr with
    | `keep_going s ->
      (* printf !"%c => %{sexp:Parser.state}\n" chr s; *)
      state := s;
      `pending
    | `ok value ->
      (* printf !"\n<reset> %{sexp:Parser.state}\n" init; *)
      state := init;
      (`func value)
    | `no_match [] ->
      (* at least the char we just entered should be in the list *)
     assert false
    | `no_match [ chr ] -> state := init; `literal chr
    | `no_match chars ->
      state := init;
      `junk (String.of_char_list chars))

let%test_unit _ =
  let f = unstage (parser Parser.default) in
  let test here chr expect =
    [%test_result: [`literal of char | `func of control_function | `junk of string | `pending]]
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
  let test_seq ?(p=Parser.default) here str expect =
    let f = unstage (parser p) in
    let rec loop chrs =
      match chrs with
      | [] -> assert false
      | [chr] ->
        [%test_result: [`literal of char | `func of control_function | `junk of string | `pending]]
          ~here:[here]
          (f chr)
          ~expect:(`func expect)
      | chr :: chrs ->
        [%test_result: [`literal of char | `func of control_function | `junk of string | `pending]]
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
  test_seq
    ~p:(Parser.of_capabilities [
      Terminfo.parse_entry {|csr=\E[%i%p1%d;%p2%dr|} |> Or_error.ok_exn;
      (*
       {|\E[%i%p1%d;%p2%dr|}, Terminfo.String "csr";
       *)
    ])
    [%here]
    "\x1b[1;30r" (Other (["csr"], [Some 1; Some 30;]));
;;

open Async.Std

let parse reader init =
  Pipe.create_reader ~close_on_exception:true (fun writer ->
    Reader.pipe reader
    |> Pipe.fold_without_pushback ~init ~f:(fun state str ->
      String.fold str ~init:state ~f:(fun state chr ->
        match Parser.step state chr with
        | `keep_going state -> state
        | `ok value ->
          Pipe.write_without_pushback writer (`func value);
          init
        | `no_match [] -> assert false
        | `no_match [chr] ->
           Pipe.write_without_pushback writer (`literal chr);
           init
        | `no_match chars ->
           Pipe.write_without_pushback writer (`junk (String.of_char_list chars));
           init))
    >>= fun (_ : Parser.state) ->
    Deferred.unit)
