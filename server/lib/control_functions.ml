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
    (* [Numbers] is used for our direct (i.e. non-terminfo) xterm support. In
     * the xterm spec, it looks like [Numbers] only ever occurs once in any
     * given sequence, and never occurs alongside a [Number]. *)
    | Numbers

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
      | [n1; n2] -> ctor (Option.value n1 ~default) (Option.value n2 ~default)
      | ns -> failwithf !"Called with %{sexp:int option list}" ns ()
  ;;

  let default_spec = [
    [c "\x06"], s Ack;
    [c "\x07"], s Bell;
    [csi; n; c "@"], n' (fun x -> Insert_blank x) 1;
    [csi; n; c "A"], n' (fun x -> Cursor_rel (Up, x)) 1;
    [csi; n; c "B"], n' (fun x -> Cursor_rel (Down, x)) 1;
    [csi; n; c "C"], n' (fun x -> Cursor_rel (Left, x)) 1;
    [csi; n; c "D"], n' (fun x -> Cursor_rel (Right, x)) 1;
    [csi; n; c "E"], n' (fun x -> Start_of_line_rel (Down, x)) 1;
    [csi; n; c "F"], n' (fun x -> Start_of_line_rel (Up, x)) 1;
    [csi; n; c ";"; n; c "H"], n'' (fun x y -> Cursor_abs {x; y}) 1;
  ]

  type elt = {
    preceeding_number : [`no | `one_optional | `many];
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
          | `no -> ""
          | `one_optional -> "<number?>"
          | `many -> "<number, ...>"
        in
        sprintf "%s%c" preceeding char)
      |> String.concat ~sep:""
    in
    sprintf "%c%s" first_char elts_str

  let rec elts_of_helpers helpers =
    match helpers with
    | [] -> []
    | [ Constant [] ] -> []
    | [ (Number | Numbers) ]
      -> assert false (* Numbers must be followed by a char *)
    | (Constant chars) :: rest ->
      let elts =
        List.map chars ~f:(fun char -> { preceeding_number = `no; char; })
      in
      elts @ (elts_of_helpers rest)
    | ((Number | Numbers) as n) :: (Constant (char :: chars)) :: rest ->
      (* CR datkin: Check that [char] isn't a digit and also that the preceeding
       * char isn't a digit. *)
      let elts =
        let preceeding_number =
          match n with
          | Number -> `one_optional
          | Numbers -> `many
          | _ -> assert false
        in
        { preceeding_number; char; }
        :: List.map chars ~f:(fun char -> { preceeding_number = `no; char; })
      in
      elts @ (elts_of_helpers rest)
    | (Number | Numbers) :: (Number | Numbers | Constant []) :: _ -> assert false
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

  let default =
    List.map default_spec ~f:(fun (helpers, fn) -> of_helpers helpers, fn)

  let ps = Number
  let pm = Numbers

  let xterm_spec_for_test = [
    [csi; ps; c ";"; ps; c "H"], n'' (fun x y -> Other (["CUP"], [Some x; Some y])) 1;
    [csi; pm; c "m"], (fun args -> Other (["SGR"], args));
  ]

  let xterm_for_test =
    List.map xterm_spec_for_test ~f:(fun (helpers, fn) -> of_helpers helpers, fn)

  let xterm_spec = [
    (* CR datkin: It would be nice to have a tool for defining variable length
     * args. *)
    (* From http://www.xfree86.org/4.5.0/ctlseqs.html
     * Some of this stuff isn't expressed in terminfo for xterm. E.g. "Send
     * Device Attributes (Secondary DA)". *)
    [c "\x06"], s Ack;
    [c "\x07"], s Bell;
    [csi; n; c "@"], n' (fun x -> Insert_blank x) 1;
    [csi; n; c "A"], n' (fun x -> Cursor_rel (Up, x)) 1;
    [csi; n; c "B"], n' (fun x -> Cursor_rel (Down, x)) 1;
    [csi; n; c "C"], n' (fun x -> Cursor_rel (Left, x)) 1;
    [csi; n; c "D"], n' (fun x -> Cursor_rel (Right, x)) 1;
    [csi; n; c "E"], n' (fun x -> Start_of_line_rel (Down, x)) 1;
    [csi; n; c "F"], n' (fun x -> Start_of_line_rel (Up, x)) 1;
    [csi; ps; c "J"], n' (fun x -> Other (["ED"], [Some x])) 0;
    [csi; ps; c "K"], n' (fun x -> Other (["EL"], [Some x])) 0;
    [csi; pm; c "H"], (fun args -> Other (["CUP"], args));
    [csi; pm; c "m"], (fun args -> Other (["SGR"], args));
    [csi; c "?"; pm; c "h"], (fun args -> Other (["DECSET"], args));
    [csi; c "?"; pm; c "l"], (fun args -> Other (["DECRST"], args));
    [c "\x1b="], s (Other (["DECPAM"], []));
    (* CR datkin: Defaults for the following are wrong. *)
    [csi; ps; c ";"; ps; c "r"], n'' (fun x y -> Other (["DECSTBM"], [Some x; Some y])) 1;
    (* "Send Device Attributes (Secondary DA)"
     * This one looks weird, see:
     * http://www.vt100.net/docs/vt510-rm/DA2.html
     * http://www.vt100.net/docs/vt510-rm/DA1.html
     *)
    [csi; c ">"; ps; c "c"], n' (fun x -> Other (["Send Device Attrib (secondary)"], [Some x])) 0;
  ]

  let xterm =
    List.map xterm_spec ~f:(fun (helpers, fn) -> of_helpers helpers, fn)
end

module Parser = struct
  (*
  type next_number_info = {
    one_optional : bool;
    many : bool;
  } [@@derivng sexp]
  *)

  (* The [node] represents all the possible [control_function]s that could
   * result from a sequence of input bytes.
   *
   * If [value] is some, then there's a control_function defined by the bytes
   * that have been entered so far.
   *
   * If [next_number__*] is some, then there's one or more control functions
   * that are the bytes entered so far, followed next by a numeric argument (an
   * optional single argument or multiple ';'-separated arguments,
   * respectively).
   *
   * The [next] map stores the [control_function]s that are the bytes entered so
   * far followed by the (non-digit) byte keys in the map. *)
  type node = {
    value : (int option list -> control_function) option;
    next_number__one_optional : node option;
    next_number__many : node option;
    next : node Char.Map.t;
  } [@@deriving sexp]

  let empty_node = {
    value = None;
    next_number__one_optional = None;
    next_number__many = None;
    next = Char.Map.empty;
  }

  let rec make (elts : Spec.elt list) fn : node =
    match elts with
    | [] -> {
      value = Some fn;
      next_number__one_optional = None;
      next_number__many = None;
      next = Char.Map.empty;
    }
    | { preceeding_number; char; } :: elts ->
      let for_char = {
        value = None;
        next_number__one_optional = None;
        next_number__many = None;
        next =
          Char.Map.empty
          |> Char.Map.add ~key:char ~data:(make elts fn);
      }
      in
      match preceeding_number with
      | `no -> for_char
      | `one_optional -> {
          value = None;
          next = Char.Map.empty;
          next_number__one_optional = Some for_char;
          next_number__many = None;
        }
      | `many -> {
          value = None;
          next = Char.Map.empty;
          next_number__one_optional = None;
          next_number__many = Some for_char;
        }
  ;;

  let rec add' node elts fn : node =
    match (elts : Spec.elt list) with
    | [] ->
      begin
        match node.value with
        | None -> { node with value = Some fn; }
        | Some _ -> assert false
      end
    | { preceeding_number = `no; char; } :: elts ->
      let next =
        Map.update node.next char ~f:(function
          | None -> make elts fn
          | Some node -> add' node elts fn)
      in
      { node with next }
    | { preceeding_number = (`one_optional | `many) as kind; char; } :: elts ->
      let next_number =
        let current =
          match kind with
          | `one_optional -> node.next_number__one_optional
          | `many -> node.next_number__many
        in
        (* CR datkin: In a past version of this function I think there was a bug
         * where we double-nested the new nodes in this branch. But I may be
         * missing something. Probably worth a test. *)
        match current with
        | None -> {
            value = None;
            next_number__one_optional = None;
            next_number__many = None;
            next =
              Char.Map.empty
              |> Map.add ~key:char ~data:(make elts fn);
          }
        | Some next_number ->
          let next_number's_next =
            Map.update next_number.next char ~f:(function
              | None -> make elts fn
              | Some node -> add' node elts fn)
          in
          { next_number with next = next_number's_next }
      in
      match kind with
      | `one_optional -> { node with next_number__one_optional = Some next_number }
      | `many -> { node with next_number__many = Some next_number }
  ;;

  let add root { Spec. first_char; elts; } fn =
    let elts : Spec.elt list =
      { preceeding_number = `no; char = first_char } :: elts
    in
    add' root elts fn

  let add root spec fn =
    try add root spec fn
    with exn ->
      failwithf !"[add] raised:\n%{sexp:Exn.t}\n%{Spec}\n%{sexp:node}" exn spec root ()

  type state = {
    node : node;
    current_number : [
      | `none
      | `one_optional of int option (* CR datkin: Does the [None] case actually happen? *)
      | `many of int option (* None means we parsed ';' and require another number *)
    ];
    stack : int option list;
    chars : char list;
  } [@@deriving sexp]

  let init_state root = {
    node = root;
    current_number = `none;
    stack = [];
    chars = [];
  }

  type step_result = [
    | `keep_going of state
    | `ok of (control_function * string)
    | `no_match of char list
  ] [@@deriving sexp]

  let step_char stack current_number all_chars node =
    let stack =
      (* We may have just finished a digit. *)
      match current_number with
      | `none | `one_optional None -> stack
      | `many None -> assert false (* see call sites *)
      | `one_optional (Some n)
      | `many (Some n) -> Some n :: stack
    in
    match node.value with
    | Some fn ->
      (* CR datkin: Check for ambiguities: if [next_node_by_char] has any
       * subsequent nodes. *)
      `ok (fn (List.rev stack), String.of_char_list all_chars)
    | None ->
      `keep_going {
        node;
        stack;
        current_number = `none;
        chars = all_chars;
      }
  ;;

  let step_one state chr : step_result list =
    let next_by_char =
      match state.current_number with
      | `many None ->
        (* The last thing we processed was a ';' as part of a variable-number
         * arg list, the next thing must be another numeric argument, not a
         * literal.  *)
        None
      | _ ->
        Option.map (Map.find state.node.next chr) ~f:(fun node ->
          step_char state.stack state.current_number (state.chars @ [chr]) node)
    in
    let digit =
      if chr >= '0' && chr <= '9'
      then Some (Char.to_int chr - Char.to_int '0')
      else None
    in
    let next_by_char_skipping_number which_next next_number =
      if Option.is_some digit
      (* Let's just assume we don't have numeric args followed by numberic
       * literals (though we definitely have numeric literals preceeding numeric
       * args in some cases, e.g. "setab" in xterm's terminfo). *)
      then None
      else
      Option.bind next_number ~f:(fun node ->
        Option.map (Map.find node.next chr) ~f:(fun node ->
        let stack =
          (* Finish the number that was in progress. *)
          match state.current_number with
          | `none | `one_optional None | `many None -> state.stack
          | `one_optional (Some n)
          | `many (Some n) ->
            (* CR datkin: If we his this case things are *very* werid. *)
            Core.Std.eprintf "\n!!! SOMETHING WEIRD HAPPENED !!!\n%!";
            Some n :: state.stack
        in
        (* CR datkin: We need to do [next_by_char_skipping_number] for both
         * [one_optional] and for [many] -- the difference is that in one case
         * we push a [None] arg, and in the other we push no args -- indicating
         * an empty arg list. *)
        let stack =
          (* We're looking *past* the next_number node, so push a [None] on the
           * stack, if necessary. *)
          match which_next with
          | `many -> stack
          | `one_optional -> None :: stack
        in
        step_char stack `none (state.chars @ [chr]) node))
    in
    let next_by_char_skipping_number__oo =
      next_by_char_skipping_number `one_optional state.node.next_number__one_optional
    in
    let next_by_char_skipping_number__m =
      next_by_char_skipping_number `many state.node.next_number__many
    in
    let next_continuing_number =
      match digit with
      | None -> None
      | Some digit ->
        (* CR datkin: Think more carefully about all these cases. *)
        match state.current_number with
        | `none | `one_optional None
        | `many None (* CR datkin: <-- This case probably shouldn't be allowed
          for same reasons as above. *)
          -> None
        | `one_optional (Some n)
        | `many (Some n) as x ->
          let n = (n * 10) + digit in
          let current_number =
            match x with
            | `one_optional _ -> `one_optional (Some n)
            | `many _ -> `many (Some n)
          in
        Some (`keep_going { state with
          current_number;
          chars = state.chars @ [chr];
        })
    in
    let next_new_number__oo =
      Option.both state.node.next_number__one_optional digit
      |> Option.map ~f:(fun (node, digit) ->
        `keep_going { state with
          node;
          current_number = `one_optional (Some digit);
          chars = state.chars @ [chr];
        })
    in
    let next_new_number__m =
      Option.both state.node.next_number__many digit
      |> Option.map ~f:(fun (node, digit) ->
        `keep_going { state with
          node;
          current_number = `many (Some digit);
          chars = state.chars @ [chr];
        })
    in
    let next_multiple_numeric_args_a =
      match state.current_number, chr with
      | `many (Some n), ';' ->
        Some (`keep_going {
          node = state.node;
          current_number = `many None;
          chars = state.chars @ [chr];
          stack = Some n :: state.stack;
        })
      | _, _ -> None
    in
    let next_multiple_numeric_args_b =
      match state.current_number, digit with
      | `many None, Some n ->
        Some (`keep_going {
          node = state.node;
          current_number = `many (Some n);
          chars = state.chars @ [chr];
          stack = state.stack;
        })
      | _, _ -> None
    in
    List.filter_opt [
      next_new_number__oo;
      next_new_number__m;
      next_by_char_skipping_number__oo;
      next_by_char_skipping_number__m;
      next_continuing_number;
      next_by_char;
      next_multiple_numeric_args_a;
      next_multiple_numeric_args_b;
    ]
  ;;

  let step states chr =
    assert (not (List.is_empty states));
    let next_states, final_states =
      List.concat_map states ~f:(fun state -> step_one state chr)
      |> List.partition_map ~f:(function
        | `keep_going x -> `Fst x
        | `ok x -> `Snd x)
    in
    match final_states, next_states with
    | [], [] ->
      (* CR datkin: Hoist [chars] out as it's the same across all states (i.e.
       * all branches start at the same input character). *)
      (* [chars] in all states should be the same, and [states] must be non
       * empty. *)
      let chars = (List.hd_exn states).chars @ [chr] in
      `no_match chars
    | [], _ :: _ -> `keep_going next_states
    | [ x ], _ -> `ok x
    | _ :: _ :: _, _ ->
      assert false (* Ambiguous *)

  let init spec =
    let parser =
      List.fold spec ~init:empty_node ~f:(fun parser (spec, fn) ->
        add parser spec fn)
    in
    init_state parser
  ;;

  let default = init Spec.default
  let xterm = init Spec.xterm
  let xterm_for_test = init Spec.xterm_for_test

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
            |> String.substr_replace_all ~pattern:"%p2" ~with_:""
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

let debug str =
  if false
  then printf "%s\n%!" str
  else ()
;;

type parse_result = [
  | `literal of char
  | `func of (t * string)
  | `junk of string
  | `pending
] [@@deriving sexp]

let parser init =
  debug (sprintf !"\n<starting> %{sexp:Parser.state}" init);
  let state = ref [init] in
  stage (fun chr ->
    match Parser.step !state chr with
    | `keep_going s ->
      debug (sprintf !"%c => %{sexp:Parser.state list}" chr s);
      state := s;
      `pending
    | `ok value ->
      debug (sprintf !"\n<reset> %{sexp:Parser.state}" init);
      state := [init];
      (`func value)
    | `no_match [] ->
      (* at least the char we just entered should be in the list *)
     assert false
    | `no_match [ chr ] -> state := [init]; `literal chr
    | `no_match chars ->
      state := [init];
      `junk (String.of_char_list chars))

let%test_unit _ =
  let f = unstage (parser Parser.default) in
  let strip (parse_result : parse_result) =
    match parse_result with
    | `func (f, _) -> `func f
    | `literal _
    | `pending
    | `junk _ as parse_result
    (*
    | `keep_going _
    *)
      -> parse_result
  in
  let test here chr expect =
    [%test_result: [`literal of char | `func of control_function | `junk of string | `pending]]
      ~here:[here]
      (f chr |> strip)
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
          (f chr |> strip)
          ~expect:(`func expect)
      | chr :: chrs ->
        [%test_result: [`literal of char | `func of control_function | `junk of string | `pending]]
          ~here:[here]
          (f chr |> strip)
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
    ])
    [%here]
    "\x1b[1;30r" (Other (["csr"], [Some 1; Some 30;]));
  test_seq
    ~p:(Parser.of_capabilities [
      Terminfo.parse_entry {|rmso=\E[27m|} |> Or_error.ok_exn;
    ])
    [%here]
    "\x1b[27m" (Other (["rmso"], []));
  test_seq
    ~p:(Parser.of_capabilities [
      Terminfo.parse_entry {|csr=\E[%i%p1%d;%p2%dr|} |> Or_error.ok_exn;
      Terminfo.parse_entry {|kHOM=\E[1;2H|} |> Or_error.ok_exn;
    ])
    [%here]
    "\x1b[1;2H" (Other (["kHOM"], []));
  test_seq
    ~p:Parser.xterm_for_test
    [%here]
    "\x1b[1;2H" (Other (["CUP"], [Some 1; Some 2]));
  test_seq
    ~p:Parser.xterm_for_test
    [%here]
    "\x1b[1;2;4m" (Other (["SGR"], [Some 1; Some 2; Some 4]));
  test_seq
    ~p:Parser.xterm_for_test
    [%here]
    "\x1b[m" (Other (["SGR"], []));
;;

open Async.Std

let parse reader init =
  let init = [init] in
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
    >>= fun (_ : Parser.state list) ->
    Deferred.unit)
