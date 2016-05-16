open Core.Std
open Async.Std

type value =
  | Bool of bool (* True if present? *)
  | Number of int
  | String of string
[@@deriving sexp]

type t = {
  names : string * string list;
  capabilities : value String.Map.t;
} [@@deriving sexp]

let capabilities t = t.capabilities

let null ~name = {
  names = name, [];
  capabilities = String.Map.empty;
}

let add_alias t alias =
  let name, aliases = t.names in
  { t with names = name, alias :: aliases }

let add t key data = { t with capabilities = Map.add t.capabilities ~key ~data; }

let parse_str =
  let unescape_caret =
    String.Escaping.unescape_gen_exn
      ~escape_char:'^'
      ~escapeworthy_map:[
        (* From Wikipedia's "ASCII control code chart" *)
        '\x00', '@';
        '\x01', 'A';
        '\x02', 'B';
        '\x03', 'C';
        '\x04', 'D';
        '\x05', 'E';
        '\x06', 'F';
        '\x07', 'G';
        '\x08', 'H';
        '\x09', 'I';
        '\x0A', 'J';
        '\x0B', 'K';
        '\x0C', 'L';
        '\x0D', 'M';
        '\x0E', 'N';
        '\x0F', 'O';
        '\x10', 'P';
        '\x11', 'Q';
        '\x12', 'R';
        '\x13', 'S';
        '\x14', 'T';
        '\x15', 'U';
        '\x16', 'V';
        '\x17', 'W';
        '\x18', 'X';
        '\x19', 'Y';
        '\x1A', 'Z';
        '\x1B', '[';
        '\x1C', '\134'; (* \134 = \ *)
        '\x1D', ']';
        (*'\x1E', '^';*)
        '\x1F', '_';
        '\x7F', '?';
      ]
    |> unstage
  in
  let unescape_slash =
    String.Escaping.unescape_gen_exn
      ~escape_char:'\\' (* \x5c = \ *)
      ~escapeworthy_map:[
        (*'\033', 'e';*)
        '\x1b', 'E';
      ]
    |> unstage
  in
  fun data ->
    unescape_caret (unescape_slash data)

let%test_unit _ =
  [%test_result: string]
    (parse_str "\x5cE^X") (* Literally "\E^X" *)
    ~expect:"\x1b\x18";
  [%test_result: string]
    (parse_str "\x5cE[?1049h")
        ~expect:"\x1b[?1049h";
;;

let parse_entry entry =
  match String.lsplit2 entry ~on:'=' with
  | Some (key, value) -> Ok (key, String (parse_str value))
  | None ->
    match String.lsplit2 entry ~on:'#' with
    | Some (key, value) ->
      Or_error.try_with_join (fun () ->
        Ok (key, Number (Int.of_string value)))
    | None -> Ok (entry, Bool true)
;;

(* CR datkin: Mappings to perform:
  * \E and \e -> 
  * \, and \054 -> ,
  * any octal \<NNN> seq
  *)

let parse raw =
  let db =
    (* First, sanitize by stripping comments and extra padding. *)
    String.split raw ~on:'\n'
    |> List.filter ~f:(fun line -> not (String.is_prefix ~prefix:"#" line))
    |> List.map ~f:String.strip
    |> String.concat ~sep:" "
  in
  (* CR datkin: Stripping another other than a leading space on the front may be
   * too much. *)
  match
    String.Escaping.split db ~escape_char:'\\' ~on:','
    |> List.map ~f:String.strip
    |> List.filter ~f:(fun x -> not (String.is_empty x))
  with
  | [] -> Or_error.error "No names" raw sexp_of_string
  | names :: entries ->
    match String.split names ~on:'|' with
    | [] -> Or_error.error "No name" raw sexp_of_string
    | name :: aliases ->
      let init = null ~name in
      let t = List.fold aliases ~init ~f:add_alias in
      with_return (fun { return } ->
        let t =
          List.fold entries ~init:t ~f:(fun t entry ->
            match parse_entry entry with
            | Error _ as err -> return err
            | Ok ("acsc", _) (* This cap is different. *)
            | Ok ("rmacs", _)
            | Ok ("home", _) -> t (* Hack for now, b/c home and clear overlap in xterm *)
            | Ok (key, data) -> add t key data)
        in
        Ok t)
;;

let load name =
  (* I haven't found a spec of any sort of the format of the terminfo "compiled"
   * db files. It seems the 'infocmp' program is probably the best way to parse
   * the compiled files. Interestingly the 'terminfo' man page describes the
   * output of 'infocmp' as though it's a spec for a file format, though it
   * sounds like terminfo db files are never stored in that format. *)
  Process.create ~prog:"infocmp" ~args:[name] ()
  >>=? fun proc ->
  Process.collect_output_and_wait proc
  >>= fun output ->
  match output.exit_status with
  | Ok () -> return (parse output.stdout)
  | Error _ ->
    return (Or_error.error "infocmp failed" (name, output) [%sexp_of: (string * Process.Output.t)])
