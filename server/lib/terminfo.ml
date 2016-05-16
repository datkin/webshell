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
        '\000', '@';
        '\001', 'A';
        '\002', 'B';
        '\003', 'C';
        '\004', 'D';
        '\005', 'E';
        '\006', 'F';
        '\007', 'G';
        '\010', 'H';
        '\011', 'I';
        '\012', 'J';
        '\013', 'K';
        '\014', 'L';
        '\015', 'M';
        '\016', 'N';
        '\017', 'O';
        '\020', 'P';
        '\021', 'Q';
        '\022', 'R';
        '\023', 'S';
        '\024', 'T';
        '\025', 'U';
        '\026', 'V';
        '\027', 'W';
        '\030', 'X';
        '\031', 'Y';
        '\032', 'Z';
        '\033', '[';
        '\034', '\\';
        '\035', ']';
        (*'\036', '^';*)
        '\037', '_';
        '\127', '?';
      ]
    |> unstage
  in
  let unescape_slash =
    String.Escaping.unescape_gen_exn
      ~escape_char:'\\'
      ~escapeworthy_map:[
        (*'\033', 'e';*)
        '\033', 'E';
      ]
    |> unstage
  in
  fun data ->
    unescape_caret (unescape_slash data)

let%test_unit _ =
  [%test_result: string]
    (parse_str "\E^X")
    ~expect:"\033\030"
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
