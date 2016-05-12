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

let null ~name = {
  names = name, [];
  capabilities = String.Map.empty;
}

let add_alias t alias =
  let name, aliases = t.names in
  { t with names = name, alias :: aliases }

let add t key data = { t with capabilities = Map.add t.capabilities ~key ~data; }

let parse_entry entry =
  match String.lsplit2 entry ~on:'=' with
  | Some (key, value) -> Ok (key, String value)
  | None ->
    match String.lsplit2 entry ~on:'#' with
    | Some (key, value) ->
      Or_error.try_with_join (fun () ->
        Ok (key, Number (Int.of_string value)))
    | None -> Ok (entry, Bool true)
;;

let parse raw =
  let db =
    (* First, sanitize by stripping comments and extra padding. *)
    String.split raw ~on:'\n'
    |> List.filter ~f:(fun line -> not (String.is_prefix ~prefix:"#" line))
    |> List.map ~f:String.strip
    |> String.concat ~sep:" "
  in
  (* CR datkin: Literal ','s are either '\,' or the octal escape sequence, \054. *)
  (* CR datkin: Stripping another oter than a leading space on the front may be
   * too much. *)
  match
    String.split db ~on:','
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
            | Ok (key, data) -> add t key data)
        in
        Ok t)
;;

let load name =
  Process.create ~prog:"infocmp" ~args:[name] ()
  >>=? fun proc ->
  Process.collect_output_and_wait proc
  >>= fun output ->
  match output.exit_status with
  | Ok () -> return (parse output.stdout)
  | Error _ ->
    return (Or_error.error "infocmp failed" (name, output) [%sexp_of: (string * Process.Output.t)])
