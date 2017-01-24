open! Core_kernel.Std

type t =
  | DEC_special_character_set
  | UK
  | USASCII
  | Dutch
  | Finnish
  | French
  | French_canadian
  | German
  | Italian
  | Norwegian_danish
  | Spanish
  | Swedish
  | Swiss
  [@@deriving sexp, compare, enumerate]

let vt100_codes = function
  | DEC_special_character_set -> [ '0' ]
  | UK -> [ 'A' ]
  | USASCII -> [ 'B' ]
  | Dutch -> [ '4' ]
  | Finnish -> [ 'C' ; '5' ]
  | French -> [ 'R' ]
  | French_canadian -> [ 'Q' ]
  | German -> [ 'K' ]
  | Italian -> [ 'Y' ]
  | Norwegian_danish -> [ 'E' ; '6' ]
  | Spanish -> [ 'Z' ]
  | Swedish -> [ 'H' ; '7' ]
  | Swiss -> [ '=' ]
;;

let by_vt100_code =
  List.concat_map all ~f:(fun t ->
    List.map (vt100_codes t) ~f:(fun char -> (char, t)))
  |> Char.Map.of_alist_exn
;;
