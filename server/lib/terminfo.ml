open Core.Std

type t = {
  x : unit;
} [@@deriving sexp_of]

let load file : t = assert false
