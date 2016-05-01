open Core.Std

type parser =
  | Constant of string
  | Number

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

let c str = Constant str
let csi = c "\x1b["
let n = Number

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

let _spec = [
  [c "\x06"], s Ack;
  [c "\x07"], s Bell;
  [csi; n; c "@"], n' (fun x -> Insert_blank n) 1;
  [csi; n; c "A"], n' (fun x -> Cursor (Up, n)) 1;
  [csi; n; c "B"], n' (fun x -> Cursor (Down, n)) 1;
  [csi; n; c "C"], n' (fun x -> Cursor (Left, n)) 1;
  [csi; n; c "D"], n' (fun x -> Cursor (Right, n)) 1;
]

let parse _ = `need_more
