module A = Odditty_kernel
(*
module B = Odditty
*)

(* Run with, e.g.:
  * (eval $(opam config env --switch 4.03.0);
  * ./.dbuild/native/bin/linked/inline_test_runner.native inline-test-runner
  * odditty_kernel -verbose)
  * *)

(* CR datkin: Use `-linkall` when compiling unit tests to avoid the need for a
 * random reference like this. *)
let _ = Odditty_kernel.Window.render

let () =
  let open Ppx_inline_test_lib in
  Runtime.exit ();
;;
