module A = Odditty_kernel
module B = Odditty

let () =
  let open Ppx_inline_test_lib in
  Runtime.exit ();
;;
