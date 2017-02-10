include struct
  module X1 = Odditty
  module X2 = Odditty_kernel_x
end

let () =
  let open Ppx_inline_test_lib in
  Runtime.exit ();
;;
