include struct
  include Odditty_kernel
end

let () =
  let open Ppx_inline_test_lib in
  Runtime.exit ();
;;
