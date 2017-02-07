include struct
  include Odditty
end

let () =
  let open Ppx_inline_test_lib in
  Runtime.exit ();
;;
