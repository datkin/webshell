include struct
  (* Depend on the tests. *)
  include Server_lib.Control_functions
end

let () =
  let open Ppx_inline_test_lib in
  Runtime.exit ();
;;
