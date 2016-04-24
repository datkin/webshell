(* Depend on the tests. *)
let () = let open Server_lib in ()

let () =
  let open Ppx_inline_test_lib in
  Runtime.summarize () |> Runtime.Test_result.record;
  Runtime.Test_result.exit ();
;;
