include struct
  (* Depend on the tests. *)
  open! Server_lib
end

let () =
  let open Ppx_inline_test_lib in
  (*
  Runtime.summarize () |> Runtime.Test_result.to_string |> print_endline;
  *)
  Runtime.exit ();
;;
