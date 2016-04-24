open Ocamlbuild_plugin

let () =
  dispatch (function
    | After_rules ->
      dep [ "compile"; "ocaml"; "inline_test_runner.cmo" ] [ "lib/server_lib.cmxa" ];
      dep [ "link"; "ocaml"; ] [ "libserver_stubs.a" ];
    | _ -> ()
  )
