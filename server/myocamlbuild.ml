open Ocamlbuild_plugin

let () =
  dispatch (function
    | After_rules ->
      (*dep [ "compile"; "ocaml"; "inline_test_runner.cmo" ] [ "lib/server_lib.cmxa" ];*)
      (*dep [ "link"; "ocaml"; "bin/main.native" ] [ "lib/server_lib.cmxa" ];*)
      dep [ "link"; "ocaml"; ] [ "lib/libserver_stubs.a" ];
      flag [ "ocaml"; "link"; ] (A "lib/libserver_stubs.a" );
      flag [ "ocaml"; "link"; "library"; "native" ]
        (S [A "-cclib"; A "-lserver_stubs"])
    | _ -> ()
  )
