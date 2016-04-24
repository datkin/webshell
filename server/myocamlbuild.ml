open Ocamlbuild_plugin

let () =
  dispatch (function
    | After_rules ->
      dep [ "link"; "ocaml"; ] [ "lib/libserver_stubs.a" ];
      flag [ "ocaml"; "link"; ] (A "lib/libserver_stubs.a" );
      flag [ "ocaml"; "link"; "library"; "native" ]
        (S [A "-cclib"; A "-lserver_stubs"])
    | _ -> ()
  )
