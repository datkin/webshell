open Ocamlbuild_plugin

let () =
  dispatch (function
    | After_rules ->
      dep [ "link"; "ocaml"; "odditty-stubs" ] [ "odditty/libodditty_stubs.a" ];
      flag [ "ocaml"; "link"; "odditty-stubs" ] (A "odditty/libodditty_stubs.a" );
      flag [ "ocaml"; "link"; "library"; "native"; "odditty-stubs-x"; ] (S [A "-cclib"; A "-lodditty_stubs"])
    | _ -> ()
  )
