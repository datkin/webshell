open Ocamlbuild_plugin

let () =
  dispatch (function
    | After_rules ->
      dep [ "link"; "ocaml"; ] [ "libserver_stubs.a" ];
    | _ -> ()
  )
