open! Core_kernel
open! Async_kernel

let () = Async_js.init ()

let () = don't_wait_for (Web.Main.run ())
