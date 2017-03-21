open! Core_kernel.Std
open! Async_kernel.Std

let () = Async_js.init ()

let () = don't_wait_for (Web.Main.run ())
