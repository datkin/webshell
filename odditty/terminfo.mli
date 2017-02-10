open Core.Std
open Async.Std

include module type of struct include Odditty_kernel_x.Terminfo end

val load : string -> t Or_error.t Deferred.t
