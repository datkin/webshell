open Core
open Async

include module type of struct include Odditty_kernel.Terminfo end

val load : string -> t Or_error.t Deferred.t
