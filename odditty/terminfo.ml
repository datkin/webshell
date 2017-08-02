open Core
open Async

include Odditty_kernel.Terminfo

let load name =
  (* I haven't found a spec of any sort of the format of the terminfo "compiled"
   * db files. It seems the 'infocmp' program is probably the best way to parse
   * the compiled files. Interestingly the 'terminfo' man page describes the
   * output of 'infocmp' as though it's a spec for a file format, though it
   * sounds like terminfo db files are never stored in that format. *)
  Process.create ~prog:"infocmp" ~args:[name] ()
  >>=? fun proc ->
  Process.collect_output_and_wait proc
  >>= fun output ->
  match output.exit_status with
  | Ok () -> return (parse output.stdout)
  | Error _ ->
    return (Or_error.error "infocmp failed" (name, output) [%sexp_of: (string * Process.Output.t)])
