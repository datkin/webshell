# webshell

The idea is to produce a web-based shell environment, using js-of-ocaml. The
goal is to explore options for interacting with a system without the need for
traditional a traditional terminal-based shell.

However, for backward compatibility there will need to be support for both. So
the first step will be a terminal emulator.

A lot of interesting shell "add ons" can be supported by the terminal emulator,
too. For instance, iTerm2 apparently does lots of [cool
things](https://iterm2.com/features.html) The history information it collects
seems especially interesting.

## Building

The project uses it's own "simple" (single file) build system, `dbuild`,
written in ocaml. `dbuild` depends on:
- ocamlfind
- opam
- core
- async

To build `dbuild`:

```bash
  $ ./meta-build.sh
```

An example usage of `dbuild` to build some of the project:

```bash
  $ rm -rf .dbuild-sandbox/ \
    && ./dbuild \
         parallel-build \
         -poll \
         -sandbox \
         -spec .dbuild-spec \
         .dbuild/js/bin/linked/web_main.js-linked \
         .dbuild/native/bin/linked/{main,inline_test_runner}.native \
         .dbuild/native/odditty_kernel/linked/inline_test_runner.native \
         2>&1 \
    | tee /tmp/dbuild.log
```

Run tests with:

```bash
(eval $(opam config env --switch 4.03.0); .dbuild/native/odditty_kernel/linked/inline_test_runner.native inline-test-runner odditty_kernel -verbose)
```

Or, using `jbuilder`:

```bash
$ jbuilder build bin/main.exe bin/inline_test_runner.exe bin-js/web_main.bc.js --dev
```

And to run tests:

```bash
$./_build/default/bin/inline_test_runner.exe inline-test-runner odditty_kernel -verbose
```

Features of `dbuild`:
- Parallel builds. ocamlbuild has
  [issues](https://caml.inria.fr/mantis/view.php?id=5754) with this,
  apparently.
- Incremental rebuilds.
- Polling. For "interactive" development.
- [Sandboxing](https://github.com/janestreet/jenga/blob/master/lib/sandbox.mli).
  A clever idea from folks at Jane Street to ensure dependencies are defined
  correctly.
- Uses the [module
  aliases](https://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec235)
  approach to building libraries (aka
  [namespaces](https://blogs.janestreet.com/better-namespaces-through-module-aliases/)).
  This improves build parallelism and executable size.  I'm unsure if this is
  easy to do with ocamlbuild. But Jane Street's
  [jenga](https://github.com/janestreet/jenga-rules/blob/master/lib/root.ml) and
  [jbuilder](https://github.com/janestreet/jbuilder/blob/master/src/gen_rules.ml)
  do this.
- Multiple compilers support. Necessary to build native x86-64 and 32-bit
  javascript targets.
- Simplicity. It fits in a single file, so it hopefully it's easy to understand
  and easy to extend and adapt. It has fewer dependencies than
  [jenga](https://github.com/janestreet/jenga).

On the other hand it has some glaring deficiencies:
- Dynamic build rules aren't supported. So when the module dependencies of a
  file change you have to restart the build system.
- It's a giant hack job.

## Terminal Emulation

To run the emulator as a web server,

```
.dbuild/native/bin/linked/main.native server run -ws-port 8081
```

Then, open `bin/index.html` in a browser to attach to the emulator.

user hits keys -> os(?) -> terminal (emulator) -> application

application -> terminal (emulator) -> screen

- sometimes the application sends characters to the terminal emulator which are just
  instructions to be interpreted by the emulator, e.g.
   - how to render the screen
   - how to translate key presses into chars interpretable by the application
   - queries to the emulator (e.g., send me the terminal ID)

- the emulator can send more than just keys to the application's stdin:
   - mouse clicks
   - cursor positions
