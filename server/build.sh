#!/bin/bash

set -o errexit

opam switch 4.03.0 && eval $(opam config env)

# CR datkin: Pass different ppx flags for inline test runner and main.
ocamlbuild \
  -use-ocamlfind \
  -pkg core \
  -pkg async \
  -pkg ppx_expect \
  -pkg ppx_expect.evaluator \
  -tag thread \
  -cflags -cclib,-lserver_stubs \
  -tag 'ppx(ppx-jane -as-ppx -inline-test-lib server_lib)' \
  -cflags -w,+a-40-42-44 \
  lib/server_lib.cmxa \
  bin/inline_test_runner.native \
  bin/main.native

./inline_test_runner.native \
  inline-test-runner \
  server_lib \
  -verbose

echo done
