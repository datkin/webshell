#!/bin/bash

set -o errexit

opam switch 4.02.3 && eval $(opam config env)

ocamlbuild \
  -use-ocamlfind \
  -pkg core \
  -pkg async \
  -tag thread \
  -cflags -cclib,-lserver_stubs \
  -tag 'ppx(ppx-jane -as-ppx -inline-test-lib server_lib)' \
  -cflags -w,-40 \
  -verbose 3 \
  lib/inline_test_runner.native

./inline_test_runner.native \
  inline-test-runner \
  server_lib \
  -list-test-names \
  -show-counts

echo done
