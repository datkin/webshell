#!/bin/bash

set -o errexit

opam switch 4.02.3 && eval $(opam config env)

ocamlbuild \
  -use-ocamlfind \
  -pkg core \
  -pkg async \
  -tag thread \
  -cflags -cclib,-lserver_stubs \
  -tag 'ppx(ppx-jane -as-ppx -inline-test-lib window)' \
  -cflags -w,-40 \
  window.native \
  server.native

./window.native \
  inline-test-runner \
  window \
  -show-counts \
  -verbose

echo done
