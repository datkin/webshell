#!/bin/bash

opam switch 4.02.3 && eval $(opam config env)

ocamlbuild \
  -use-ocamlfind \
  -pkg core \
  -pkg async \
  -tag thread \
  -cflags -cclib,-lserver_stubs \
  server.native
