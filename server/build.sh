#!/bin/bash

opam switch 4.02.3 && eval $(opam config env)

#ocamlbuild -clean

#mkdir _build

#gcc -Wall -c server_stubs.c -g -O0 -I $(opam config var lib)/ocaml

#mv server_stubs.o _build/

  #-tag unix \
ocamlbuild \
  -use-ocamlfind \
  -pkg core \
  -pkg async \
  -tag thread \
  -cflags -cclib,-lserver_stubs \
  libserver_stubs.a \
  server.native
