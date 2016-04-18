#!/bin/bash

ocamlbuild -clean

mkdir _build

gcc -Wall -c server_stubs.c -g -O0 -I $(opam config var lib)/ocaml

mv server_stubs.o _build/

ocamlbuild -use-ocamlfind -pkg core -pkg async server.native -tag thread -tag unix -lflag server_stubs.o -verbose 1
