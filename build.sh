#!/bin/bash

gcc -Wall -c server_stubs.c -g -O0 -I $(opam config var lib)/ocaml
