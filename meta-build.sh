#!/bin/bash

set -o errexit
set -x

eval $(opam config env --switch 4.03.0)

# Start with all warnings, then disable:
#   - 40: Constructor / label name out of scope
#   - 42: Disambiguated constructor
#   - 44: `open` shadows an identifier
#   - 48: implicit elimination of optional arg
warnings=+a-4-40-42-44-48

ocamlfind ocamlopt \
  -linkpkg \
  -verbose \
  -w ${warnings} \
  -thread \
  -package core \
  -package async \
  -package ppx_inline_test.runner.lib \
  -package ppx_expect.evaluator \
  -ppx 'ppx-jane -as-ppx -inline-test-lib dbuild' \
  dbuild.mli dbuild.ml \
  -o dbuild

(TEST= ./dbuild inline-test-runner dbuild -verbose)
