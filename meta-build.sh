#!/bin/bash

set -o errexit
set -x

opam switch 4.03.0 && eval $(opam config env)

# All warnings, disable:
#   - 40: Constructor / label name out of scope
#   - 42: Disambiguated constructor
#   - 44: `open` shadows an identifier
warnings=+a-40-42-44

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
  dbuild.ml \
  -o dbuild

(TEST= ./dbuild inline-test-runner dbuild -verbose)
