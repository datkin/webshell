#!/bin/bash

set -o errexit

opam switch 4.03.0 && eval $(opam config env)

# CR datkin: Pass different ppx flags for inline test runner and main.
ocamlbuild \
  -I odditty_kernel \
  -use-ocamlfind \
  -pkg core_kernel \
  -pkg async_kernel \
  -pkg ppx_expect \
  -pkg ppx_expect.evaluator \
  -tag thread \
 -tag 'ppx(ppx-jane -as-ppx -inline-test-lib odditty_kernel)' \
  -cflags -w,+a-40-42-44 \
  odditty_kernel.cmx


echo done
