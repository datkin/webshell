#!/bin/bash

set -o errexit
set -x

opam switch 4.03.0 && eval $(opam config env)

ocamlbuild \
  -use-ocamlfind \
  -I odditty_kernel \
  -pkg core_kernel \
  -pkg async_kernel \
  -pkg ppx_expect \
  -pkg ppx_expect.evaluator \
  -tag thread \
  -tag 'ppx(ppx-jane -as-ppx -inline-test-lib odditty_kernel)' \
  -cflags -w,+a-40-42-44 \
  odditty_kernel_lib/odditty_kernel.cmxa

# -I odditty_kernel \

ocamlbuild \
  -use-ocamlfind \
  -I odditty \
  -I odditty_kernel_lib \
  -pkg core \
  -pkg async \
  -pkg ppx_expect \
  -pkg ppx_expect.evaluator \
  -tag thread \
  -tag 'ppx(ppx-jane -as-ppx -inline-test-lib odditty)' \
  -cflags -w,+a-40-42-44 \
  -cflags -cclib,-lodditty_stubs \
  -verbose 1 \
  odditty.cmxa

# -I odditty_kernel_lib \
# -I odditty \
ocamlbuild \
  -use-ocamlfind \
  -mod Odditty_kernel_x \
  -mod Odditty \
  -pkg core \
  -pkg async \
  -pkg ppx_expect \
  -pkg ppx_expect.evaluator \
  -tag thread \
  -tag 'ppx(ppx-jane -as-ppx -inline-test-lib odditty)' \
  -cflags -w,+a-40-42-44 \
  test/inline_test_runner.native

for lib in odditty odditty_kernel; do
  ./inline_test_runner.native \
    inline-test-runner \
    $lib \
    -verbose
done

echo done
