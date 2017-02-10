#!/bin/bash

set -o errexit
set -x

opam switch 4.03.0 && eval $(opam config env)

build_dir=.datkin-build

function c {
  dir=$1
  mod=$2

  mkdir -p ${build_dir}/${dir}/

  case $dir in
    odditty_kernel)
      packages="-package core_kernel -package async_kernel"
      ;;
    *)
      echo "Hmm"
      exit
      ;;
  esac

  if [ -e ${dir}/${mod}.mli ]; then
    ppx-jane ${dir}/${mod}.mli > ${build_dir}/${dir}/${mod}.mli
    ocamlfind ocamlc ${packages} -I ${build_dir}/${dir} -c ${build_dir}/${dir}/${mod}.mli -o ${build_dir}/${dir}/${mod}.cmi
  else
    #ocamlfind ocamlc -package core_kernel -i ${build_dir}/${dir}/${mod}.ml > ${build_dir}/${dir}/${mod}.mli
    echo "skipping mli"
  fi

  #ocamlfind ocamlc -ppx 'ppx-jane -as-ppx -inline-test-lib odditty_kernel' -package core_kernel -c ${dir}/${mod}.ml  -o ${build_dir}/${dir}/${mod}.cmo
  ppx-jane -inline-test-lib odditty-kernel ${dir}/${mod}.ml > ${build_dir}/${dir}/${mod}.ml
  ocamlfind ocamlc ${packages} -I ${build_dir}/${dir} -c ${build_dir}/${dir}/${mod}.ml  -o ${build_dir}/${dir}/${mod}.cmo
}

c odditty_kernel character_attributes
c odditty_kernel character_set
c odditty_kernel terminfo
c odditty_kernel dec_private_mode
c odditty_kernel control_functions
c odditty_kernel window

exit

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
  -mod Odditty_kernel \
  -mod Odditty_kernel_x \
  -pkg core \
  -pkg async \
  -pkg ppx_expect \
  -pkg ppx_expect.evaluator \
  -tag thread \
  -tag odditty-stubs \
  -tag 'ppx(ppx-jane -as-ppx -inline-test-lib odditty)' \
  -cflags -w,+a-40-42-44 \
  -cflags -cclib,-lodditty_stubs \
  -verbose 1 \
  odditty.cmxa

# -I odditty_kernel_lib \
# -I odditty \
ocamlbuild \
  -use-ocamlfind \
  -lib Odditty_kernel \
  -lib Odditty \
  -pkg core \
  -pkg async \
  -pkg ppx_expect \
  -pkg ppx_expect.evaluator \
  -tag thread \
  -tag odditty-stubs-x \
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
