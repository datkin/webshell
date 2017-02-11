#!/bin/bash

set -o errexit
set -x

opam switch 4.03.0 && eval $(opam config env)

build_dir=.datkin-build

function get_modules {
  lib=$1

  case ${lib} in
    odditty_kernel)
      cat <<EOF
        character_attributes
        character_set
        terminfo
        dec_private_mode
        control_functions
        window
EOF
        ;;
      *)
        echo "Hrmm"
        exit
        ;;
    esac
}

function pack_name {
  case $1 in
    odditty_kernel)
      echo "Odditty_kernel"
      ;;
    *)
      echo "pack_name err"
      exit
      ;;
  esac
}

function get_packages {
  lib=$1

  case ${lib} in
    odditty_kernel)
      echo "-package core_kernel -package async_kernel"
      ;;
    *)
      echo "get_packages err"
      exit
      ;;
  esac
}

function c {
  lib=$1
  mod=$2

  packages=$(get_packages ${lib})

  mkdir -p ${build_dir}/${lib}/

  if [ -e ${lib}/${mod}.mli ]; then
    ppx-jane ${lib}/${mod}.mli > ${build_dir}/${lib}/${mod}.mli
    ocamlfind ocamlc ${packages} -I ${build_dir}/${lib} -c ${build_dir}/${lib}/${mod}.mli -o ${build_dir}/${lib}/${mod}.cmi
  else
    echo "skipping mli"
  fi

  ppx-jane -inline-test-lib odditty-kernel ${lib}/${mod}.ml > ${build_dir}/${lib}/${mod}.ml
  ocamlfind ocamlc   ${packages} -I ${build_dir}/${lib} -for-pack $(pack_name ${lib}) -c ${build_dir}/${lib}/${mod}.ml -o ${build_dir}/${lib}/${mod}.cmo
  ocamlfind ocamlopt ${packages} -I ${build_dir}/${lib} -for-pack $(pack_name ${lib}) -c ${build_dir}/${lib}/${mod}.ml -o ${build_dir}/${lib}/${mod}.cmx
}

function pack {
  lib=$1

  mkdir -p ${build_dir}/libs/

  mods=""
  for mod in $(get_modules ${lib}); do
    mods="${mods} ${build_dir}/${lib}/${mod}.EXT"
  done

  cmo_mods=$(echo ${mods} | sed 's/\.EXT/.cmo/g')
  cmx_mods=$(echo ${mods} | sed 's/\.EXT/.cmx/g')

  ocamlfind ocamlc   -pack -o ${build_dir}/${lib}.cmo $cmo_mods
  ocamlfind ocamlopt -pack -o ${build_dir}/${lib}.cmx $cmx_mods
  ocamlfind ocamlc   -a    -o ${build_dir}/libs/${lib}.cma  ${build_dir}/${lib}.cmo
  ocamlfind ocamlopt -a    -o ${build_dir}/libs/${lib}.cmxa ${build_dir}/${lib}.cmx
}

for lib in odditty_kernel; do
  for mod in $(get_modules odditty_kernel); do
    c ${lib} ${mod}
  done

  pack ${lib}
done

exit

pack odditty_kernel

c odditty pty
c odditty terminfo

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
