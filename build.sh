#!/bin/bash

set -o errexit
set -x

opam switch 4.03.0 && eval $(opam config env)

build_dir=.datkin-build

function get_modules_in_dep_order {
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
    odditty)
      cat <<EOF
        pty
        terminfo
EOF
        ;;
      *)
        echo "get_modules_in_dep_order err"
        exit 1
        ;;
    esac
}

function pack_name {
  case $1 in
    odditty_kernel)
      echo "Odditty_kernel"
      ;;
    odditty)
      echo "Odditty"
      ;;
    *)
      echo "pack_name err"
      exit 1
      ;;
  esac
}

function get_flags {
  lib=$1

  case ${lib} in
    odditty_kernel)
      echo "-package core_kernel -package async_kernel"
      ;;
    odditty)
      echo "-thread -package core -package async -I ${build_dir}/odditty_kernel/src"
      ;;
    *)
      echo "get_flags err"
      exit 1
      ;;
  esac
}

function compile_module {
  lib=$1
  mod=$2

  packages=$(get_flags ${lib})

  mkdir -p ${build_dir}/${lib}/src

  if [ -e ${lib}/${mod}.mli ]; then
    ppx-jane ${lib}/${mod}.mli > ${build_dir}/${lib}/src/${mod}.mli
    ocamlfind ocamlc ${packages} -I ${build_dir}/${lib}/src -c ${build_dir}/${lib}/src/${mod}.mli -o ${build_dir}/${lib}/src/${mod}.cmi
  else
    echo "skipping mli"
  fi

  ppx-jane -inline-test-lib ${lib} ${lib}/${mod}.ml > ${build_dir}/${lib}/src/${mod}.ml
  ocamlfind ocamlc   ${packages} -I ${build_dir}/${lib}/src -for-pack $(pack_name ${lib}) -c ${build_dir}/${lib}/src/${mod}.ml -o ${build_dir}/${lib}/src/${mod}.cmo
  ocamlfind ocamlopt ${packages} -I ${build_dir}/${lib}/src -for-pack $(pack_name ${lib}) -c ${build_dir}/${lib}/src/${mod}.ml -o ${build_dir}/${lib}/src/${mod}.cmx
}

function compile_clib {
  lib=$1

  mkdir -p ${build_dir}/${lib}/c

  if [ ! -e ${lib}/*_stubs.c ]; then
    echo "No c stubs for ${lib}"
    return
  fi

  for stub in ${lib}/*_stubs.c; do
    base=$(basename $stub | sed 's/\.c//')
    gcc -I $(ocamlc -where) -c ${lib}/${base}.c -o ${build_dir}/${lib}/c/${base}.o
  done

  ar cr ${build_dir}/${lib}/c/lib${lib}.a ${build_dir}/${lib}/c/*.o
}

function pack {
  lib=$1

  mkdir -p ${build_dir}/${lib}/lib

  mods=""
  for mod in $(get_modules_in_dep_order ${lib}); do
    mods="${mods} ${build_dir}/${lib}/src/${mod}.EXT"
  done

  cmo_mods=$(echo ${mods} | sed 's/\.EXT/.cmo/g')
  cmx_mods=$(echo ${mods} | sed 's/\.EXT/.cmx/g')

  #ocamlfind ocamlc   -pack -o ${build_dir}/${lib}/src/${lib}.cmo  $cmo_mods
  ocamlfind ocamlopt -pack -o ${build_dir}/${lib}/src/${lib}.cmx  $cmx_mods
  #ocamlfind ocamlc   -a    -o ${build_dir}/${lib}/lib/${lib}.cma  ${build_dir}/${lib}/src/${lib}.cmo
  ocamlfind ocamlopt -a    -o ${build_dir}/${lib}/lib/${lib}.cmxa ${build_dir}/${lib}/src/${lib}.cmx
}

function exe {
  file=$1

  dir=$(dirname ${file})
  base=$(basename ${file} | sed 's/\.ml$//')

  mkdir -p ${build_dir}/${dir}/src
  mkdir -p ${build_dir}/${dir}/exe

  case $1 in
    test/inline_test_runner.ml)
      shared_flags="-thread -package core -package async -package ppx_expect -package ppx_expect.evaluator"
      c_flags="${shared_flags} -I ${build_dir}/odditty_kernel/src                     -I ${build_dir}/odditty/src"
      l_flags="${shared_flags}    ${build_dir}/odditty_kernel/lib/odditty_kernel.cmxa    ${build_dir}/odditty/lib/odditty.cmxa -ccopt -L${build_dir}/odditty/c/ -cclib -lodditty"
      ;;
    *)
      echo "exe flags err"
      exit 1
      ;;
  esac

  ppx-jane ${file} > ${build_dir}/${dir}/src/${base}.ml
  ocamlfind ocamlopt ${c_flags} -c       ${build_dir}/${dir}/src/${base}.ml  -o ${build_dir}/${dir}/src/${base}.cmx
  ocamlfind ocamlopt ${l_flags} -linkall -linkpkg ${build_dir}/${dir}/src/${base}.cmx -o ${build_dir}/${dir}/exe/${base}.native
}

for lib in odditty_kernel odditty; do
  for mod in $(get_modules_in_dep_order ${lib}); do
    compile_module ${lib} ${mod}
  done
   compile_clib ${lib}

  pack ${lib}
done

exe test/inline_test_runner.ml

for lib in odditty_kernel odditty; do
  ./${build_dir}/test/exe/inline_test_runner.native \
    inline-test-runner \
    $lib \
    -verbose
done

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

echo done
