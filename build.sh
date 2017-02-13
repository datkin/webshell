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
      web)
        cat <<EOF
        main
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
    web)
      echo "Web"
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
    web)
      echo "-package js_of_ocaml.async"
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

  p1="-ppx"
  p2="ppx-jane -as-ppx -inline-test-lib ${lib}"
  p3=""
  p4=""
  case ${lib} in
    web)
      p3="-ppx"
      p4="$(opam config var lib)/js_of_ocaml/ppx_js"
      ;;
    *)
      ;;
  esac

  if [ -e ${lib}/${mod}.mli ]; then
    cp ${lib}/${mod}.mli ${build_dir}/${lib}/src/${mod}.mli
    ocamlfind ocamlc $p1 "$p2" $p3 $p4 ${packages} -I ${build_dir}/${lib}/src -c ${build_dir}/${lib}/src/${mod}.mli -o ${build_dir}/${lib}/src/${mod}.cmi
  else
    echo "skipping mli"
  fi

  cp ${lib}/${mod}.ml ${build_dir}/${lib}/src/${mod}.ml
  ocamlfind ocamlc   $p1 "$p2" $p3 $p4 ${packages} -I ${build_dir}/${lib}/src -for-pack $(pack_name ${lib}) -c ${build_dir}/${lib}/src/${mod}.ml -o ${build_dir}/${lib}/src/${mod}.cmo
  ocamlfind ocamlopt $p1 "$p2" $p3 $p4 ${packages} -I ${build_dir}/${lib}/src -for-pack $(pack_name ${lib}) -c ${build_dir}/${lib}/src/${mod}.ml -o ${build_dir}/${lib}/src/${mod}.cmx
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

  # CR datkin: It's presumably bad to include the '-L' option here?
  # An equally non-kosher option: we can omit these flags, and just pass
  # ${clib} in the list of libs to include in the archive (I think this
  # approach only references the lib by name, though).
  clib=${build_dir}/${lib}/c/lib${lib}.a
  if [ -e ${clib} ]; then
    ccopts="-ccopt -L${build_dir}/${lib}/c -cclib -l${lib}"
  else
    ccopts=""
  fi

  cmo_mods=$(echo ${mods} | sed 's/\.EXT/.cmo/g')
  cmx_mods=$(echo ${mods} | sed 's/\.EXT/.cmx/g')

  ocamlfind ocamlc   -pack        -o ${build_dir}/${lib}/src/${lib}.cmo  $cmo_mods
  ocamlfind ocamlopt -pack        -o ${build_dir}/${lib}/src/${lib}.cmx  $cmx_mods
  ocamlfind ocamlc   -a ${ccopts} -o ${build_dir}/${lib}/lib/${lib}.cma  ${build_dir}/${lib}/src/${lib}.cmo
  ocamlfind ocamlopt -a ${ccopts} -o ${build_dir}/${lib}/lib/${lib}.cmxa ${build_dir}/${lib}/src/${lib}.cmx
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
      # We pass '-linkall' b/c it ensures that the inline test runner will be
      # linked against the inline test libs, even though it doesn't actually
      # reference them.
      l_flags="${shared_flags}    ${build_dir}/odditty_kernel/lib/odditty_kernel.cmxa    ${build_dir}/odditty/lib/odditty.cmxa -linkall"
      ;;
    bin/main.ml)
      shared_flags="-thread -package core -package async"
      c_flags="${shared_flags} -I ${build_dir}/odditty_kernel/src                     -I ${build_dir}/odditty/src"
      l_flags="${shared_flags}    ${build_dir}/odditty_kernel/lib/odditty_kernel.cmxa    ${build_dir}/odditty/lib/odditty.cmxa"
      ;;
    *)
      echo "exe flags err"
      exit 1
      ;;
  esac

  ppx-jane ${file} > ${build_dir}/${dir}/src/${base}.ml
  ocamlfind ocamlopt ${c_flags} -c       ${build_dir}/${dir}/src/${base}.ml  -o ${build_dir}/${dir}/src/${base}.cmx
  ocamlfind ocamlopt ${l_flags} -linkpkg ${build_dir}/${dir}/src/${base}.cmx -o ${build_dir}/${dir}/exe/${base}.native
}

function skip {
for lib in odditty_kernel odditty; do
  for mod in $(get_modules_in_dep_order ${lib}); do
    compile_module ${lib} ${mod}
  done
   compile_clib ${lib}

  pack ${lib}
done

exe test/inline_test_runner.ml
exe bin/main.ml

for lib in core_kernel odditty_kernel odditty; do
  ./${build_dir}/test/exe/inline_test_runner.native \
    inline-test-runner \
    $lib \
    -verbose
done

${build_dir}/bin/exe/main.native -help
}

# = Notes on building the js side =
#
# `4.03.0+for-js` is an installation of 4.03.0+32bit-natdynlink, an opam compiler switch defined at:
# https://github.com/datkin/local-opam-repository.
#
# It installs (in the following order):
#  async_kernel
#  camlp4 (b/c js_of_ocaml seems to require it)
#  js_of_ocaml
#  async_js

opam switch 4.03.0+for-js && eval $(opam config env)

build_dir=.datkin-js-build

for lib in odditty_kernel web; do
  for mod in $(get_modules_in_dep_order ${lib}); do
    compile_module ${lib} ${mod}
  done

  pack ${lib}
done

exit
