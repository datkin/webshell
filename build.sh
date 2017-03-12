#!/bin/bash

# CR datkin: "Features to add"
#  - use ocamlfind for packages
#  - add caching logic (only rebuild changed files)
#  - port to ocaml (aka jenga :/)

set -o errexit
set -x

warnings=+a-40-42-44

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
      server)
        cat <<EOF
        web_server
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
    server)
      echo "Server"
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
      # Another option is "js_of_ocaml.async"?
      echo "-package js_of_ocaml -package js_of_ocaml.async -package async_js -package virtual_dom"
      ;;
    server)
      echo "-thread -package async -package websocket.async"
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

  # CR datkin: Ordering of the ppx extensions matters here? I'm unsure why/how. Perhaps the ppx-jane expander raises "untranslated extension" errors?
  if [ -e ${lib}/${mod}.mli ]; then
    cp ${lib}/${mod}.mli ${build_dir}/${lib}/src/${mod}.mli
    ocamlfind ocamlc -g $p3 $p4 $p1 "$p2" ${packages} -I ${build_dir}/${lib}/src -c ${build_dir}/${lib}/src/${mod}.mli -o ${build_dir}/${lib}/src/${mod}.cmi
  else
    echo "skipping mli"
  fi

  cp ${lib}/${mod}.ml ${build_dir}/${lib}/src/${mod}.ml
  ocamlfind ocamlc   -w ${warnings} -g $p3 $p4 $p1 "$p2" ${packages} -I ${build_dir}/${lib}/src -for-pack $(pack_name ${lib}) -c ${build_dir}/${lib}/src/${mod}.ml -o ${build_dir}/${lib}/src/${mod}.cmo
  ocamlfind ocamlopt -w ${warnings} -g $p3 $p4 $p1 "$p2" ${packages} -I ${build_dir}/${lib}/src -for-pack $(pack_name ${lib}) -c ${build_dir}/${lib}/src/${mod}.ml -o ${build_dir}/${lib}/src/${mod}.cmx
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

  case $file in
    test/inline_test_runner.ml)
      # ppx_inline_test.runner.lib introduces a new defn to the external
      # [Base_am_testing] function, which is required for tests to run.
      shared_flags="-thread -package core -package async -package ppx_expect -package ppx_inline_test.runner.lib -package ppx_expect.evaluator"
       c_flags="${shared_flags} -I ${build_dir}/odditty_kernel/src                     -I ${build_dir}/odditty/src"
      # We pass '-linkall' b/c it ensures that the inline test runner will be
      # linked against the inline test libs, even though it doesn't actually
      # reference them.
      ln_flags="${shared_flags}    ${build_dir}/odditty_kernel/lib/odditty_kernel.cmxa    ${build_dir}/odditty/lib/odditty.cmxa"
      ;;
    bin/main.ml)
      shared_flags="-thread -package core -package async -package websocket.async"
       c_flags="${shared_flags} -I ${build_dir}/odditty_kernel/src                     -I ${build_dir}/odditty/src              -I ${build_dir}/server/src"
      ln_flags="${shared_flags}    ${build_dir}/odditty_kernel/lib/odditty_kernel.cmxa    ${build_dir}/odditty/lib/odditty.cmxa    ${build_dir}/server/lib/server.cmxa"
      ;;
    bin/web_main.ml)
      shared_flags="-thread -package js_of_ocaml.async -package core_kernel -package async_kernel -package js_of_ocaml -package virtual_dom"
       c_flags="${shared_flags} -I ${build_dir}/web/src"
      ln_flags="${shared_flags}    ${build_dir}/web/lib/web.cmxa"
      ;;
    *)
      echo "exe flags err"
      exit 1
      ;;
  esac

  lb_flags=$(echo "$ln_flags" | sed 's/\.cmxa/.cma/g')

  ppx-jane ${file} > ${build_dir}/${dir}/src/${base}.ml

  # Can't link fork_in_pty for the bytecode? Not sure why.
  if [ $file == "bin/web_main.ml" ]; then
    ocamlfind ocamlc   -w ${warnings} -g ${c_flags}  -c       ${build_dir}/${dir}/src/${base}.ml  -o ${build_dir}/${dir}/src/${base}.cmo
    ocamlfind ocamlc   -w ${warnings} -g ${lb_flags} -linkpkg ${build_dir}/${dir}/src/${base}.cmo -o ${build_dir}/${dir}/exe/${base}.byte
  fi

  # Can't link cmxa for js-of-ocaml code
  if [ $file != "bin/web_main.ml" ]; then
    ocamlfind ocamlopt -w ${warnings} -g ${c_flags}  -c       ${build_dir}/${dir}/src/${base}.ml  -o ${build_dir}/${dir}/src/${base}.cmx
    ocamlfind ocamlopt -w ${warnings} -g ${ln_flags} -linkpkg ${build_dir}/${dir}/src/${base}.cmx -o ${build_dir}/${dir}/exe/${base}.native
  fi
}

function js {
  file=$1

  dir=$(dirname ${file})
  base=$(basename ${file} | sed 's/\.ml$//')

  time js_of_ocaml \
    +bin_prot.js \
    +core_kernel.js \
    +nat.js \
    +weak.js \
    +base/runtime.js \
    ${build_dir}/${dir}/exe/${base}.byte \
    -o ${build_dir}/${dir}/exe/${base}.js \
    --source-map-inline
}

for lib in odditty_kernel odditty server; do
  for mod in $(get_modules_in_dep_order ${lib}); do
    compile_module ${lib} ${mod}
  done
   compile_clib ${lib}

  pack ${lib}
done

exe test/inline_test_runner.ml
exe bin/main.ml

for lib in odditty_kernel odditty; do
  ./${build_dir}/test/exe/inline_test_runner.native \
    inline-test-runner \
    $lib \
    -verbose
done

${build_dir}/bin/exe/main.native -help

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

for lib in web; do
  for mod in $(get_modules_in_dep_order ${lib}); do
    compile_module ${lib} ${mod}
  done

  pack ${lib}
done

exe bin/web_main.ml

mkdir findlib-js
for f in $(ocamlfind query -format '%+(archive)' core_kernel async_kernel js_of_ocaml.async virtual_dom -recursive -predicates byte | sort -u | grep -v '^$');
do
  js_file=findlib-js/$(basename $f).js
  if [ ! -e ${js_file} ]; then
    time js_of_ocaml --pretty --source-map-inline --no-runtime +runtime.js +weak.js $f -o ${js_file}
  fi
done

mkdir local-js

# /Users/datkin/.opam/4.03.0+for-js//lib/js_of_ocaml/mlString.js

time js_of_ocaml --no-runtime -o /tmp/runtime-1.js --source-map-inline --pretty +predefined_exceptions.js +runtime.js +weak.js +nat.js +base/runtime.js +core_kernel/runtime.js +virtual_dom/virtualdom.compiled.js --runtime-only dummy-source

#
# This should *exclude* the +runtime.js and +weak.js files (in fact, all the "runtime.js" files)
# I'm unclear on which *.js files should be listed here, and which are "covered" by runtime.js
# Definite order:
#  - custom runtime.js
#  - stdlib.cma.js w/ +runtime.js, +weak.js
#  - caml, shadow_stdlib, bigarray, base, ...
# CR datkin: Sort this list in dep order: 
# ocamlfind query -format 'findlib-js/%(archive).js' core_kernel async_kernel virtual_dom js_of_ocaml.async -recursive -predicates byte | grep -v '/.js' | tr '\n' ' '

jsoo_link /tmp/runtime-1.js findlib-js/stdlib.cma.js \
  findlib-js/caml.cma.js findlib-js/shadow_stdlib.cma.js findlib-js/base.cma.js findlib-js/unix.cma.js findlib-js/bigarray.cma.js findlib-js/fieldslib.cma.js findlib-js/ppx_compare_lib.cma.js findlib-js/sexplib.cma.js findlib-js/variantslib.cma.js findlib-js/bin_shape_lib.cma.js findlib-js/bin_prot.cma.js findlib-js/ppx_hash_lib.cma.js findlib-js/inline_test_config.cma.js findlib-js/ppx_inline_test_lib.cma.js findlib-js/base_for_tests.cma.js findlib-js/jane_street_headers.cma.js findlib-js/nums.cma.js findlib-js/ppx_assert_lib.cma.js findlib-js/ppx_bench_lib.cma.js findlib-js/expect_test_common.cma.js findlib-js/expect_test_config.cma.js findlib-js/expect_test_collector.cma.js findlib-js/stdio.cma.js findlib-js/typerep_lib.cma.js findlib-js/core_kernel.cma.js findlib-js/async_kernel.cma.js findlib-js/result.cma.js findlib-js/lwt.cma.js findlib-js/js_of_ocaml.cma.js findlib-js/re.cma.js findlib-js/uutf.cma.js findlib-js/tyxml_f.cma.js findlib-js/virtual_dom.cma.js findlib-js/async_js.cma.js \
   local-js/web.cma.js local-js/web_main.cmo.js -o linked.js

# This one is even closer
jsoo_link /tmp/runtime-1.js findlib-js/stdlib.cma.js   findlib-js/caml.cma.js findlib-js/shadow_stdlib.cma.js findlib-js/base.cma.js findlib-js/unix.cma.js findlib-js/bigarray.cma.js findlib-js/fieldslib.cma.js findlib-js/ppx_compare_lib.cma.js findlib-js/sexplib.cma.js findlib-js/variantslib.cma.js findlib-js/bin_shape_lib.cma.js findlib-js/bin_prot.cma.js findlib-js/ppx_hash_lib.cma.js findlib-js/inline_test_config.cma.js findlib-js/ppx_inline_test_lib.cma.js findlib-js/base_for_tests.cma.js findlib-js/jane_street_headers.cma.js findlib-js/nums.cma.js findlib-js/ppx_assert_lib.cma.js findlib-js/ppx_bench_lib.cma.js findlib-js/expect_test_common.cma.js findlib-js/expect_test_config.cma.js findlib-js/expect_test_collector.cma.js findlib-js/stdio.cma.js findlib-js/typerep_lib.cma.js findlib-js/core_kernel.cma.js findlib-js/async_kernel.cma.js findlib-js/result.cma.js findlib-js/async_js.cma.js    local-js/web.cma.js local-js/web_main.cmo.js -o linked.js

#findlib-js/bigarray.cma.js findlib-js/caml.cma.js findlib-js/shadow_stdlib.cma.js findlib-js/base.cma.js findlib-js/sexplib.cma.js findlib-js/base_for_tests.cma.js findlib-js/bigarray.cma.js findlib-js/bin_prot.cma.js findlib-js/bin_shape_lib.cma.js findlib-js/core_kernel.cma.js findlib-js/expect_test_collector.cma.js findlib-js/expect_test_common.cma.js findlib-js/expect_test_config.cma.js findlib-js/fieldslib.cma.js findlib-js/inline_test_config.cma.js findlib-js/jane_street_headers.cma.js findlib-js/nums.cma.js findlib-js/ppx_assert_lib.cma.js findlib-js/ppx_bench_lib.cma.js findlib-js/ppx_compare_lib.cma.js findlib-js/ppx_hash_lib.cma.js findlib-js/ppx_inline_test_lib.cma.js findlib-js/stdio.cma.js findlib-js/typerep_lib.cma.js findlib-js/unix.cma.js findlib-js/variantslib.cma.js local-js/web.cma.js local-js/web_main.cmo.js -o linked.js

#js bin/web_main.ml

exit
