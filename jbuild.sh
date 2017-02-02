#!/bin/bash

eval $(opam config env)

jbuilder odditty_kernel/bin/inline_test_runner.exe
./_build/default/odditty_kernel/bin/inline_test_runner.exe inline-test-runner odditty_kernel
