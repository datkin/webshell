#!/bin/bash

jbuilder bin/inline_test_runner.exe
./_build/default/bin/inline_test_runner.exe inline-test-runner server_lib -verbose
