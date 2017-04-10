#!/bin/sh -e

set -x

rebar3 escriptize
mkdir -p regressions
cd regressions
unset ERL_LIBS
../_build/default/bin/beam_disassemble --regression-test
head -3 regression_test.log
