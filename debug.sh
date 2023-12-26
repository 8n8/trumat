#!/bin/sh

cp -r input tmp_test_input
clang -g -O0 main.c
gdb --args a.out --overwrite tmp_test_input/LineComment/TrailingSpace.elm
rm -rf tmp_test_input
