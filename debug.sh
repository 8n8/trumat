#!/bin/sh

# cp -r formatted tmp_test_formatted
clang -g -O0 main.c
gdb --args a.out --overwrite test.elm # tmp_test_formatted/Float/1e1.elm
# rm -rf tmp_test_formatted
