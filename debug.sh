#!/bin/sh

cp -r formatted tmp_test_formatted
clang -g -O0 main.c
gdb --args a.out --overwrite tmp_test_formatted/UpperName/2.elm
rm -rf tmp_test_formatted
