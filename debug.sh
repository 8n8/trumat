#!/bin/sh

rm a.out
cp input/Float/1.0e1.elm test.elm
clang -g -fsanitize=memory -O0 main.c
gdb --args a.out --overwrite test.elm
