#!/bin/sh

rm a.out
cp input/CaseOfPattern/FunctionCall/InfixInArgument.elm test.elm
clang -g -O0 -fsanitize=memory main.c
gdb --args a.out --overwrite test.elm
