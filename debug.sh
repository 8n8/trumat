#!/bin/sh

rm a.out
cp input/IfThenElse/LineCommentAndIfThenElseInElse.elm test.elm
clang -g -O0 -fsanitize=memory main.c
gdb --args a.out --overwrite test.elm
