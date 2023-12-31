#!/bin/sh

rm a.out
cp input/CommentsBeforeTopBindEquals/LineBlockLine.elm test.elm
clang -g -O0 main.c
gdb --args a.out --overwrite test.elm
