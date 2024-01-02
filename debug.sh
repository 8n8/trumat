#!/bin/sh

rm a.out
cp input/ModuleDeclaration/ExposeAllVariantsOfExplicit.elm test.elm
clang -g -fsanitize=memory -O0 main.c
gdb --args a.out --overwrite test.elm
