#!/bin/zsh
set -e

clang -g main.c -pg
rm -rf tmp
cp -r input tmp
./a.out --overwrite tmp
gprof | less
