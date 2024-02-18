#!/bin/zsh
set -e

gcc -O3 main.c
rm -rf tmp
cp -r input tmp
time elm-format --yes tmp > /dev/null
time ./a.out --overwrite tmp
