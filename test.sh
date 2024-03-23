#!/bin/zsh

set -e

rm -rf got
cp -r input got
tcc -Wall -std=c99 -run main.c --overwrite got

rm -rf expected
cp -r input expected
elm-format expected --yes > /dev/null
diff -r --color got expected

echo "`find input -type f | wc -l` tests"
