#!/bin/bash

set -e

rm -rf got
cp -r input got
clang -fsanitize=memory -g -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
./a.out --overwrite got

rm -rf got
cp -r input got
clang -fsanitize=address -g -fno-omit-frame-pointer -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
./a.out --overwrite got > /dev/null
rm -rf expected
cp -r input expected
elm-format expected --yes > /dev/null
diff -r --color got expected

echo "`find input -type f | wc -l` tests"
