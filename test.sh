#!/bin/bash

set -e

rm -rf got
clang -fsanitize=memory -g -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
cp -r tests got
./a.out --overwrite got

rm -rf got
clang -fsanitize=address -g -fno-omit-frame-pointer -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
cp -r tests got
./a.out --overwrite got > /dev/null
rm -rf expected
cp -r tests expected
elm-format expected --yes > /dev/null
diff -r --color got expected

echo "`find tests -type f | wc -l` tests"
