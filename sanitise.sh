#!/bin/bash

set -e

rm -rf tmp
clang -fsanitize=memory -g -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
cp -r input tmp
./a.out --overwrite tmp

rm -rf tmp
clang -fsanitize=address -g -fno-omit-frame-pointer -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
cp -r input tmp
./a.out --overwrite tmp > /dev/null
