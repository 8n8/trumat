#!/bin/bash

set -e

rm -rf tmp_formatted tmp_input

cp -r formatted tmp_formatted
cp -r input tmp_input
clang -fsanitize=memory -g -Wall -Werror -O0 -std=c99 main.c
./a.out --overwrite tmp_formatted
./a.out --overwrite tmp_input
rm -rf tmp_formatted tmp_input

cp -r formatted tmp_formatted
cp -r input tmp_input
clang -fsanitize=address -g -fno-omit-frame-pointer -Wall -Werror -O0 -std=c99 main.c
./a.out --overwrite tmp_formatted > /dev/null
./a.out --overwrite tmp_input > /dev/null
diff -r --color formatted tmp_formatted
diff -r --color expected tmp_input
rm -rf tmp_formatted tmp_input
