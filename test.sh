#!/bin/bash

set -e

rm -rf tmp_formatted tmp_input

cp -r formatted tmp_formatted
cp -r input tmp_input
clang -fsanitize=memory -g -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
./a.out --overwrite tmp_formatted
./a.out --overwrite tmp_input
rm -rf tmp_formatted tmp_input


cp -r formatted tmp_formatted
cp -r input tmp_input
clang -fsanitize=address -g -fno-omit-frame-pointer -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
./a.out --overwrite tmp_formatted > /dev/null
./a.out --overwrite tmp_input > /dev/null
diff -r --color tmp_formatted formatted
diff -r --color tmp_input expected
rm -rf tmp_formatted tmp_input
