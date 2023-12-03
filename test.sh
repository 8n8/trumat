#!/bin/bash

set -e

rm -rf tmp_test_formatted

cp -r formatted tmp_test_formatted
clang -fsanitize=memory -g -Wall -Werror -O0 -std=c99 main.c
./a.out --overwrite tmp_test_formatted
rm -rf tmp_test_formatted

cp -r formatted tmp_test_formatted
clang -fsanitize=address -g -fno-omit-frame-pointer -Wall -Werror -O0 -std=c99 main.c
./a.out --overwrite tmp_test_formatted > /dev/null
diff -r formatted tmp_test_formatted
rm -rf tmp_test_formatted
