#!/bin/bash

set -e

rm -rf tmp_test_formatted tmp_test_input

cp -r formatted tmp_test_formatted
cp -r unformatted tmp_test_input
clang -fsanitize=memory -g -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
./a.out --overwrite tmp_test_formatted
./a.out --overwrite tmp_test_input
rm -rf tmp_test_formatted tmp_test_input


cp -r formatted tmp_test_formatted
cp -r unformatted tmp_test_input
clang -fsanitize=address -g -fno-omit-frame-pointer -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
./a.out --overwrite tmp_test_formatted > /dev/null
./a.out --overwrite tmp_test_input > /dev/null
diff -r --color tmp_test_formatted formatted
cp -r unformatted tmp_test_expected
elm-format tmp_test_expected --yes > /dev/null
diff -r --color tmp_test_input tmp_test_expected
rm -rf tmp_test_formatted tmp_test_input tmp_test_expected

echo "`find unformatted formatted -type f | wc -l` tests of which `find formatted -type f | wc -l` are formatted and `find unformatted -type f | wc -l` are unformatted"
