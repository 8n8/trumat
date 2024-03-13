#!/bin/bash

set -e

# generate the tests

rm -rf tests
mkdir tests
clang -c hydrogen.c
clang -fsanitize=memory -g -Wall -Werror -Wextra -pedantic -O0 -std=c99 hydrogen.o test.c
./a.out

rm -rf got expected
mv tests got
cp -r got expected

# run the tests

clang -fsanitize=memory -g -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
./a.out --overwrite got

clang -fsanitize=address -g -fno-omit-frame-pointer -Wall -Werror -Wextra -pedantic -O0 -std=c99 main.c
./a.out --overwrite got > /dev/null
elm-format expected --yes > /dev/null
diff -r --color got expected

echo "`find expected -type f | wc -l` tests"
