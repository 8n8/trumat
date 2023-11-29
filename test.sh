#!/bin/bash

set -e

# Run the tests with the memory sanitizer
clang -fsanitize=memory -g -c -Wall -Werror -O0 -std=c99 trumat.c
clang -fsanitize=memory -g -Wall -Werror -O0 -std=c99 trumat.o test.c
./a.out

# Run the tests with the address sanitizer
clang -fsanitize=address -fno-omit-frame-pointer -g -c -Wall -Werror -O0 -std=c99 trumat.c
clang -fsanitize=address -fno-omit-frame-pointer -g -Wall -Werror -O0 -std=c99 trumat.o test.c
# Pipe it to /dev/null so that the test results are not printed again. The
# results were printed by the previous run.
./a.out >/dev/null
