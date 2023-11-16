#!/bin/bash

set -e

# Make sure that the main runner builds
clang -c -Wall -Werror -O0 -std=c99 trumat.c
clang -Wall -Werror -O0 -std=c99 trumat.o main.c

# Run the tests with the memory sanitizer
clang -fsanitize=memory -g -c -Wall -Werror -O0 -std=c99 trumat.c
clang -fsanitize=memory -g -Wall -Werror -O0 -std=c99 trumat.o test.c
./a.out
