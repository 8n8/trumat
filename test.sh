#!/bin/bash

set -e

clang -fsanitize=memory -g -c -Wall -Werror -O0 -std=c99 trumat.c
# Build the main runner just to check that it compiles.
clang -fsanitize=memory -g -Wall -Werror -O0 -std=c99 trumat.o main.c
clang -fsanitize=memory -g -Wall -Werror -O0 -std=c99 trumat.o test.c
./a.out
