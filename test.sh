#!/bin/bash

set -e

cc -c -Wall -Werror -O0 -std=c99 trumat.c
// Build the main runner just to check that it compiles.
cc -Wall -Werror -O0 -std=c99 trumat.o main.c
cc -Wall -Werror -O0 -std=c99 trumat.o test.c
./a.out
