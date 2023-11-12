#!/bin/bash

set -e

cc -c -Wall -Werror -O0 -g -std=c99 trumat.c
cc -Wall -Werror -O0 -g -std=c99 trumat.o test.c
valgrind --leak-check=yes -s ./a.out
