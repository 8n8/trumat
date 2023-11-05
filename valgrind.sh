#!/bin/bash

set -e

cc -c -Wall -Werror -O0 -g -std=c99 text.c trumat.c
cc -Wall -Werror -O0 -g -std=c99 text.o trumat.o test.c
valgrind --leak-check=yes -s ./a.out
