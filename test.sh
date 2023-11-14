#!/bin/bash

set -e

cc -c -Wall -Werror -O0 -std=c99 trumat.c
cc -Wall -Werror -O0 -std=c99 trumat.o main.c
cc -Wall -Werror -O0 -std=c99 trumat.o test.c
./a.out
