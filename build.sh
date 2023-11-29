#!/bin/bash

set -e

clang -c -Wall -Werror -O2 -std=c99 trumat.c
clang -Wall -O2 -Werror -std=c99 trumat.o main.c -o trumat
