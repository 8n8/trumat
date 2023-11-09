#!/bin/bash

set -e

cc -c -Wall -Werror -O2 -std=c99 text.c trumat.c
cc -Wall -O2 -Werror -std=c99 text.o trumat.o main.c -o trumat
