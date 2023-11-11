#!/bin/bash

set -e

cc -c -Wall -Werror -g -std=c99 trumat.c
cc -Wall -Werror -g -std=c99 trumat.o test.c
