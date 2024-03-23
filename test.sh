#!/bin/bash

set -e

rm -rf got
cp -r input got
tcc -Wall -std=c99 -run main.c --overwrite got
diff -r --color got expected

echo "`find input -type f | wc -l` tests"
