#!/bin/bash

set -e

rm -rf got
stack build --fast
cp -r input got
stack exec -- trumat --overwrite got
rm -rf expected
cp -r input expected
elm-format expected --yes > /dev/null
diff -r --color got expected

echo "`find input -type f | wc -l` tests"
