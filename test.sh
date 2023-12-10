#!/bin/bash

set -e

stack install --fast
rm -rf tmp_test_formatted
cp -r formatted tmp_test_formatted
trumat-exe --overwrite tmp_test_formatted
diff -r --color formatted tmp_test_formatted
rm -rf tmp_test_formatted

rm -rf tmp_test_input
cp -r input tmp_test_input
trumat-exe --overwrite tmp_test_input
diff -r --color tmp_test_input expected
rm -rf tmp_test_input

echo "FINISHED TEST RUN"
