#!/bin/bash

set -e

stack install --fast
rm -rf tmp_test_formatted
cp -r formatted tmp_test_formatted
trumat-exe --overwrite tmp_test_formatted
diff -r formatted tmp_test_formatted
rm -rf tmp_test_formatted
echo "FINISHED TEST RUN"
