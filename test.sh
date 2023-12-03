#!/bin/bash

set -e

rm -rf tmp_test_formatted

stack install --fast

cp -r formatted tmp_test_formatted
trumat-exe --overwrite tmp_test_formatted
diff formatted tmp_test_formatted
