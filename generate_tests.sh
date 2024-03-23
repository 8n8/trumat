#!/bin/bash

set -e

rm -rf expected
cp -r input expected
elm-format expected --yes > /dev/null

