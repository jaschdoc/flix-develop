#!/bin/bash

echo "Generating tests"
scala lazylist-exhaustive-test.sc > test/GeneratedTests.flix
echo "Done"
