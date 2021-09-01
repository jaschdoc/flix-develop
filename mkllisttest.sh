#!/bin/bash

echo "Generating tests"
scala lazylist-exhaustive-test.sc > test/flixlazylisttests.flix
echo "Done"
