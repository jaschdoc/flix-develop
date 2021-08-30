#!/bin/bash

echo "Generating tests"
scala lazylist-exhaustive-test.sc > flixlazylisttests.flix
echo "Done"
