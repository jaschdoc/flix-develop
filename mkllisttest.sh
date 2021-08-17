#!/bin/bash

echo "Generating tests"
scala lazylist-exhaustive-test.sc > flixlazylisttestsfile
echo "Done"
