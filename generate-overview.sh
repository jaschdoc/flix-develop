#!/bin/sh

echo "Generating overview..."

scala overview-iterator-lazylist.sc > overview-iterator-lazylist.md

echo "Generated overview in overview-iterator-lazylist.md"
