#!/bin/sh

# Check if mosmlb can properly read test.mlb
echo Reading escaped.mlb
../../camlrunm ../mosmlb escaped.mlb
echo Done.
echo Reading test.mlb
../../camlrunm ../mosmlb test.mlb
echo Done.

# Parse-print-parse-compare test on test.mlb
../../camlrunm ../mosmlb-test test.mlb intermediate.mlb
rm -f intermediate.mlb
