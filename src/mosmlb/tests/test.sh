#!/bin/sh

# Check if mosmlb can properly read test.mlb
../../camlrunm ../mosmlb test.mlb

# Parse-print-parse-compare test on test.mlb
../../camlrunm ../mosmlb-test test.mlb intermediate.mlb
rm -f intermediate.mlb
