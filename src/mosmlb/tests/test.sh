#!/bin/sh
../../camlrunm ../mosmlb test.mlb
../../camlrunm ../mosmlb-test test.mlb intermediate.mlb
rm -f intermediate.mlb
