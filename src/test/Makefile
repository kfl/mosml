# Makefile for mosml test cases

# This works with bash

all:
	rm -f result
	mosml -P full test.sml > result 2>&1
	mosml -P full < ovlfail.sml >> result 2>&1
	mosml -P full < constfai.sml >> result 2>&1
	mosml -P full < testcon.sml >> result 2>&1
	mosml -P full < testty.sml >> result 2>&1
	mosml -P full < typerr.sml >> result 2>&1
	mosml -P full < recfail.sml >> result 2>&1
	diff result result.ok

clean:
	rm -f result
