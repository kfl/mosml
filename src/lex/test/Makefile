# Makefile for mosml test cases

# This works with bash

all:
	rm -f result
	mosmllex expanded.lex > result 2>&1
	mosmllex abbrev.lex >> result 2>&1
	diff expanded.sml abbrev.sml >> result 2>&1
	diff result result.ok

clean:
	rm -f result
	rm -f *.sml
