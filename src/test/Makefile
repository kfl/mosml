# Makefile for mosml test cases

# This works with bash
MOSML=mosml

all:
	rm -f result
	$(MOSML) -P full test.sml > result 2>&1
	$(MOSML) -P full < ovlfail.sml >> result 2>&1
	$(MOSML) -P full < constfai.sml >> result 2>&1
	$(MOSML) -P full < testcon.sml >> result 2>&1
	$(MOSML) -P full < testty.sml >> result 2>&1
	$(MOSML) -P full < typerr.sml >> result 2>&1
	$(MOSML) -P full < recfail.sml >> result 2>&1

diff:
	diff result result.ok

current:
	rm -f result
	make all MOSML=../camlrunm\ ../compiler/mosmltop\ -stdlib\ ../mosmllib

clean:
	rm -f result
