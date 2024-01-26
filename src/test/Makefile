# Makefile for mosml test cases

# This works with bash
MOSML=mosml
MAKE=gmake

all:
	rm -f result
	$(MOSML) -P full test.sml > result 2>&1
	$(MOSML) -P full < ovlfail.sml >> result 2>&1
# Only works on 32-bit architectures
# $(MOSML) -P full < constfai.sml >> result 2>&1
	$(MOSML) -P full < testcon.sml >> result 2>&1
	$(MOSML) -P full < testty.sml >> result 2>&1
	$(MOSML) -P full < typerr.sml >> result 2>&1
	$(MOSML) -P full < recfail.sml >> result 2>&1

diff:
	diff result result.ok

# Make results that are independent of timings and mosml version
cleaned:
	grep -v "^User:" result | grep -v "^Moscow ML version" > cleaned-result
	grep -v "^User:" result.ok | grep -v "^Moscow ML version" > cleaned-result.ok

current:
	rm -f result
	$(MAKE) all MOSML=../camlrunm\ ../compiler/mosmltop\ -stdlib\ ../mosmllib

clean:
	rm -f result
