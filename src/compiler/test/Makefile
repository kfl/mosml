# Makefile for mosml test cases

# This works with bash

MOSML=mosml -liberal

# with the supplied compiler
all:
	rm -f result
	$(MOSML) -P full < opnlcl.sml >> result 2>&1
	$(MOSML) -P full < opntop.sml >> result 2>&1
	$(MOSML) -P full < link.sml >> result 2>&1
	$(MOSML) -P full < array.sml >> result 2>&1
	$(MOSML) -P full < array2.sml >> result 2>&1
	$(MOSML) -P full < sieve.sml >> result 2>&1
	$(MOSML) -P full < poly.sml >> result 2>&1
	$(MOSML) -P full < hiorder.sml >> result 2>&1
	$(MOSML) -P full < applic.sml >> result 2>&1
	$(MOSML) -P full < where.sml >> result 2>&1
	$(MOSML) -P full < gnvsap.sml >> result 2>&1
	$(MOSML) -P full < refeq.sml >> result 2>&1
	$(MOSML) -P full < eq.sml >> result 2>&1        
	$(MOSML) -P full < scope.sml >> result 2>&1        
	$(MOSML) -P full < matchsuc.sml >> result 2>&1        
	$(MOSML) -P full < matcherr.sml >> result 2>&1        
	$(MOSML) -P full < sigcon.sml >> result 2>&1        
	$(MOSML) -P full < coerce.sml >> result 2>&1        
	$(MOSML) -P full < modres.sml >> result 2>&1        
	$(MOSML) -P full < inf.sml >> result 2>&1        
	$(MOSML) -P full < recmod.sml >> result 2>&1        
	$(MOSML) -P full < okasaki.sml >> result 2>&1        
	$(MOSML) -P full < greederr.sml >> result 2>&1        
	$(MOSML) -P full < sharing.sml >> result 2>&1        
	$(MOSML) -P full < wheretyp.sml >> result 2>&1        
	#this one last as its slow
	$(MOSML) -P full test.sml >> result 2>&1
	diff result result.ok

# with the current compiler
current:
	rm -f result
	make all MOSML=../../camlrunm\ ../mosmltop\ -stdlib\ ../../mosmllib

clean:
	rm -f result
	rm -f *.ui *.uo
