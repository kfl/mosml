# Makefile for mosml test cases

# This works with bash

all:
	rm -f result
	mosml -P full < opnlcl.sml >> result 2>&1
	mosml -P full < opntop.sml >> result 2>&1
	mosml -P full < link.sml >> result 2>&1
	mosml -P full < array.sml >> result 2>&1
	mosml -P full < array2.sml >> result 2>&1
	mosml -P full < sieve.sml >> result 2>&1
	mosml -P full < poly.sml >> result 2>&1
	mosml -P full < hiorder.sml >> result 2>&1
	mosml -P full < applic.sml >> result 2>&1
	mosml -P full < where.sml >> result 2>&1
	mosml -P full < gnvsap.sml >> result 2>&1
	mosml -P full < refeq.sml >> result 2>&1
	mosml -P full < eq.sml >> result 2>&1        
	mosml -P full < scope.sml >> result 2>&1        
	mosml -P full < matchsuc.sml >> result 2>&1        
	mosml -P full < matcherr.sml >> result 2>&1        
	mosml -P full < sigcon.sml >> result 2>&1        
	mosml -P full < coerce.sml >> result 2>&1        
	mosml -P full < modres.sml >> result 2>&1        
	mosml -P full < inf.sml >> result 2>&1        
	#this one last as its slow
	mosml -P full test.sml >> result 2>&1
	diff result result.ok

# cvr: TODO remove in the final release
newtest:
	rm -f result
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < opnlcl.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < opntop.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < link.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < array.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < array2.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < sieve.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < poly.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < hiorder.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < applic.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < where.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < gnvsap.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < refeq.sml >> result 2>&1
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < eq.sml >> result 2>&1        
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < scope.sml >> result 2>&1        
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < matchsuc.sml >> result 2>&1        
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < matcherr.sml >> result 2>&1        
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < sigcon.sml >> result 2>&1        
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < coerce.sml >> result 2>&1        
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < modres.sml >> result 2>&1        
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full < inf.sml >> result 2>&1        
	#this one last as its slow
	../../camlrunm ../mosmltop -stdlib ../../cvrlib -P full test.sml >> result 2>&1
	diff result result.ok

clean:
	rm -f result








