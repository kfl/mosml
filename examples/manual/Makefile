# Unix Makefile stub for separate compilation with Moscow ML.  

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

all: Evaluate.uo Expr.uo Reduce.uo

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
Evaluate.ui: Expr.uo 
Evaluate.uo: Evaluate.ui Expr.uo Reduce.ui 
Reduce.uo: Reduce.ui Expr.uo 
Reduce.ui: Expr.uo 
