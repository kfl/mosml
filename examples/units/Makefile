# Unix Makefile stub for separate compilation with Moscow ML.  

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c -liberal
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

UNITS= -structure Expr Reduce -toplevel Evaluate Test

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo
 
all: Evaluate.uo Expr.uo Reduce.uo Test.uo

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak

# these rules are only needed if UNITS is undefined or empty
.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep $(UNITS) >> Makefile

### DO NOT DELETE THIS LINE
Expr.uo: Expr.sml 
	$(MOSMLC) -structure Expr.sml 
Reduce.ui: Reduce.sig Expr.uo 
	$(MOSMLC) -structure Expr.ui Reduce.sig 
Reduce.uo: Reduce.sml Reduce.ui Expr.uo 
	$(MOSMLC) -structure Expr.ui Reduce.sml 
Evaluate.ui: Evaluate.sig Reduce.ui 
	$(MOSMLC) -toplevel Reduce.ui Expr.ui Evaluate.sig 
Evaluate.uo: Evaluate.sml Evaluate.ui Reduce.ui 
	$(MOSMLC) -toplevel Reduce.ui Expr.ui \
    Evaluate.sml 
Test.uo: Test.sml Evaluate.ui 
	$(MOSMLC) -toplevel Evaluate.ui Reduce.ui Expr.ui Test.sml 
