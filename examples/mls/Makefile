# Unix Makefile for mls

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc 

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

all: mls

mls: mls.uo
	$(MOSMLL) -o mls mls.uo

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -f mls

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
