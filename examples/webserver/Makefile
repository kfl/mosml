# Unix Makefile stub for separate compilation with Moscow ML.  

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

all: echoserver minimalserver mosmlserver

echoserver: echoserver.uo 
	mosmlc -o echoserver echoserver.uo 

minimalserver: minimalserver.uo 
	mosmlc -o minimalserver minimalserver.uo 

mosmlserver: mosmlserver.uo 
	mosmlc -o mosmlserver mosmlserver.uo 

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f echoserver minimalserver mosmlserver
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
