# Makefile for dynamic library interface to execv etc

# The runtime system must be compiled with support for dynamic libraries.

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I../../runtime

MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmunix.so Unix.uo Signal.uo
	@echo "Now execute 'make test'"

munix.o: munix.c
	$(CC) $(CFLAGS) -c -o munix.o munix.c

libmunix.so: munix.o
	$(DYNLD) -o libmunix.so munix.o 

install:
	cp libmunix.so $(BINDIR)
	cp Signal.sig Unix.sig $(LIBDIR)
	cp Signal.ui Unix.ui $(LIBDIR)
	cp Signal.uo Unix.uo $(LIBDIR)
	@echo
	@echo "Then execute  'make test'"

test:
	gcc -O2 -o sieve sieve.c
	mosml testunix.sml

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.ui
	rm -f *.uo
	rm -f sieve
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
Signal.uo: Signal.ui 
testunix.uo: Unix.ui 
Unix.uo: Unix.ui Signal.ui 
Unix.ui: Signal.ui 
