# Makefile for dynamic library interface to POSIX regular expressions

# The runtime system must be compiled with support for dynamic libraries.
# The regex library must be installed.

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I../../runtime

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmregex.so Regex.uo
	@echo "Now execute 'mosml testregex.sml' to try regexp matching"

mregex.o: mregex.c
	$(CC) $(CFLAGS) -c -o mregex.o mregex.c

libmregex.so: mregex.o
	(cd regex-0.12; ./configure; make regex.o)
	$(DYNLD) -o libmregex.so regex-0.12/regex.o mregex.o 

install:
	cp libmregex.so $(BINDIR)
	cp Regex.sig $(LIBDIR)
	cp Regex.ui $(LIBDIR)
	cp Regex.uo $(LIBDIR)
	@echo
	@echo "Then execute  mosml testregex.sml"

test:
	mosml testregex.sml

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	(cd regex-0.12; make clean)

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
Regex.uo: Regex.ui 
