# Makefile for dynamic library interface to GNU multiprecision package GMP

# The runtime system must be compiled with support for dynamic libraries.
# The GNU GMP multiprecision package must be installed.

# Where to find GMP header file and compiled library

GMPDIR=${HOME}/c/gmp-2.0.2

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I../../runtime -I ${GMPDIR}

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmgmp.so IntInf.uo
	@echo "Now type  make install "

install:
	cp libmgmp.so $(BINDIR)
	@echo
	@echo "You must set your LD_LIBRARY_PATH for Intinf to find libmgmp.so."
	@echo "You need to do something like this:"
	@echo " tcsh: setenv LD_LIBRARY_PATH ${BINDIR}"
	@echo " bash: export LD_LIBRARY_PATH=${BINDIR}"
	@echo 
	@echo "Then try  mosml fac.sml"

intinf.o: intinf.c
	$(CC) $(CFLAGS) -c -o intinf.o intinf.c

libmgmp.so: intinf.o
	$(DYNLD) -o libmgmp.so intinf.o ${GMPDIR}/libgmp.a

test:
	mosml testintinf.sml

clean:
	rm -f *.o
	rm -f *.so
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
IntInf.uo: IntInf.ui 
