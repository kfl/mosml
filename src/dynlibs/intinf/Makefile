# Makefile for dynamic library interface to GNU multiprecision package GMP

# The runtime system must be compiled with support for dynamic libraries.
# The GNU GMP multiprecision package must be installed.

# Where to find GMP header file and compiled library

GMPDIR=${HOME}/c/gmp-2.0.2

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(INCDIR) -I ${GMPDIR}

MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmgmp.so IntInf.uo

install:
	cp libmgmp.so $(LIBDIR)

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

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
IntInf.uo: IntInf.ui 
