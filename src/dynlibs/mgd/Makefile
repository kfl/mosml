# Makefile for dynamic library interface to Thomas Boutell's gd image package

# The runtime system must be compiled with support for dynamic libraries.
# Thomas Boutell's gd image package must be installed.

# Where to find gd header file and compiled library

GDDIR=${HOME}/c/gd1.3

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I../../runtime -I ${GDDIR}

MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmgd.so Gdimage.uo Graphs.uo

mgd.o: mgd.c
	$(CC) $(CFLAGS) -c -o mgd.o mgd.c

libmgd.so: mgd.o
	$(DYNLD) -o libmgd.so mgd.o ${GDDIR}/libgd.a

test:
	mosml testgdimage.sml

install:
	cp libmgd.so $(BINDIR)
	cp Gdimage.sig $(LIBDIR)
	cp Gdimage.ui $(LIBDIR)
	cp Gdimage.uo $(LIBDIR)

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.gif
	rm -f Makefile.bak

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
Gdimage.uo: Gdimage.ui 
