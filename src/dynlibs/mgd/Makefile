# Makefile for dynamic library interface to Thomas Boutell's gd image package

# The runtime system must be compiled with support for dynamic libraries.
# Thomas Boutell's gd image package must be installed.

# Where to find gd header file and compiled library

GDDIR=${HOME}/c/gd1.3

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I../../runtime -I ${GDDIR}

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmgd.so Gdimage.uo Graphs.uo
	@echo
	@echo "Now type  make install "

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
	@echo
	@echo "You must set your LD_LIBRARY_PATH for Gdimage to find libmgd.so."
	@echo "You need to do something like this:"
	@echo " tcsh: setenv LD_LIBRARY_PATH ${BINDIR}"
	@echo " bash: export LD_LIBRARY_PATH=${BINDIR}"
	@echo 
	@echo "Then execute  mosml testgdimage.sml"

clean:
	rm -f *.o
	rm -f *.so
	rm -f *.ui
	rm -f *.uo
	rm -f *.gif
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
Gdimage.uo: Gdimage.ui 
