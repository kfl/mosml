# Makefile for Moscow ML 1.43 dynamically loadable gdbm library

# The runtime system must be compiled with support for dynamic libraries.
# The GNU GDBM persistent hashtables must be installed.

# Where to find GDBM header file and compiled library

GDBMDIR=${HOME}/c/gdbm-1.7.3

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I../../runtime -I ${GDBMDIR}
BASELIBS=-lm -ldl

include ../../Makefile.inc

all: libmgdbm.so
	@echo
	@echo "Now type  make install "

install:
	cp libmgdbm.so $(BINDIR)
	@echo
	@echo "You must set your LD_LIBRARY_PATH for Gdbm to find libmgdbm.so."
	@echo "You need to do something like this:"
	@echo " tcsh: setenv LD_LIBRARY_PATH ${BINDIR}"
	@echo " bash: export LD_LIBRARY_PATH=${BINDIR}"
	@echo 
	@echo "Then try mosml example1.sml, followed by mosml example2.sml"

mgdbm.o: mgdbm.c
	$(CC) $(CFLAGS) -c -o mgdbm.o mgdbm.c 

libmgdbm.so: mgdbm.o
	$(DYNLD) -o libmgdbm.so mgdbm.o ${GDBMDIR}/libgdbm.a

test:
	mosml -P full testgdbm.sml

clean:
	rm -f *.o
	rm -f *.so
	rm -f empty testdb monthnames

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
