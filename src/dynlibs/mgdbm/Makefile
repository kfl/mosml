# Makefile for Moscow ML dynamically loadable gdbm library

# The runtime system must be compiled with support for dynamic libraries.
# The GNU GDBM persistent hashtables must be installed.

# Where to find GDBM header file and compiled library

GDBMLIBDIR=${HOME}/c/gdbm-1.7.3
GDBMINCDIR=${HOME}/c/gdbm-1.7.3
#GDBMLIBDIR=/usr/lib
#GDBMINCDIR=/usr/include

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(INCDIR) -I ${GDBMINCDIR}
BASELIBS=-lm -ldl

all: libmgdbm.so

install:
	cp libmgdbm.so $(LIBDIR)

mgdbm.o: mgdbm.c
	$(CC) $(CFLAGS) -c -o mgdbm.o mgdbm.c 

libmgdbm.so: mgdbm.o
	$(DYNLD) -o libmgdbm.so mgdbm.o ${GDBMLIBDIR}/libgdbm.a

test:
	mosml -P full testgdbm.sml

clean:
	rm -f *.o
	rm -f *.so
	rm -f empty testdb monthnames

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
