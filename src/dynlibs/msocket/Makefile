# Makefile for dynamic library interface to sockets

# The runtime system must be compiled with support for dynamic libraries.
# The operating system must support sockets.

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(INCDIR)

MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmsocket.so 

msocket.o: msocket.c
	$(CC) $(CFLAGS) -c -o msocket.o msocket.c

libmsocket.so: msocket.o
	$(DYNLD) -o libmsocket.so msocket.o 
# This is needed under Solaris:
#	$(DYNLD) -o libmsocket.so -lsocket msocket.o 

install:
	cp libmsocket.so $(LIBDIR)

test:
	mosml testsocket.sml

clean:
	rm -f *.o
	rm -f *.so
	rm -f Makefile.bak

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
testsocket.uo: Socket.ui 
Socket.uo: Socket.ui 
