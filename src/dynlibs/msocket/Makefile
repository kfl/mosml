# Makefile for dynamic library interface to sockets

# The runtime system must be compiled with support for dynamic libraries.
# The operating system must support sockets.

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I../../runtime

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmsocket.so Socket.uo
	@echo "Now execute 'mosml testserver.sml'"

msocket.o: msocket.c
	$(CC) $(CFLAGS) -c -o msocket.o msocket.c

libmsocket.so: msocket.o
	$(DYNLD) -o libmsocket.so msocket.o 
# This is needed under Solaris:
#	$(DYNLD) -o libmsocket.so -lsocket msocket.o 

install:
	cp libmsocket.so $(BINDIR)
	cp Socket.sig $(LIBDIR)
	cp Socket.ui $(LIBDIR)
	cp Socket.uo $(LIBDIR)
	@echo
	@echo "Then execute  mosml testserver.sml"

test:
	mosml testsocket.sml

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
testsocket.uo: Socket.ui 
Socket.uo: Socket.ui 
