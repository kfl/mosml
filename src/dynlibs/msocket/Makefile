# Makefile for dynamic library interface to sockets

# The runtime system must be compiled with support for dynamic libraries.
# The operating system must support sockets.

include ../../Makefile.inc

#OPTS=-fno-defer-pop
ROOTDIR:=../..
CFLAGS=-Dunix -O3 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(CAMLRT)

MOSMLTOOLS=camlrunm $(TOOLSDIR)

all: libmsocket.so 

msocket.o: msocket.c
	$(CC) $(CFLAGS) -c -o msocket.o msocket.c

libmsocket.so: msocket.o
	$(DYNLD) -o libmsocket.so msocket.o 
# This is needed under Solaris:
#	$(DYNLD) -o libmsocket.so -lsocket msocket.o 

install:
	$(INSTALL_DATA) libmsocket.so $(DESTDIR)$(LIBDIR)

uninstall:
	${UNINSTALL_DATA} $(DESTDIR)$(LIBDIR)/libmsocket.so

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
