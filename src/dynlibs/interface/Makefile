# Makefile for Moscow ML dynamic libraries: the interface example

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(INCDIR)

all: libcside.so

cside.o: cside.c
	$(CC) $(CFLAGS) -c -o cside.o cside.c

libcside.so: cside.o
	$(DYNLD) -o libcside.so cside.o

clean:
	rm -f *.o
	rm -f *.so

test:
	mosml smlside.sml

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
