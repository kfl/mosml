# Makefile for Moscow ML 1.43 dynamic libraries: the interface example

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I../../runtime
BASELIBS=-lm -ldl

include ../../Makefile.inc

all: libcside.so
	@echo
	@echo "Now execute 'mosml smlside.sml' to test the C functions"

cside.o: cside.c
	$(CC) $(CFLAGS) -c -o cside.o cside.c

libcside.so: cside.o
	$(DYNLD) -o libcside.so cside.o

clean:
	rm -f *.o
	rm -f *.so

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
