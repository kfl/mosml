# Makefile for testing the ML <-> C callback interface
# Requires Dynlib

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I../../../runtime
BASELIBS=-lm -ldl

include ../../../Makefile.inc

all: test

cside.o: cside.c
	$(CC) $(CFLAGS) -c -o cside.o cside.c

libcside.so: cside.o
	$(DYNLD) -o libcside.so cside.o

test: libcside.so
	mosml testcallback.sml

clean:
	rm -f *.o
	rm -f *.so

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
