# Makefile for Moscow ML dynamic libraries: the crypt example

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(INCDIR)
BASELIBS=-lm -ldl

all: libcrypt.so 
	@echo "Now execute 'mosml crypt.sml' to test the C function"

crypt.o: crypt.c
	$(CC) $(CFLAGS) -c -o crypt.o crypt.c

libcrypt.so: crypt.o
	$(DYNLD) -lcrypt -o libcrypt.so crypt.o
#	$(DYNLD)         -o libcrypt.so crypt.o

clean:
	rm -f *.o
	rm -f *.so


### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
