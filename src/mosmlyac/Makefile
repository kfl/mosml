# Unix Makefile for the parser generator.

CFLAGS=-O -DNDEBUG

include ../Makefile.inc

OBJS= closure.o error.o lalr.o lr0.o main.o mkpar.o output.o reader.o \
  skeleton.o symtab.o verbose.o warshall.o

all: mosmlyac$(EXE)

mosmlyac$(EXE): $(OBJS)
	$(CC) $(CFLAGS) -o mosmlyac$(EXE) $(OBJS)
	$(STRIP) mosmlyac$(EXE)

clean:
	rm -f *.o
	rm -f mosmlyac$(EXE)

install:
	${INSTALL_PROGRAM} mosmlyac$(EXE) $(BINDIR)/mosmlyac$(EXE)

depend:

closure.o: defs.h
error.o: defs.h
lalr.o: defs.h
lr0.o: defs.h
main.o: defs.h
mkpar.o: defs.h
output.o: defs.h
reader.o: defs.h
skeleton.o: defs.h
symtab.o: defs.h
verbose.o: defs.h
warshall.o: defs.h
