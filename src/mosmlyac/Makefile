# Unix Makefile for the parser generator.

CFLAGS=-O -DNDEBUG

include ../Makefile.inc

OBJS= closure.o error.o lalr.o lr0.o main.o mkpar.o output.o reader.o \
  skeleton.o symtab.o verbose.o warshall.o

all: mosmlyac$(EXE)

mosmlyac: $(OBJS)
	$(CC) $(CFLAGS) -o mosmlyac $(OBJS)
	$(STRIP) mosmlyac

mosmlyac.exe: $(OBJS)
	$(CC) $(CFLAGS) -o mosmlyac.exe $(OBJS) -mconsole
	$(STRIP) mosmlyac.exe

clean_obj:
	rm -f *.o

clean: clean_obj
	rm -f mosmlyac mosmlyac.exe

install:
	${INSTALL_PROGRAM} mosmlyac$(EXE) $(DESTDIR)$(BINDIR)/mosmlyac$(EXE)

uninstall:
	${UNINSTALL_PROGRAM} $(DESTDIR)$(BINDIR)/mosmlyac$(EXE)

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
