# Makefile for cross compiling to WIN32

CFLAGS=-O2

HEADER_OBJS= mosmlexe.o
CAMLRUNM_OBJS= camlrunm.o
MOSMLC_OBJS= mosmlc.o driver.o
MOSML_OBJS= mosml.o driver.o

include ../Makefile.inc

all: header.exe camlrunm.exe mosmlc.exe mosml.exe

install:
	$(INSTALL_PROGRAM) header.exe $(DESTDIR)$(LIBDIR)/header
	$(INSTALL_PROGRAM) camlrunm.exe $(DESTDIR)$(BINDIR)/camlrunm.exe
	$(INSTALL_PROGRAM) mosmlc.exe $(DESTDIR)$(BINDIR)/mosmlc.exe
	$(INSTALL_PROGRAM) mosml.exe $(DESTDIR)$(BINDIR)/mosml.exe

clean:
	rm *.exe
	rm *.o

header.exe: $(HEADER_OBJS)
	$(CC) $(CFLAGS) -o header.exe $(HEADER_OBJS) ../runtime/camlrt.dll

camlrunm.exe: $(CAMLRUNM_OBJS)
	$(CC) $(CFLAGS) -o camlrunm.exe $(CAMLRUNM_OBJS) ../runtime/camlrt.dll

mosmlc.exe: $(MOSMLC_OBJS)
	$(CC) $(CFLAGS) -o mosmlc.exe $(MOSMLC_OBJS)

mosml.exe: $(MOSML_OBJS)
	$(CC) $(CFLAGS) -o mosml.exe $(MOSML_OBJS) ../runtime/camlrt.dll

#.c.o:
#	$(CC) $(CFLAGS) -c $<

mosmlexe.o: driver.h mosmlexe.c
camlrunm.o: driver.h camlrunm.c
mosmlc.o: driver.h mosmlc.c
mosml.o: driver.h mosml.c
driver.o: driver.h driver.c

