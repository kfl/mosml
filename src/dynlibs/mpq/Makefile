# Makefile for dynamic library interface to PostgreSQL database server

# The runtime system must be compiled with support for dynamic libraries.
# The PostgreSQL C library interface must be installed.

# Where to find the PostgreSQL header file and compiled library
# Note: Unix distributions vary widely in the PGSQLLIBDIR 
#       and PGSQLINCDIR locations

PGSQLLIBDIR=/usr/lib
PGSQLINCDIR=/usr/include/pgsql
#PGSQLLIBDIR=/usr/local/pgsql/lib
#PGSQLINCDIR=/usr/local/pgsql/include
#PGSQLLIBDIR=/usr/lib/pgsql/lib
#PGSQLINCDIR=/usr/lib/pgsql/include
#PGSQLLIBDIR=/pack/postgresql/lib
#PGSQLINCDIR=/pack/postgresql/include

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS=-Dunix -O2 $(OPTS) $(ADDDYNLIBCFLAGS) -I$(INCDIR) -I ${PGSQLINCDIR}

MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmpq.so

mpq.o: mpq.c
	$(CC) $(CFLAGS) -c -o mpq.o mpq.c

libmpq.so: mpq.o
# Some recent Linux distributions need this:
	$(DYNLD) -lcrypt -o libmpq.so mpq.o ${PGSQLLIBDIR}/libpq.a
#	$(DYNLD) -o libmpq.so mpq.o ${PGSQLLIBDIR}/libpq.a

install:
	cp libmpq.so $(LIBDIR)

test:
	mosml testpsql.sml

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
testpsql.uo: Postgres.ui 
Postgres.uo: Postgres.ui 
