# Makefile for dynamic library interface to MySQL database server

# The runtime system must be compiled with support for dynamic libraries.
# The MySQL C (client) library interface must be installed.

# Where to find the Mysql header file and compiled library

MYSQLLIBDIR=/usr/lib/mysql
MYSQLINCDIR=/usr/include/mysql
#MYSQLLIBDIR=/usr/local/lib/mysql
#MYSQLINCDIR=/usr/local/include/mysql

include ../../Makefile.inc

OPTS=-fno-defer-pop
CFLAGS= -Dunix -O2 $(OPTS) -I$(INCDIR) -I ${MYSQLINCDIR}

MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmmysql.so 

mmysql.o: mmysql.c
	$(CC) $(CFLAGS) -c -o mmysql.o mmysql.c

libmmysql.so: mmysql.o
	$(DYNLD) -o libmmysql.so mmysql.o -L${MYSQLLIBDIR} -lmysqlclient -lnsl
#	$(DYNLD) -o libmmysql.so mmysql.o -L${MYSQLLIBDIR} -lmysqlclient

install:
	cp libmmysql.so $(LIBDIR)

test:
	mosml testmysql.sml

clean:
	rm -f *~
	rm -f *.o
	rm -f *.so
	rm -f Makefile.bak

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
testpsql.uo: Mysql.ui 
Mysql.uo: Mysql.ui 
