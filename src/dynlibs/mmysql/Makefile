# Makefile for dynamic library interface to MySQL database server

# The runtime system must be compiled with support for dynamic libraries.
# The MySQL C (client) library interface must be installed.

# Where to find the Mysql header file and compiled library

MYSQLLIBDIR=/usr/lib/mysql
MYSQLINCDIR=/usr/include/mysql
include ../../Makefile.inc

#MYSQLLIBDIR=/usr/local/lib/mysql
#MYSQLINCDIR=/usr/local/include/mysql
#include /usr/local/mosml/src/Makefile.inc

OPTS=-fno-defer-pop
CFLAGS= -Dunix -O2 $(OPTS) -I ../../runtime -I ${MYSQLINCDIR}
# CFLAGS= -Dunix -O2 $(OPTS) -I /usr/local/mosml/src/runtime -I ${MYSQLINCDIR}

MOSMLHOME=${HOME}/mosml
#MOSMLHOME=/usr/local/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: libmmysql.so Mysql.uo
	@echo "Now execute 'mosml testmysql.sml' to try Mysql"

mmysql.o: mmysql.c
	$(CC) $(CFLAGS) -c -o mmysql.o mmysql.c

libmmysql.so: mmysql.o
	$(DYNLD) -o libmmysql.so mmysql.o -L${MYSQLLIBDIR} -lmysqlclient -lnsl
#	$(DYNLD) -o libmmysql.so mmysql.o -L${MYSQLLIBDIR} -lmysqlclient

install:
	cp libmmysql.so $(BINDIR)
	cp Mysql.sig $(LIBDIR)
	cp Mysql.ui $(LIBDIR)
	cp Mysql.uo $(LIBDIR)
	@echo
	@echo "You must set your LD_LIBRARY_PATH for Mysql to find libmmysql.so."
	@echo "You need to do something like this:"
	@echo " tcsh: setenv LD_LIBRARY_PATH ${BINDIR}"
	@echo " bash: export LD_LIBRARY_PATH=${BINDIR}"
	@echo 
	@echo "Then execute  mosml testmysql.sml"

test:
	mosml testmysql.sml

clean:
	rm -f *~
	rm -f *.o
	rm -f *.so
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
testpsql.uo: Mysql.ui 
Mysql.uo: Mysql.ui 
