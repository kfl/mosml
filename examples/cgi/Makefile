# Unix Makefile stub for separate compilation with Moscow ML.  

# Edit this to point to the CGI binaries directory on your webserver:

WWWCGIBIN=/var/lib/httpd/cgi-bin/sestoft

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

all: cgiex1 cgiex2

cgiex1: cgiex1.uo
	mosmlc -noheader -o cgiex1 cgiex1.uo 

cgiex2: cgiex2.uo
	mosmlc -noheader -o cgiex2 cgiex2.uo 

cgitest: cgitest.uo
	mosmlc -noheader -o cgitest cgitest.uo 

install:
	cat `which camlrunm` cgiex1 > ${WWWCGIBIN}/cgiex1
	chmod a+x ${WWWCGIBIN}/cgiex1
	cat `which camlrunm` cgiex2 > ${WWWCGIBIN}/cgiex2
	chmod a+x ${WWWCGIBIN}/cgiex2

uninstall:
	rm -f ${WWWCGIBIN}/cgiex1
	rm -f ${WWWCGIBIN}/cgiex2
	rm -f ${WWWCGIBIN}/cgitest

installcgitest:
	cat `which camlrunm` cgitest > ${WWWCGIBIN}/cgitest
	chmod a+x ${WWWCGIBIN}/cgitest

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f cgiex1 cgiex2 cgitest
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
