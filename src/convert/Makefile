# Unix Makefile for convert


MOSMLHOME=../..
MOSMLTOOLS=../camlrunm $(MOSMLHOME)/tools

MOSMLC=$(MOSMLHOME)/bin/mosmlc -c
MOSMLL=$(MOSMLHOME)/bin/mosmlc
MOSMLLEX=$(MOSMLHOME)/bin/mosmllex
MOSMLYACC=$(MOSMLHOME)/bin/mosmlyac

OBJS = Scanner.uo Parser.uo Convert.uo

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

all: convert

convert: $(OBJS)
	$(MOSMLL) -standalone -o convert Convert.uo



Parser.sml Parser.sig: Parser.grm
	$(MOSMLYACC) Parser.grm

Scanner.sml: Scanner.lex
	$(MOSMLLEX) Scanner.lex

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -f Parser.sig Parser.sml Scanner.sml
	rm -f convert

#install:
#	${INSTALL_DATA} convert $(TOOLDIR)

depend: Scanner.sml Parser.sml
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
Convert.uo: Parser.ui Scanner.uo 
Parser.uo: Parser.ui 
Scanner.uo: Parser.ui 
