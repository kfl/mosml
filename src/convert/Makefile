# Unix Makefile for convert


MOSMLHOME=../..
MOSMLTOOLS=../camlrunm $(MOSMLHOME)/tools

MOSMLC=$(MOSMLHOME)/bin/mosmlc -c
MOSMLL=$(MOSMLHOME)/bin/mosmlc
MOSMLLEX=$(MOSMLHOME)/bin/mosmllex
MOSMLYACC=$(MOSMLHOME)/bin/mosmlyac

CONVOBJS = Scanner.uo Parser.uo Convert.uo
GENPMOBJS = Deplex.uo Deppars.uo GenPm.uo

LEXYACC = Scanner.sml Parser.sml Parser.sig Deplex.sml Deppars.sig Deppars.sml
LEXYACCSML = Scanner.sml Parser.sml Deplex.sml Deppars.sml

BINUTILS = convert gen_pm

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo .grm .lex

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

.lex.sml:
	$(MOSMLLEX) $<

all: $(BINUTILS)

convert: $(CONVOBJS)
	$(MOSMLL) -standalone -o convert Convert.uo

gen_pm: $(GENPMOBJS)
	$(MOSMLL) -standalone -o gen_pm GenPm.uo


Parser.sml Parser.sig: Parser.grm
	$(MOSMLYACC) $<


Deppars.sml Deppars.sig: Deppars.grm
	$(MOSMLYACC) $<

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -f $(LEXYACC)
	rm -f $(BINUTILS)

#install:
#	${INSTALL_DATA} convert $(TOOLDIR)

depend: $(LEXYACCSML)
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
Deppars.uo: Deppars.ui 
Deplex.uo: Deppars.ui 
Parser.uo: Parser.ui 
Scanner.uo: Parser.ui 
GenPm.uo: Deppars.ui Deplex.uo 
Convert.uo: Parser.ui Scanner.uo 
