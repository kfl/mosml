# Unix Makefile for mosmldep

INCLUDES=-I ../compiler
COMPFLAGS=$(INCLUDES)
LINKFLAGS=-g $(INCLUDES)

include ../Makefile.inc

OBJS = \
	Deppars.uo Deplex.uo Mosmldep.uo

LIBOBJS = \
	Obj.uo Nonstdio.uo Lexing.uo Parsing.uo

CLIBOBJS = \
	Hasht.uo Fnlib.uo Config.uo

all: cutdeps mosmldep

mosmldep: $(OBJS)
	$(MOSMLL) $(LINKFLAGS) -noheader -o mosmldep Mosmldep.uo

cutdeps: cutdeps.uo
	$(MOSMLL) $(LINKFLAGS) -noheader -o cutdeps cutdeps.uo

Deppars.sml Deppars.sig: Deppars.grm
	$(MOSMLYACC) Deppars.grm

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -f Deppars.sig Deppars.sml Deplex.sig Deplex.sml Mosmldep.sml
	rm -f cutdeps mosmldep

install:
	${INSTALL_DATA} cutdeps mosmldep $(TOOLDIR)

depend: Deplex.sml Deppars.sml Mosmldep.sml
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLCUT) < Makefile.bak > Makefile
	$(MOSMLDEP) >> Makefile

### DO NOT DELETE THIS LINE
Mosmldep.uo: Deppars.ui Deplex.uo 
Deppars.uo: Deppars.ui 
Deplex.uo: Deppars.ui 
Smltope.uo: Smltope.ui Deplex.uo Deppars.ui 
Maine.uo: Maine.ui Smltope.ui 
