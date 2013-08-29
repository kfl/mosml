# Unix Makefile for the Moscow ML project manager
# Hopefully this will not be needed in a not so distant future :-)

COMPFLAGS=-P full
LINKFLAGS=-P full

include ../Makefile.inc

STRFILES= Parsercomb.sig Parsercomb.sml ArgParse.sig ArgParse.sml

TOPFILES= Compilerinterface.sml Systemcompile.sml PMBasic.sml PMCompile.sml \
	  mosmlpm.sml

OBJS= \
        Parsercomb.uo ArgParse.uo Compilerinterface.uo Systemcompile.uo \
	PMBasic.uo PMCompile.uo \
	mosmlpm.uo


all: mosmlpm

mosmlpm: $(STRFILES) $(TOPFILES)
	$(MOSMLC) $(COMPFLAGS) -structure $(STRFILES) -toplevel $(TOPFILES)
	$(MOSMLL) $(LINKFLAGS) -noheader $(OBJS) -o mosmlpm 

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f test/*.ui
	rm -f test/*.uo
	rm -f mosmlpm
	rm -f Makefile.bak
#	cd test; make clean

install:
	${INSTALL_DATA} mosmlpm $(DESTDIR)$(LIBDIR)
