# Unix Makefile for the Moscow ML project manager
# Hopefully this will not be needed in a not so distant future :-)

COMPFLAGS=-toplevel -P full
LINKFLAGS=-P full

include ../Makefile.inc

FILES= \
	Parsercomb-sig.sml Parsercomb.sml PMBasic.sml PMCompile.sml \
	ArgParse-sig.sml ArgParse.sml mosmlpm.sml

OBJS= \
	Parsercomb-sig.uo Parsercomb.uo PMBasic.uo PMCompile.uo \
	ArgParse-sig.uo ArgParse.uo mosmlpm.uo


all: mosmlpm

mosmlpm: $(FILES)
	$(MOSMLC) $(COMPFLAGS) $(FILES)
	$(MOSMLL) $(LINKFLAGS) -noheader $(OBJS) -o mosmlpm 

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f mosmlpm
#	rm -f Makefile.bak
#	cd test; make clean

install:
	${INSTALL_DATA} mosmlpm $(LIBDIR)
