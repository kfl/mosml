# Unix Makefile for the Moscow ML mlb tool.

COMPFLAGS=-P full
LINKFLAGS=-P full

include ../Makefile.inc

STRFILES= ArgParse.sig ArgParse.sml Mlb.sml Parser.sig Parser.sml Scanner.sml Mlb_functions.sml

TOPFILES=mosmlb.sml

OBJS=ArgParse.uo Mlb.uo Parser.uo Scanner.uo Mlb_functions.uo mosmlb.uo

all: mosmlb

mosmlb: $(STRFILES) $(TOPFILES)
	$(MOSMLC) $(COMPFLAGS) -structure $(STRFILES) -toplevel $(TOPFILES)
	$(MOSMLL) $(LINKFLAGS) -noheader $(OBJS) -o mosmlb 

Scanner.sml: Scanner.lex
	$(MOSMLLEX) $<

Parser.sml Parser.sig: Parser.grm
	$(MOSMLYACC) $<

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f mosmlb
	rm -f Makefile.bak
	rm -f *~

install:
	${INSTALL_DATA} mosmlb $(DESTDIR)$(LIBDIR)
