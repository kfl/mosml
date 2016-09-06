# Unix Makefile for the Moscow ML mlb tool.

COMPFLAGS=-P full
LINKFLAGS=-P full

include ../Makefile.inc

STRFILES=Arg.sig Arg.sml Mlb.sml Log.sml Parser.sig Parser.sml Scanner.sml Mlb_functions.sml

TESTTOPFILES=mosmlb-test.sml
TOPFILES=mosmlb.sml

OBJS=Arg.uo Mlb.uo Log.uo Parser.uo Scanner.uo Mlb_functions.uo

all: mosmlb-test mosmlb

mosmlb-test: $(STRFILES) $(TESTTOPFILES)
	$(MOSMLC) $(COMPFLAGS) -structure $(STRFILES) -toplevel $(TESTTOPFILES)
	$(MOSMLL) $(LINKFLAGS) -noheader $(OBJS) mosmlb-test.uo -o mosmlb-test

mosmlb: $(STRFILES) $(TOPFILES)
	$(MOSMLC) $(COMPFLAGS) -structure $(STRFILES) -toplevel $(TOPFILES)
	$(MOSMLL) $(LINKFLAGS) -noheader $(OBJS) mosmlb.uo -o mosmlb

Scanner.sml: Scanner.lex
	$(MOSMLLEX) $<

Parser.sml Parser.sig: Parser.grm
	$(MOSMLYACC) $<

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f mosmlb-test
	rm -f mosmlb
	rm -f Makefile.bak
	rm -f *~

install:
	${INSTALL_DATA} mosmlb $(DESTDIR)$(LIBDIR)
