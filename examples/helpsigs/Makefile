# Makefile for help utility recompilation

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c 
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac
CPP=/lib/cpp -P -traditional -Dunix -Umsdos

all: Asynt.uo Database.uo Hasht.uo Htmlsigs.uo Lexer.uo Parser.uo Parsspec.uo \
	Printbase.uo Stack.uo makebase.uo makebase

Parser.sml Parser.sig: Parser.grm
	$(MOSMLYACC) Parser.grm

makebase : Database.uo Htmlsigs.uo Lexer.uo Parser.uo Parsspec.uo \
    Printbase.uo makebase.uo
	$(MOSMLL) -o makebase makebase.uo

base:
	makebase

install:
	cp helpsigs.val ../../src/mosmllib
	cp helpsigs.val ../../lib
	cp htmlsigs/*.html ../../doc/mosmllib
	cp index.tex ${HOME}/tex/papers/mosml

.SUFFIXES :
.SUFFIXES : .sml .sig .ui .uo .mlp .lex .c .o

.sig.ui:
	$(MOSMLC) $(COMPFLAGS) $<

.sml.uo:
	$(MOSMLC) $(COMPFLAGS) $<

.mlp.sml:
	$(CPP) $< > $*.sml

.lex.sml:
	$(MOSMLLEX) $<

depend: Lexer.sml Parser.sml 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

clean:
	rm -f Parser.sig Parser.sml Lexer.sml
	rm -f makebase helpsigs.val index.txt index.tex 
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -f htmlsigs/*.html

### DO NOT DELETE THIS LINE
Hasht.uo: Hasht.ui 
Stack.uo: Stack.ui 
Parser.ui: Asynt.uo 
Parser.uo: Parser.ui Asynt.uo 
Lexer.uo: Parser.ui Stack.ui Hasht.ui 
Htmlsigs.uo: Htmlsigs.ui Database.ui 
Printbase.uo: Database.ui 
Parsspec.uo: Asynt.uo Parser.ui Lexer.uo Database.ui 
Database.uo: Database.ui 
makebase.uo: Htmlsigs.ui Database.ui Printbase.uo Parsspec.uo 
