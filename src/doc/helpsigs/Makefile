# Makefile for help utility recompilation

include ../../Makefile.inc

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c 
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac
CPP=/lib/cpp -P -traditional -Dunix -Umsdos

all: Asynt.uo Database.uo Hasht.uo Htmlsigs.uo Lexer.uo Parser.uo Parsspec.uo \
	Printbase.uo Stack.uo Texsigs.uo makebase.uo makebase

Parser.sml Parser.sig: Parser.grm
	$(MOSMLYACC) Parser.grm

makebase : Database.uo Htmlsigs.uo Lexer.uo Parser.uo Parsspec.uo \
    Printbase.uo Texsigs.uo makebase.uo
	$(MOSMLL) -o makebase makebase.uo

base:
	makebase

install:
	cp helpsigs.val ../../../lib
	cp htmlsigs/*.html ../../../doc/mosmllib

depend: Lexer.sml Parser.sml 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

clean:
	rm -f Parser.sig Parser.sml Lexer.sml
	rm -f makebase helpsigs.val index.txt index.tex texsigsigs.tex
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -rf htmlsigs

### DO NOT DELETE THIS LINE
Texsigs.uo: Texsigs.ui Database.ui 
Parser.ui: Asynt.uo 
Parser.uo: Parser.ui Asynt.uo 
Lexer.uo: Parser.ui Stack.ui Hasht.ui 
makebase.uo: Htmlsigs.ui Database.ui Printbase.uo Parsspec.uo 
Stack.uo: Stack.ui 
Printbase.uo: Database.ui 
Parsspec.uo: Asynt.uo Parser.ui Lexer.uo Database.ui 
Htmlsigs.uo: Htmlsigs.ui Database.ui 
Hasht.uo: Hasht.ui 
Database.uo: Database.ui 
