# Makefile for help utility recompilation

include ../../Makefile.inc

MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c 
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

all: Asynt.uo Database.uo Hasht.uo Htmlsigs.uo Lexer.uo Parser.uo Parsspec.uo \
	Printbase.uo Stack.uo Texsigs.uo makebase.uo makebase

Parser.sml Parser.sig: Parser.grm
	$(MOSMLYACC) Parser.grm

makebase : Database.uo Htmlsigs.uo Lexer.uo Parser.uo Parsspec.uo \
    Printbase.uo Texsigs.uo makebase.uo
	$(MOSMLL) -o makebase makebase.uo

base:
	./makebase $(LIBDIR)

install:
	$(INSTALL_DATA) helpsigs.val $(DESTDIR)$(LIBDIR)
	test -d $(DESTDIR)$(DOCDIR)/mosmllib || mkdir -p $(DESTDIR)$(DOCDIR)/mosmllib
	$(INSTALL_DATA) htmlsigs/*.html $(DESTDIR)$(DOCDIR)/mosmllib
	$(INSTALL_DATA) index.html $(DESTDIR)$(DOCDIR)/mosmllib

uninstall:
	${UNINSTALL_DATA} $(DESTDIR)$(LIBDIR)/helpsigs.val
	${UNINSTALL_DATA} $(DESTDIR)$(DOCDIR)/mosmllib/*.html

depend: Lexer.sml Parser.sml 
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

clean:
	rm -f Parser.sig Parser.sml Lexer.sml
	rm -f makebase index.txt index.tex texsigsigs.tex
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak

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
