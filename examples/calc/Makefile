# Unix makefile for calc example

MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLLEX=${MOSMLHOME}/bin/mosmllex
MOSMLYACC=${MOSMLHOME}/bin/mosmlyac

MOSMLC=mosmlc -c
MOSMLL=mosmlc

# Unix
REMOVE=rm -f
MOVE=mv
EXEFILE=

# DOS
#REMOVE=del
#MOVE=move
#EXEFILE=.exe

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

OBJS=Parser.uo Lexer.uo calc.uo

all: calc

calc: $(OBJS)
	$(MOSMLL) -o calc$(EXEFILE) calc.uo

Parser.sml Parser.sig: Parser.grm
	$(MOSMLYACC) Parser.grm

Lexer.sml: Lexer.lex
	$(MOSMLLEX) Lexer.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) Parser.sig
	$(REMOVE) Parser.sml
	$(REMOVE) Lexer.sml
	$(REMOVE) calc
	$(REMOVE) *.ui
	$(REMOVE) *.uo

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: Lexer.sml Parser.sml Parser.sig
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
Parser.uo: Parser.ui 
calc.uo: Parser.ui Lexer.uo 
Lexer.uo: Parser.ui 
