# Primitive Makefile for Moscow ML documentation -- for DIN A4 paper

# Requires latex, bibtex, makeindex, dvips, ps2pdf, psnup
# Thus no guarantee this will work except on my workstations ...

include ../Makefile.inc

all: helpsigs allps allpdf

allpdf: manual.pdf mosmlref.pdf mosmllib.pdf mosmllib2up.pdf 

allps: manual.ps mosmlref.ps mosmllib.ps mosmllib2up.ps 

LATEX=latex -interaction=batchmode

helpsigs:
	cd helpsigs; make all base 

macmosmlbib: macmosml.tex macmosml.bbl
	${LATEX} macmosml
	bibtex macmosml

macmosml.dvi: macmosmlbib
	${LATEX} macmosml
	${LATEX} macmosml

manual.dvi: manual.tex manual.bbl mosml.sty
	${LATEX} manual
	bibtex manual
	${LATEX} manual
	${LATEX} manual

index.tex texsigsigs.tex:
	cd helpsigs; make all base

mosmlref.dvi: mosmlref.tex mosml.sty
	${LATEX} mosmlref
	${LATEX} mosmlref

mosmllib.dvi: mosmllib.tex texsigsigs.tex
	touch mosmllib.ind
	${LATEX} mosmllib
	makeindex mosmllib
	${LATEX} mosmllib
	${LATEX} mosmllib

mosmllib2up.ps: mosmllib.ps
	psnup -pa4 -2 mosmllib.ps mosmllib2up.ps

install:
	cp mosmlref.pdf $(DOCDIR)
	cp manual.pdf $(DOCDIR)
	cp mosmllib.pdf $(DOCDIR)
	cp mosmllib2up.pdf $(DOCDIR)
	cd helpsigs; make install

clean:
	rm -f *.aux *.dvi *.blg *.bbl *.blg *.log *.ps
	rm -f *.ind *.idx *.ilg *.toc
	rm -f index.tex texsigsigs.tex
	cd helpsigs; make clean

.SUFFIXES :
.SUFFIXES : .aux .bbl .tex .bib .dvi .ps .pdf 

.ps.pdf:
	ps2pdf $<

.dvi.ps:
	dvips -ta4 $< -o

.tex.aux: 
	${LATEX} $<

.aux.bbl:
	bibtex $<
