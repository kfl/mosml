# Primitive Makefile for Moscow ML documentation -- for DIN A4 paper

# Requires latex, bibtex, makeindex, dvips, ps2pdf, psnup
# Thus no guarantee this will work except on my workstations ...

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

manual.dvi: manual.tex manual.bbl
	${LATEX} manual
	bibtex manual

index.tex texsigsigs.tex:
	cd helpsigs; make all base

mosmlref.dvi: mosmlref.tex index.tex
	${LATEX} mosmlref
	${LATEX} mosmlref

mosmllib.dvi: mosmllib.tex texsigsigs.tex
	touch mosmllib.ind
	${LATEX} mosmllib
	makeindex mosmllib
	${LATEX} mosmllib

mosmllib2up.ps: mosmllib.ps
	psnup -pa4 -2 mosmllib.ps mosmllib2up.ps

install:
	cp mosmlref.pdf ../../doc
	cp manual.pdf ../../doc
	cp mosmllib.pdf ../../doc
	cp index.html ../../mosmllib
	cd helpsigs; make install

clean:
	rm -f *.aux *.dvi *.blg *.ps *.pdf *.bbl *.blg *.log
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
