# Primitive (too) Makefile for documentation

# There's no guarantee any of this will work except on my workstations

all: html allps allpdf

allpdf: manual.pdf mosmlref.pdf texsigs.pdf 

allps: manual.ps mosmlref.ps texsigs.ps 

html helpsigs:
	cd helpsigs; make all base 

macmosmlbib: macmosml.tex macmosml.aux.bbl
	latex macmosml
	bibtex macmosml

macmosml.dvi: macmosmlbib
	latex macmosml
	latex macmosml

manualbib: manual.tex manual.bbl
	latex manual
	bibtex manual

manual.dvi: manualbib
	latex manual
	latex manual

mosmlref.dvi: mosmlref.tex helpsigs/index.tex
	latex mosmlref
	latex mosmlref

texsigs.dvi: texsigs.tex helpsigs/texsigsigs.tex
	latex texsigs
	latex texsigs
	makeindex texsigs
	latex texsigs

texsigs2up.ps: texsigs.ps
	psnup -pa4 -2 texsigs.ps texsigs2up.ps

install:
	cp index.html ../../mosmllib
	cd helpsigs; make install

clean:
	rm -f *.aux *.dvi *.blg *.ps *.pdf *.bbl *.log *.idx *.ilg
	cd helpsigs; make clean

.SUFFIXES :
.SUFFIXES : .aux .bbl .tex .bib .dvi .ps .pdf 

.ps.pdf:
	ps2pdf $<

.dvi.ps:
	dvips -ta4 $< -o

.aux.bbl:
	bibtex $<
