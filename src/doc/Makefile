# Primitive Makefile for documentation

allpdf: manual.pdf mosmlref.pdf texsigs.pdf 

allps: manual.ps mosmlref.ps texsigs.ps 

macmosmlbib: macmosml.tex macmosml.aux.bbl
	latex macmosml
	bibtex macmosml

macmosml.dvi: macmosmlbib
	latex macmosml
	latex macmosml

manualbib: manual.tex manual.aux.bbl
	latex manual
	bibtex manual

manual.dvi: manualbib
	latex manual
	latex manual

mosmlref.dvi: mosmlref.tex
	latex mosmlref
	latex mosmlref

texsigs.dvi: texsigs.tex texsigsigs.tex
	latex texsigs
	latex texsigs
	makeindex texsigs
	latex texsigs

texsigs2up.ps: texsigs.ps
	psnup -pa4 -2 texsigs.ps texsigs2up.ps

clean:
	rm -f *.aux *.dvi *.blg *.ps *.pdf

.SUFFIXES :
.SUFFIXES : .aux .bbl .tex .bib .dvi .ps .pdf 

.ps.pdf:
	ps2pdf $<

.dvi.ps:
	dvips -ta4 $< -o

.aux.bbl:
	bibtex $<
