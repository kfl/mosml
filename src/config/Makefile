include ../Makefile.inc

all:
	@echo "Run 'sh autoconf' or 'sh autoconf gcc' or ..."

install:
	$(INSTALL_PROGRAM) m.h s.h $(INCDIR)

clean scratch:
	rm -f m.h s.h
	cd auto-aux; rm -f *.o a.out
