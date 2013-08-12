include ../Makefile.inc

all: defs.h
	@echo "Run 'sh autoconf' or 'sh autoconf gcc' or ..."

defs.h:
	@echo "#define VERSION $(VERSION)" > defs.h
	@echo "#define VERSION_S \"$(VERSION)\"" >> defs.h
	@echo "#define DYNLIBSUPPORT $(DYNLIBSUPPORT)" >> defs.h 
	@echo "#define DYNLIBSUPPORT_S \"$(DYNLIBSUPPORT)\"" >> defs.h 


install:
	$(INSTALL_PROGRAM) m.h s.h $(INCDIR)

clean scratch:
	rm -f m.h s.h defs.h
	cd auto-aux; rm -f *.o a.out
