include ../Makefile.inc

all: defs.h runtime

defs.h:
	@echo "#define VERSION $(VERSION)" > defs.h
	@echo "#define VERSION_S \"$(VERSION)\"" >> defs.h
	@echo "#define DYNLIBSUPPORT $(DYNLIBSUPPORT)" >> defs.h 
	@echo "#define DYNLIBSUPPORT_S \"$(DYNLIBSUPPORT)\"" >> defs.h 
	sh autoconf $(CC)

.PHONY: runtime
runtime:
	$(INSTALL_PROGRAM) m.h s.h ../runtime

install:

clean scratch:
	rm -f m.h s.h defs.h
	cd auto-aux; rm -f *.o a.out
