include ../Makefile.inc

all: defs.h runtime

defs.h:
	@echo "#define VERSION $(VERSION)" > defs.h
	@echo "#define VERSION_S \"$(VERSION)\"" >> defs.h
	@echo "#define DYNLIBSUPPORT $(DYNLIBSUPPORT)" >> defs.h 
	@echo "#define DYNLIBSUPPORT_S \"$(DYNLIBSUPPORT)\"" >> defs.h 

.PHONY: runtime w32
runtime:
	sh autoconf $(CC)
	$(INSTALL_DATA) m.h s.h ../runtime
w32:
	$(INSTALL_DATA) ../config.w32/m.h ../runtime
	$(INSTALL_DATA) ../config.w32/s.h ../runtime


clean scratch:
	rm -f m.h s.h defs.h
	cd auto-aux; rm -f *.o a.out
