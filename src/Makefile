# Unix Makefile for Moscow ML

# To build the Moscow ML system on a new machine for the first time, 
#	(1) edit PREFIX etc. in file Makefile.inc
#	(2) execute `make world'

# To install it

#	(1) execute `make install'
#	(2) edit PREFIX in file mosml/tools/Makefile.stub

# ------------------------------------------------------------

include Makefile.inc

# Basis dynlibs
BASISDYNLIB=
ifeq ($(DYNLIBSUPPORT),true)
  BASISDYNLIB=intinf msocket munix
endif


# Build the entire system for the first time

world:
	cd config; $(MAKE) all
	cd runtime; $(MAKE) all
	cp runtime/camlrunm$(EXE) .
	cd mosmlyac; $(MAKE) all
	cd mosmllib; $(MAKE) all
	cd compiler; $(MAKE) all
	cd toolssrc; $(MAKE) all
	cd lex; $(MAKE) all
	cd launch; $(MAKE) all
	$(MAKE) basisdynlib

uptodate:
	cd runtime; $(MAKE) all
	cp runtime/camlrunm$(EXE) .
	cd mosmlyac; $(MAKE) all
	cd mosmllib; $(MAKE) all
	cd compiler; $(MAKE) all
	cd toolssrc; $(MAKE) all
	cd lex; $(MAKE) all
	cd launch; $(MAKE) all

# Recompile all Mosml code from scratch
again:
	cd mosmllib; $(MAKE) clean all
	cd compiler; $(MAKE) clean all
	cd toolssrc; $(MAKE) clean all
	cd lex; $(MAKE) clean all


basisdynlib:
	for i in $(BASISDYNLIB); do \
	  (cd dynlibs/$$i; $(MAKE)) || exit $$?; \
	done


# Install the Mosml system
install:
	test -d $(BINDIR) || mkdir -p $(BINDIR)
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	test -d $(INCDIR) || mkdir -p $(INCDIR)
	test -d $(DOCDIR) || mkdir -p $(DOCDIR)
	test -d $(TOOLDIR) || mkdir -p $(TOOLDIR)
	cd runtime; $(MAKE) install
#	cd config; $(MAKE) install
	cd launch; $(MAKE) all install
	cd mosmlyac; $(MAKE) install
	cd mosmllib; $(MAKE) install
	cd compiler; $(MAKE) install
	cd toolssrc; $(MAKE) install
	cd lex; $(MAKE) install
	cd doc; $(MAKE) install
	$(MAKE) basisdynlib_install

basisdynlib_install:
	for i in $(BASISDYNLIB); do \
	  (cd dynlibs/$$i; $(MAKE) install) || exit $$?; \
	done


# Remove all generated files
clean:
	cd config; $(MAKE) clean
	cd runtime; $(MAKE) clean
	cd launch; $(MAKE) clean
	cd mosmlyac; $(MAKE) clean
	cd mosmllib; $(MAKE) clean
	cd compiler; $(MAKE) clean
	cd toolssrc; $(MAKE) clean
	cd lex; $(MAKE) clean
	cd test; $(MAKE) clean
	cd mosmllib/test; $(MAKE) clean
	cd ../examples; $(MAKE) clean	
	cd dynlibs; $(MAKE) clean
	rm -f camlrunm$(EXE)
	cd doc; $(MAKE) clean

dynlibs:
	cd dynlibs; $(MAKE)

# Rebuild the dependencies in all Makefiles.
# You can't do this unless you have a compiled runtime system.
depend:
	cd mosmllib; $(MAKE) depend
	cd mosmlyac; $(MAKE) all
	cd compiler; $(MAKE) depend
	cd lex; $(MAKE) depend
