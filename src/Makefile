# Unix Makefile for Moscow ML

# To build the Moscow ML system on a new machine for the first time, 
#	(1) edit MOSMLHOME etc. in file Makefile.inc
#	(2) execute `make world'

# To install it

#	(1) execute `make install'
#	(2) edit MOSMLHOME in file mosml/tools/Makefile.stub

# ------------------------------------------------------------

include Makefile.inc

# Build the entire system for the first time

world:
	cd config; sh autoconf $(CC)
	cd runtime; make all
	cp runtime/camlrunm$(EXE) .
	cd mosmlyac; make all
	cd mosmllib; make all
	cd compiler; make all
	cd toolssrc; make all
	cd lex; make all
	cd launch; make all

uptodate:
	cd runtime; make all
	cp runtime/camlrunm$(EXE) .
	cd mosmlyac; make all
	cd mosmllib; make all
	cd compiler; make all
	cd toolssrc; make all
	cd lex; make all
	cd launch; make all

# Recompile all Mosml code from scratch
again:
	cd mosmllib; make clean all
	cd compiler; make clean all
	cd toolssrc; make clean all
	cd lex; make clean all

# Install the Mosml system
install:
	test -d $(BINDIR) || mkdir -p $(BINDIR)
	test -d $(LIBDIR) || mkdir -p $(LIBDIR)
	test -d $(INCDIR) || mkdir -p $(INCDIR)
	test -d $(DOCDIR) || mkdir -p $(DOCDIR)
	test -d $(TOOLDIR) || mkdir -p $(TOOLDIR)
	cd runtime; make install
	cd config; make install
	cd launch; make all install
	cd mosmlyac; make install
	cd mosmllib; make install
	cd compiler; make install
	cd toolssrc; make install
	cd lex; make install
	(cd $(INCDIR)/..; rm -f config; ln -s include config)
	cd doc; make install

# Remove all generated files
clean:
	cd config; make clean
	cd runtime; make clean
	cd launch; make clean
	cd mosmlyac; make clean
	cd mosmllib; make clean
	cd compiler; make clean
	cd toolssrc; make clean
	cd lex; make clean
	cd test; make clean
	cd mosmllib/test; make clean
	cd ../examples; make clean	
	cd dynlibs; make clean
	rm -f camlrunm$(EXE)
	cd doc; make clean

dynlibs:
	cd dynlibs; make

# Rebuild the dependencies in all Makefiles.
# You can't do this unless you have a compiled runtime system.
depend:
	cd mosmllib; make depend
	cd mosmlyac; make all
	cd compiler; make depend
	cd lex; make depend
