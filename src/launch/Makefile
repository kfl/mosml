# Unix Makefile for launching Moscow ML
# Based on Caml Light (copyright 1993 INRIA)

include ../Makefile.inc

all: mosml mosmlc mosmllex camlexec testprog

# header cannot be generated until camlrunm is installed in $(BINDIR)...

install:
	echo "#!$(BINDIR)/camlrunm" > $(DESTDIR)$(LIBDIR)/header;
	for script in mosml mosmlc mosmllex; do \
	  ${INSTALL_SCRIPT} $$script $(DESTDIR)$(BINDIR)/$$script; \
	  chmod a+x $(DESTDIR)$(BINDIR)/$$script; \
	done

old_install:
	(echo "#!$(BINDIR)/camlrunm"; \
	  echo "exit 2"; \
	  cat testprog) > /tmp/testscr
	chmod a+x /tmp/testscr
	sh -c 'if sh -c /tmp/testscr 2>/dev/null; \
                then echo "#!$(BINDIR)/camlrunm" > $(LIBDIR)/header; \
                else ${INSTALL_PROGRAM} camlexec$(EXE) $(LIBDIR)/header; \
                fi'
	rm -f /tmp/testscr
	echo "#!$(BINDIR)/camlrunm" > $(DESTDIR)$(LIBDIR)/header;
	for script in mosml mosmlc mosmllex; do \
	  ${INSTALL_SCRIPT} $$script $(BINDIR)/$$script; \
	  chmod a+x $(BINDIR)/$$script; \
	done

uninstall:
	${UNINSTALL_PROGRAM} $(DESTDIR)$(LIBDIR)/header
	cd $(DESTDIR)$(BINDIR) && ${UNINSTALL_SCRIPT} mosml mosmlc mosmllex

mosml: mosml.tpl
	sed -e "s|LIBDIR|$(LIBDIR)|" -e "s|BINDIR|$(BINDIR)|" mosml.tpl > mosml

mosmlc: mosmlc.tpl
	sed -e "s|LIBDIR|$(LIBDIR)|" -e "s|BINDIR|$(BINDIR)|" -e "s|VERSION|$(VERSION)|" -e "s|CC|$(CC)|" mosmlc.tpl > mosmlc

mosmllex: mosmllex.tpl
	sed -e "s|LIBDIR|$(LIBDIR)|" -e "s|BINDIR|$(BINDIR)|" mosmllex.tpl > mosmllex

testprog: testprog.sml
	$(MOSMLC) testprog.sml
	$(MOSMLL) -noheader -o testprog BasicIO.uo testprog.uo

camlexec.c: camlexec.tpl
	sed -e "s|LIBDIR|$(LIBDIR)|" -e "s|BINDIR|$(BINDIR)|" camlexec.tpl > camlexec.c

camlexec: camlexec.c
	$(CC) $(OPTS) -o camlexec$(EXE) camlexec.c
	$(STRIP) camlexec$(EXE)

clean:
	rm -f camlexec.c camlexec$(EXE) mosmlc mosml mosmllex testprog testprog.u[io]

depend:
