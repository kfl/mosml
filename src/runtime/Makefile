# Makefile for the Moscow ML 2.10 version of the Caml Light runtime system

OPTS=-fno-defer-pop -fomit-frame-pointer -DTHREADED
include ../Makefile.inc

BASECFLAGS= -Dunix -O2 $(OPTS)

ifeq ($(UNAME_S),Cross_W32)
	BASECFLAGS= -Dunix -DCAMLRT -DWIN32 -D_CONSOLE -D_MBCS -O2 $(OPTS)
endif

BASEOBJS=interp.o expand.o misc.o stacks.o fix_code.o main.o fail.o signals.o \
  freelist.o major_gc.o minor_gc.o memory.o alloc.o roots.o \
  compare.o ints.o floats.o str.o io.o \
  extern.o externcp.o intern.o interncp.o \
  hash.o sys.o meta.o parsing.o lexing.o gc_ctrl.o mosml.o unix.o \
  runtime.o md5sum.o callback.o

DOBJS=$(OBJS:.o=.d.o) debugger.d.o

UNIXPRIMS=compare.c extern.c externcp.c floats.c gc_ctrl.c hash.c \
  intern.c interncp.c interp.c ints.c io.c lexing.c meta.c parsing.c \
  str.c sys.c mosml.c unix.c md5sum.c callback.c

DOSPRIMS=compare.c extern.c externcp.c floats.c gc_ctrl.c hash.c \
  intern.c interncp.c interp.c ints.c io.c lexing.c meta.c parsing.c \
  str.c sys.c graph.c mosml.c msdos.c md5sum.c

BASEPRIMS=$(UNIXPRIMS)

PRIMS=$(BASEPRIMS) $(ADDPRIMS)
OBJS=$(BASEOBJS) $(ADDOBJS)
LIBS=$(BASELIBS) $(ADDRUNLIBS)
CFLAGS=$(BASECFLAGS) $(ADDRUNCFLAGS)

all: camlrunm$(EXE)

camlrunm$(EXE): $(OBJS) prims.o
	$(LD) -o camlrunm$(EXE) prims.o $(OBJS) $(LIBS)
	$(STRIP) camlrunm$(EXE)

camlrunmd$(EXE): $(DOBJS) prims.d.o
	$(LD) $(CFLAGS) -o camlrunmd$(EXE) prims.d.o $(DOBJS) $(LIBS)


# For cross compiling to WIN32
camlrt.dll: $(OBJS) prims.o
	$(DYNLD) -o camlrt.dll prims.o $(OBJS) $(LIBS) -mconsole
#        $(CC) $(CFLAGS) /Fecamlrt.dll prims.obj $(OBJS) $(LIBS) /link /subsystem:console /def:"mosmldll.def"
#        $(CC) $(CFLAGS) /Fecamlrt.dll prims.obj $(OBJS) $(LIBS) /link /subsystem:console


clean:
	rm -f camlrunm$(EXE) camlrunmd$(EXE) *.o *.a
	rm -f primitives prims.c opnames.h jumptbl.h
	rm -f .debugobj/*.o
	rm -f m.h s.h

install:
	${INSTALL_PROGRAM} camlrunm$(EXE) $(DESTDIR)$(BINDIR)
	${INSTALL_PROGRAM} camlrunm$(EXE) $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} *.h $(DESTDIR)$(INCDIR)

install_w32: camlrt.dll
	${INSTALL_PROGRAM} camlrt.dll $(DESTDIR)$(BINDIR)
	${INSTALL_PROGRAM} camlrt.dll $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} *.h $(DESTDIR)$(INCDIR)


primitives : $(PRIMS)
	sed -n -e '/\/\* ML \*\//s/.* \([a-zA-Z0-9_][a-zA-Z0-9_]*\) *(.*/\1/p' \
                $(PRIMS) > primitives2
	sh -c 'if cmp -s primitives primitives2; \
        then rm primitives2; \
        else mv primitives2 primitives; \
        fi'

prims.c : primitives
	(echo '#include "mlvalues.h"'; \
	 echo '#include "prims.h"'; \
	 sed -e 's/.*/extern value &();/' primitives; \
	 echo 'c_primitive cprim[] = {'; \
	 sed -e 's/.*/  &,/' primitives; \
	 echo '  0 };'; \
         echo 'char * names_of_cprim[] = {'; \
	 sed -e 's/.*/  "&",/' primitives; \
	 echo '  0 };') > prims.c

opnames.h : instruct.h
	sed -e '/\/\*/d' \
            -e 's/enum /char * names_of_/' \
            -e 's/{$$/[] = {/' \
            -e 's/\([A-Z][A-Z_0-9]*\)/"\1"/g' instruct.h > opnames.h

# jumptbl.h is required only if you have GCC 2.0 or later
jumptbl.h : instruct.h
	awk -f ../tools/make-jumptbl instruct.h > jumptbl.h

.SUFFIXES: .d.o

.c.d.o:
	sh -c 'if  [ ! -d .debugobj ] ; then mkdir .debugobj; fi'
	cd .debugobj; $(CC) -Dunix -c -g -I.. -I.. -DDEBUG $(OPTS) ../$<
	mv .debugobj/$*.o $*.d.o

depend : prims.c opnames.h jumptbl.h
	mv Makefile Makefile.bak
	(sed -n -e '1,/^### DO NOT DELETE THIS LINE/p' Makefile.bak;	 \
         gcc -MM -I.. $(OPTS) *.c; \
         gcc -MM -I.. $(OPTS) -DDEBUG *.c | sed -e 's/\.o/.d.o/') > Makefile
	rm Makefile.bak

### EVERYTHING THAT GOES BEYOND THIS COMMENT WILL BE ERASED WITHOUT WARNING
### DO NOT DELETE THIS LINE
alloc.o: alloc.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h debugger.h major_gc.h freelist.h memory.h gc.h minor_gc.h \
 stacks.h
compare.o: compare.c fail.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h str.h
debugger.o: debugger.c
dynlib.o: dynlib.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h fail.h
extern.o: extern.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h
externcp.o: externcp.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h str.h
fail.o: fail.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h fail.h memory.h gc.h major_gc.h freelist.h minor_gc.h \
 signals.h globals.h
fix_code.o: fix_code.c config.h ../config/m.h ../config/s.h misc.h \
 mlvalues.h instruct.h reverse.h
floats.o: floats.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h debugger.h fail.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h
freelist.o: freelist.c config.h ../config/m.h ../config/s.h debugger.h \
 misc.h mlvalues.h freelist.h gc.h gc_ctrl.h major_gc.h
gc_ctrl.o: gc_ctrl.c alloc.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h debugger.h gc.h gc_ctrl.h major_gc.h \
 freelist.h minor_gc.h memory.h
graph.o: graph.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h alloc.h memory.h gc.h major_gc.h freelist.h minor_gc.h fail.h
hash.o: hash.c mlvalues.h config.h ../config/m.h ../config/s.h misc.h \
 memory.h gc.h major_gc.h freelist.h minor_gc.h str.h
intern.o: intern.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h
interncp.o: interncp.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h
interp.o: interp.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h debugger.h fail.h instruct.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h prims.h signals.h stacks.h str.h unalignd.h \
 interp.h jumptbl.h globals.h
ints.o: ints.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h fail.h memory.h gc.h major_gc.h freelist.h minor_gc.h
io.o: io.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h fail.h io.h memory.h gc.h major_gc.h freelist.h minor_gc.h \
 signals.h sys.h
lexing.o: lexing.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h stacks.h memory.h gc.h major_gc.h freelist.h minor_gc.h str.h \
 interp.h
main.o: main.c version.h alloc.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h exec.h fail.h gc.h gc_ctrl.h globals.h \
 intext.h io.h stacks.h memory.h major_gc.h freelist.h minor_gc.h \
 sys.h interp.h
major_gc.o: major_gc.c config.h ../config/m.h ../config/s.h debugger.h \
 misc.h mlvalues.h fail.h freelist.h gc.h gc_ctrl.h globals.h \
 major_gc.h roots.h runtime.h
md5sum.o: md5sum.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h alloc.h str.h md5sum.h
memory.o: memory.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h fail.h freelist.h gc.h gc_ctrl.h major_gc.h \
 memory.h minor_gc.h
meta.o: meta.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h globals.h major_gc.h freelist.h memory.h gc.h minor_gc.h \
 prims.h
minor_gc.o: minor_gc.c config.h ../config/m.h ../config/s.h debugger.h \
 misc.h mlvalues.h fail.h gc.h gc_ctrl.h major_gc.h freelist.h \
 memory.h minor_gc.h roots.h runtime.h
misc.o: misc.c config.h ../config/m.h ../config/s.h debugger.h misc.h \
 mlvalues.h io.h
mosml.o: mosml.c mosml.h fail.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h memory.h gc.h major_gc.h freelist.h minor_gc.h str.h \
 runtime.h alloc.h intext.h io.h debugger.h globals.h
msdos.o: msdos.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h signals.h instruct.h fail.h
parsing.o: parsing.c config.h ../config/m.h ../config/s.h mlvalues.h \
 misc.h memory.h gc.h major_gc.h freelist.h minor_gc.h
prims.o: prims.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h prims.h
roots.o: roots.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h stacks.h
runtime.o: runtime.c config.h ../config/m.h ../config/s.h runtime.h
signals.o: signals.c alloc.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h signals.h stacks.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h fail.h instruct.h interp.h
stacks.o: stacks.c config.h ../config/m.h ../config/s.h fail.h misc.h \
 mlvalues.h stacks.h memory.h gc.h major_gc.h freelist.h minor_gc.h \
 debugger.h
str.o: str.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h debugger.h
sys.o: sys.c config.h ../config/m.h ../config/s.h alloc.h misc.h \
 mlvalues.h fail.h globals.h instruct.h signals.h stacks.h memory.h \
 gc.h major_gc.h freelist.h minor_gc.h io.h
unix.o: unix.c fail.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h memory.h gc.h major_gc.h freelist.h minor_gc.h str.h \
 alloc.h
alloc.d.o: alloc.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h debugger.h major_gc.h freelist.h memory.h gc.h minor_gc.h \
 stacks.h
compare.d.o: compare.c fail.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h str.h
debugger.d.o: debugger.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h instruct.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h opnames.h stacks.h unalignd.h
dynlib.d.o: dynlib.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h fail.h
extern.d.o: extern.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h
externcp.d.o: externcp.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h str.h
fail.d.o: fail.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h fail.h memory.h gc.h major_gc.h freelist.h minor_gc.h \
 signals.h
fix_code.d.o: fix_code.c config.h ../config/m.h ../config/s.h misc.h \
 mlvalues.h instruct.h reverse.h
floats.d.o: floats.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h debugger.h fail.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h
freelist.d.o: freelist.c config.h ../config/m.h ../config/s.h debugger.h \
 misc.h mlvalues.h freelist.h gc.h gc_ctrl.h major_gc.h
gc_ctrl.d.o: gc_ctrl.c alloc.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h debugger.h gc.h gc_ctrl.h major_gc.h \
 freelist.h minor_gc.h memory.h
graph.d.o: graph.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h alloc.h memory.h gc.h major_gc.h freelist.h minor_gc.h fail.h
hash.d.o: hash.c mlvalues.h config.h ../config/m.h ../config/s.h misc.h \
 memory.h gc.h major_gc.h freelist.h minor_gc.h str.h
intern.d.o: intern.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h
interncp.d.o: interncp.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h fail.h gc.h intext.h io.h memory.h \
 major_gc.h freelist.h minor_gc.h reverse.h
interp.d.o: interp.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h debugger.h fail.h instruct.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h prims.h signals.h stacks.h str.h unalignd.h \
 interp.h
ints.d.o: ints.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h fail.h memory.h gc.h major_gc.h freelist.h minor_gc.h
io.d.o: io.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h fail.h io.h memory.h gc.h major_gc.h freelist.h minor_gc.h \
 signals.h sys.h
lexing.d.o: lexing.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h stacks.h memory.h gc.h major_gc.h freelist.h minor_gc.h str.h \
 interp.h
main.d.o: main.c version.h alloc.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h exec.h fail.h gc.h gc_ctrl.h globals.h \
 intext.h io.h stacks.h memory.h major_gc.h freelist.h minor_gc.h \
 sys.h interp.h
major_gc.d.o: major_gc.c config.h ../config/m.h ../config/s.h debugger.h \
 misc.h mlvalues.h fail.h freelist.h gc.h gc_ctrl.h globals.h \
 major_gc.h roots.h runtime.h
md5sum.d.o: md5sum.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h alloc.h str.h md5sum.h
memory.d.o: memory.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h fail.h freelist.h gc.h gc_ctrl.h major_gc.h \
 memory.h minor_gc.h
meta.d.o: meta.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h globals.h major_gc.h freelist.h memory.h gc.h minor_gc.h \
 prims.h
minor_gc.d.o: minor_gc.c config.h ../config/m.h ../config/s.h debugger.h \
 misc.h mlvalues.h fail.h gc.h gc_ctrl.h major_gc.h freelist.h \
 memory.h minor_gc.h roots.h runtime.h
misc.d.o: misc.c config.h ../config/m.h ../config/s.h debugger.h misc.h \
 mlvalues.h io.h
mosml.d.o: mosml.c fail.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h memory.h gc.h major_gc.h freelist.h minor_gc.h str.h \
 runtime.h alloc.h intext.h io.h debugger.h
msdos.d.o: msdos.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h signals.h instruct.h fail.h
parsing.d.o: parsing.c config.h ../config/m.h ../config/s.h mlvalues.h \
 misc.h memory.h gc.h major_gc.h freelist.h minor_gc.h
prims.d.o: prims.c mlvalues.h config.h ../config/m.h ../config/s.h \
 misc.h prims.h
roots.d.o: roots.c debugger.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h memory.h gc.h major_gc.h freelist.h \
 minor_gc.h stacks.h
runtime.d.o: runtime.c config.h ../config/m.h ../config/s.h runtime.h
signals.d.o: signals.c alloc.h misc.h config.h ../config/m.h \
 ../config/s.h mlvalues.h signals.h stacks.h memory.h gc.h major_gc.h \
 freelist.h minor_gc.h fail.h instruct.h interp.h
stacks.d.o: stacks.c config.h ../config/m.h ../config/s.h fail.h misc.h \
 mlvalues.h stacks.h memory.h gc.h major_gc.h freelist.h minor_gc.h \
 debugger.h
str.d.o: str.c alloc.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h debugger.h
sys.d.o: sys.c config.h ../config/m.h ../config/s.h alloc.h misc.h \
 mlvalues.h fail.h globals.h instruct.h signals.h stacks.h memory.h \
 gc.h major_gc.h freelist.h minor_gc.h io.h
unix.d.o: unix.c fail.h misc.h config.h ../config/m.h ../config/s.h \
 mlvalues.h memory.h gc.h major_gc.h freelist.h minor_gc.h str.h \
 alloc.h
