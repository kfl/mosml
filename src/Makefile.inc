# Unix configuration Makefile for Moscow ML

# Where to install stuff

MOSMLHOME=${HOME}/mosml

# Various utility programs
INSTALL_PROGRAM=cp
INSTALL_SCRIPT=cp
INSTALL_DATA=cp
PERL=perl

# To compile the runtime system camlrunm with support for
# dynamically loadable libraries (DDLs), uncomment these:

ADDPRIMS=dynlib.c
ADDOBJS=dynlib.o
ADDRUNLIBS=-ldl
ADDRUNCFLAGS=
ADDDYNLIBCFLAGS=

# To compile the runtime system camlrunm under Cygwin with support for
# dynamically loadable libraries (DDLs), uncomment these:

ADDPRIMS=dynlib.c
ADDOBJS=dynlib.o
ADDRUNCFLAGS=
ADDDYNLIBCFLAGS=

# For Linux (x86 or Alpha), use:
# ------------------------------
CPP=/lib/cpp -P -traditional -Dunix -Umsdos
STRIP=strip
LD=gcc -rdynamic
DYNLD=ld -shared

# For Cygwin 20.1, use:
# ---------------------
# MOSMLHOME=/mosml
# CPP=/lib/cpp -P -traditional -Dunix -Umsdos
# STRIP=strip
# LD=gcc -Wl,--export-dynamic
# DYNLD=ld -shared
# EXE=.exe

# For NetBSD, use:
# ------------------------
# CPP=/usr/bin/cpp -P -traditional -Dunix -Umsdos
# STRIP=/usr/bin/strip
# ADDRUNCFLAGS=-fPIC
# ADDDYNLIBCFLAGS=-fPIC
# INSTALL_PROGRAM=${BSD_INSTALL_PROGRAM}
# INSTALL_SCRIPT=${BSD_INSTALL_SCRIPT}
# INSTALL_DATA=${BSD_INSTALL_DATA}

# For Solaris 2 (not SunOS 4), use:
# ---------------------------------
# CPP=/usr/ccs/lib/cpp -P -Dunix -Umsdos
# STRIP=/usr/ccs/bin/strip
# LD=gcc
# DYNLD=ld -G -B symbolic -z nodefs
# ADDRUNCFLAGS=
# ADDDYNLIBCFLAGS=

# If the above fails with dynlibs, try this:
# LD=gcc -Wl,--export-dynamic


# For HP-UX 9, use:
# -----------------
# CPP=/lib/cpp -P -Dunix -Umsdos
# STRIP=strip
# LD=gcc -Xlinker -E
# DYNLD=ld -b -B symbolic -E
# ADDRUNLIBS=-ldld
# ADDRUNCFLAGS=-fPIC
# ADDDYNLIBCFLAGS=-fPIC

# For HP-UX 10, use:
# ------------------
# CPP=/opt/langtools/lbin/cpp -P -Dunix -Umsdos
# STRIP=strip
# LD=gcc -Xlinker -E
# DYNLD=ld -b -B symbolic -E
# ADDRUNLIBS=-ldld
# ADDRUNCFLAGS=-fPIC
# ADDDYNLIBCFLAGS=-fPIC

# For OSF/1 (Digital Unix), use:
# ------------------------------
# CPP=/lib/cpp -P -traditional -Dunix -Umsdos
# STRIP=strip
# LD=gcc
# DYNLD=ld -shared
# ADDRUNLIBS=
# ADDCFLAGS=
# ADDRUNCFLAGS=
# ADDDYNLIBCFLAGS=

# For Irix, use:
# --------------
# CPP=/lib/cpp -P -Dunix -Umsdos
# STRIP=/usr/bin/strip
# LD=gcc -rdynamic
# DYNLD=ld -shared

SHELL=/bin/sh

# ---------- You shouldn't need to edit below this line ------------

# BINDIR contains true executable files, such as scripts
# LIBDIR contains bytecode files (such as mosmlcmp and library units)
# INCDIR contains the runtime system header files (for compiling dynlibs)
# DOCDIR contains documentation

BINDIR=${MOSMLHOME}/bin
LIBDIR=${MOSMLHOME}/lib
INCDIR=${MOSMLHOME}/include
DOCDIR=${MOSMLHOME}/doc
TOOLDIR=${MOSMLHOME}/tools
# LIBDIR=${MOSMLHOME}/lib/moscow_ml
# TOOLDIR=${MOSMLHOME}/libexec/moscow_ml

CAMLRT=../runtime

MOSMLTOOLS=../tools

MOSMLC=../camlrunm ../mosmlcmp -stdlib ../mosmllib -P none
MOSMLL=../camlrunm ../mosmllnk -stdlib ../mosmllib -P none
MOSMLLDOS=../camlrunm ../mosmllnk.dos -stdlib ../mosmllib -P none
MOSMLLW32=../camlrunm ../mosmllnk.w32 -stdlib ../mosmllib -P none
MOSMLLNEW=../camlrunm ../mosmllnk.new -stdlib ../mosmllib -P none

MOSMLLEX=../camlrunm ../mosmllex
MOSMLYACC=../mosmlyac/mosmlyac
MOSMLCUT=../camlrunm ../cutdeps 
MOSMLDEP=../camlrunm ../mosmldep

# For cross-compiling to MS DOS (from Linux) (development only)
# CPP=/lib/cpp -P -traditional -Uunix -Dmsdos -Uwin32

# For cross-compiling to Win 32 (from Linux) (development only)
# CPP=/lib/cpp -P -traditional -Umsdos -Uunix -Dwin32

CC=gcc

.SUFFIXES :
.SUFFIXES : .sml .sig .ui .uo .mlp .lex .c .o

.sig.ui:
	$(MOSMLC) $(COMPFLAGS) $<

.sml.uo:
	$(MOSMLC) $(COMPFLAGS) $<

.mlp.sml:
	$(CPP) $< > $*.sml

.lex.sml:
	$(MOSMLLEX) $<

.c.o:
	$(CC) -c $(CFLAGS) -o $@ $<
