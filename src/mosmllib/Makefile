# Unix Makefile for Moscow ML library

# Library is compiled with -imptypes, even when used with value polymorphism
COMPFLAGS=-imptypes

include ../Makefile.inc

# Make with the supplied compiler

all: Array.uo Array2.uo Arraysort.uo \
     BasicIO.uo Binaryset.uo Binarymap.uo BinIO.uo Bool.uo Byte.uo \
     Callback.uo Char.uo CharArray.uo CharVector.uo CommandLine.uo \
     Date.uo Dynarray.uo Dynlib.uo \
     FileSys.uo Gdbm.uo Gdimage.uo Help.uo \
     Int.uo Intmap.uo Intset.uo IO.uo \
     Lexing.uo List.uo ListPair.uo Listsort.uo Location.uo \
     Math.uo Misc.uo Mosml.uo Mosmlcgi.uo Mosmlcookie.uo Msp.uo Mysql.uo \
     NJ93.uo Nonstdio.uo \
     Obj.uo Option.uo OS.uo \
     Parsing.uo Path.uo Polygdbm.uo Polyhash.uo Postgres.uo PP.uo Process.uo \
     Random.uo Real.uo Regex.uo \
     Signal.uo SML90.uo Socket.uo Splaymap.uo Splayset.uo Splaytree.uo \
     Strbase.uo String.uo StringCvt.uo Substring.uo Susp.uo \
     TextIO.uo Time.uo Timer.uo \
     Unix.uo Vector.uo \
     Weak.uo Word.uo Word8.uo Word8Array.uo Word8Vector.uo

# Make with the current compiler
current:
	make clean
	make all MOSMLC=../camlrunm\ ../compiler/mosmlcmp\ -stdlib\ .\ -P\ none

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -f Array.sml FileSys.sml Help.sml Int.sml Mosml.sml
	rm -f Path.sml Process.sml Strbase.sml 
	rm -f Vector.sml Word.sml Word8Array.sml Word8Vector.sml Weak.sml

install:
	${INSTALL_DATA} README $(LIBDIR)
	${INSTALL_DATA} *.ui $(LIBDIR)
	${INSTALL_DATA} *.uo $(LIBDIR)
	${INSTALL_DATA} *.sig $(LIBDIR)
	${INSTALL_DATA} General.fke $(LIBDIR)/General.sig
	${INSTALL_DATA} Meta.fke $(LIBDIR)/Meta.sig
	rm -f $(LIBDIR)/camlrunm
	ln -s $(BINDIR)/camlrunm $(LIBDIR)/camlrunm

depend: Array.sml FileSys.sml Help.sml Int.sml Mosml.sml Path.sml \
    Process.sml Strbase.sml Vector.sml Word.sml Word8Array.sml \
    Word8Vector.sml Weak.sml
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLCUT) < Makefile.bak > Makefile
	$(MOSMLDEP) >> Makefile

# mosmldep does not see that OS depends on FileSys, Path, Process:

OS.ui: FileSys.ui Path.ui Process.ui

### DO NOT DELETE THIS LINE
Word8Vector.uo: Word8Vector.ui List.ui Word8.ui 
Word8Array.uo: Word8Array.ui List.ui Word8.ui Word8Vector.ui 
Word.uo: Word.ui String.ui StringCvt.ui Char.ui 
Weak.uo: Weak.ui 
Vector.uo: Vector.ui List.ui 
Strbase.uo: Strbase.ui List.ui 
Process.uo: Process.ui List.ui BasicIO.ui 
Path.uo: Path.ui String.ui List.ui Substring.ui 
Mosml.uo: Mosml.ui Timer.ui FileSys.ui BinIO.ui String.ui List.ui Vector.ui \
    Word8.ui Process.ui Byte.ui TextIO.ui Word8Vector.ui Time.ui 
Int.uo: Int.ui String.ui StringCvt.ui Char.ui 
Help.uo: Help.ui String.ui StringCvt.ui List.ui BasicIO.ui Vector.ui \
    TextIO.ui Char.ui 
OS.uo: OS.ui 
FileSys.uo: FileSys.ui Path.ui List.ui OS.ui Time.ui 
Array.uo: Array.ui List.ui Vector.ui 
Gdimage.uo: Gdimage.ui Dynlib.ui Vector.ui 
Signal.uo: Signal.ui Word.ui 
Signal.ui: Word.ui 
Unix.uo: Unix.ui Signal.ui Word.ui Dynlib.ui Option.ui Vector.ui Process.ui \
    TextIO.ui Obj.uo 
Unix.ui: Signal.ui Process.ui TextIO.ui 
Socket.uo: Socket.ui Word.ui Dynlib.ui Word8Array.ui Vector.ui \
    Word8Vector.ui Time.ui 
Socket.ui: Word8Array.ui Word8Vector.ui Time.ui 
Regex.uo: Regex.ui Word.ui Dynlib.ui List.ui Vector.ui Substring.ui 
Mysql.uo: Mysql.ui String.ui Real.ui Dynlib.ui StringCvt.ui Msp.ui List.ui \
    Date.ui Option.ui Vector.ui Substring.ui Int.ui 
Mysql.ui: Msp.ui Date.ui Vector.ui 
Postgres.uo: Postgres.ui String.ui Real.ui Dynlib.ui StringCvt.ui Msp.ui \
    List.ui Date.ui Option.ui Word8Array.ui Vector.ui Substring.ui Int.ui \
    Bool.ui 
Postgres.ui: Msp.ui Date.ui Word8Array.ui Vector.ui 
Mosmlcookie.uo: Mosmlcookie.ui String.ui List.ui Date.ui Option.ui \
    Process.ui Substring.ui Bool.ui 
Mosmlcookie.ui: Date.ui 
Msp.uo: Msp.ui String.ui StringCvt.ui List.ui Option.ui Vector.ui TextIO.ui \
    Int.ui Mosmlcgi.ui Char.ui 
Word8Vector.ui: Word8.ui 
Word8Array.ui: Word8.ui Word8Vector.ui 
Word8.uo: Word8.ui Word.ui String.ui StringCvt.ui Char.ui 
Word8.ui: Word.ui StringCvt.ui 
Word.ui: StringCvt.ui 
Timer.uo: Timer.ui Time.ui 
Timer.ui: Time.ui 
Time.uo: Time.ui String.ui StringCvt.ui Char.ui 
Time.ui: StringCvt.ui 
TextIO.uo: TextIO.ui String.ui Char.ui 
TextIO.ui: StringCvt.ui Char.ui 
Susp.uo: Susp.ui 
Substring.uo: Substring.ui String.ui Strbase.ui 
StringCvt.uo: StringCvt.ui 
String.uo: String.ui List.ui Strbase.ui Char.ui 
String.ui: Char.ui 
Splaytree.uo: Splaytree.ui 
Splayset.uo: Splayset.ui List.ui Splaytree.ui 
Splaymap.uo: Splaymap.ui Splaytree.ui 
SML90.uo: SML90.ui String.ui BasicIO.ui 
Real.uo: Real.ui StringCvt.ui Char.ui 
Real.ui: StringCvt.ui 
Random.uo: Random.ui 
Polyhash.uo: Polyhash.ui Array.ui 
Polygdbm.uo: Polygdbm.ui List.ui Gdbm.ui 
Polygdbm.ui: Gdbm.ui 
Parsing.uo: Parsing.ui Lexing.ui Vector.ui Obj.uo 
Parsing.ui: Lexing.ui Vector.ui Obj.uo 
PP.uo: PP.ui String.ui List.ui Vector.ui Array.ui TextIO.ui 
Option.uo: Option.ui 
Nonstdio.uo: Nonstdio.ui BasicIO.ui CharArray.ui 
Nonstdio.ui: BasicIO.ui CharArray.ui Char.ui 
NJ93.uo: NJ93.ui String.ui List.ui BasicIO.ui TextIO.ui 
Mosmlcgi.uo: Mosmlcgi.ui String.ui StringCvt.ui List.ui Option.ui \
    Process.ui Substring.ui Splaymap.ui TextIO.ui Int.ui Char.ui 
Mosml.ui: Word8Vector.ui 
Misc.uo: Misc.ui String.ui List.ui Option.ui Vector.ui Array.ui TextIO.ui \
    Char.ui 
Misc.ui: Array.ui 
Math.uo: Math.ui 
Location.uo: Location.ui CharVector.ui Parsing.ui BasicIO.ui Nonstdio.ui \
    Lexing.ui 
Location.ui: BasicIO.ui Lexing.ui 
Listsort.uo: Listsort.ui List.ui 
ListPair.uo: ListPair.ui List.ui 
List.uo: List.ui 
Lexing.uo: Lexing.ui CharArray.ui Obj.uo 
Lexing.ui: CharArray.ui Obj.uo 
Intset.uo: Intset.ui List.ui 
Intmap.uo: Intmap.ui 
Int.ui: StringCvt.ui 
Gdbm.uo: Gdbm.ui Dynlib.ui List.ui 
FileSys.ui: Time.ui 
Dynlib.uo: Dynlib.ui 
Dynarray.uo: Dynarray.ui Array.ui 
Date.uo: Date.ui String.ui Real.ui StringCvt.ui Option.ui Vector.ui Int.ui \
    Time.ui Char.ui 
Date.ui: StringCvt.ui Time.ui 
CommandLine.uo: CommandLine.ui Vector.ui 
CharVector.uo: CharVector.ui Word8Vector.ui Char.ui 
CharVector.ui: Char.ui 
CharArray.uo: CharArray.ui CharVector.ui Word8Array.ui Char.ui 
CharArray.ui: CharVector.ui Char.ui 
Char.uo: Char.ui Strbase.ui 
Callback.uo: Callback.ui Polyhash.ui 
Byte.uo: Byte.ui String.ui Word8Array.ui Word8.ui Substring.ui \
    Word8Vector.ui Char.ui 
Byte.ui: String.ui Word8Array.ui Word8.ui Substring.ui Word8Vector.ui \
    Char.ui 
Bool.uo: Bool.ui StringCvt.ui Char.ui 
Bool.ui: StringCvt.ui 
Binaryset.uo: Binaryset.ui List.ui 
Binarymap.uo: Binarymap.ui 
BinIO.uo: BinIO.ui Word8.ui TextIO.ui Word8Vector.ui 
BinIO.ui: Word8.ui Word8Vector.ui 
BasicIO.uo: BasicIO.ui 
Arraysort.uo: Arraysort.ui Array.ui 
Arraysort.ui: Array.ui 
Array2.uo: Array2.ui List.ui Vector.ui Array.ui 
Array2.ui: Vector.ui 
Array.ui: Vector.ui 
AppleScript.uo: AppleScript.ui 
