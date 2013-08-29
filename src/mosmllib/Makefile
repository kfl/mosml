# Unix Makefile for Moscow ML library

# Library is compiled with -imptypes, even when used with value polymorphism
COMPFLAGS=-imptypes

include ../Makefile.inc

# Make with the supplied compiler

all: Array.uo Array2.uo ArraySlice.uo Arraysort.uo \
     BasicIO.uo Binaryset.uo Binarymap.uo BinIO.uo Bool.uo Buffer.uo Byte.uo \
     Callback.uo Char.uo CharArray.uo CharArraySlice.uo \
     CharVector.uo CharVectorSlice.uo CommandLine.uo \
     Date.uo Dynarray.uo Dynlib.uo \
     FileSys.uo Gdbm.uo Gdimage.uo Hashset.uo Help.uo \
     Int.uo Intmap.uo Intset.uo IO.uo \
     Lexing.uo List.uo ListPair.uo Listsort.uo Location.uo \
     Math.uo Misc.uo Mosml.uo Mosmlcgi.uo Mosmlcookie.uo Msp.uo Mysql.uo \
     NJ93.uo Nonstdio.uo \
     Obj.uo Option.uo OS.uo \
     Parsing.uo Path.uo Polygdbm.uo Polyhash.uo Postgres.uo PP.uo Process.uo \
     Random.uo Rbset.uo Real.uo Redblackmap.uo Regex.uo \
     Signal.uo SML90.uo Socket.uo Splaymap.uo Splayset.uo Splaytree.uo \
     Strbase.uo String.uo StringCvt.uo Substring.uo Susp.uo \
     TextIO.uo Time.uo Timer.uo \
     Unix.uo Vector.uo VectorSlice.uo \
     Weak.uo Word.uo Word8.uo Word8Array.uo Word8ArraySlice.uo \
     Word8Vector.uo Word8VectorSlice.uo

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
	${INSTALL_DATA} README $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} *.ui $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} *.uo $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} *.sig $(DESTDIR)$(LIBDIR)
	${INSTALL_DATA} General.fke $(DESTDIR)$(LIBDIR)/General.sig
	${INSTALL_DATA} Meta.fke $(DESTDIR)$(LIBDIR)/Meta.sig
#	rm -f $(DESTDIR)$(LIBDIR)/camlrunm
#	ln -s $(BINDIR)/camlrunm $(LIBDIR)/camlrunm

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
CharArray.uo: CharArray.ui CharVector.ui Word8Array.ui Char.ui 
Redblackset.uo: Redblackset.ui List.ui 
NJ93.uo: NJ93.ui String.ui List.ui BasicIO.ui TextIO.ui 
Char.uo: Char.ui Strbase.ui 
Binarymap.uo: Binarymap.ui 
Splayset.uo: Splayset.ui List.ui Splaytree.ui 
ArraySlice.ui: Vector.ui Array.ui VectorSlice.ui 
Word.ui: StringCvt.ui 
Postgres.ui: Msp.ui Date.ui Word8Array.ui Vector.ui 
Msp.uo: Msp.ui String.ui StringCvt.ui List.ui Option.ui Vector.ui TextIO.ui \
    Int.ui Mosmlcgi.ui Char.ui 
Substring.uo: Substring.ui String.ui Strbase.ui 
Timer.ui: Time.ui 
Mosml.uo: Mosml.ui Timer.ui FileSys.ui BinIO.ui String.ui List.ui Vector.ui \
    Word8.ui Process.ui Byte.ui TextIO.ui Word8Vector.ui Time.ui 
Parsing.uo: Parsing.ui Lexing.ui Vector.ui Obj.uo 
Array.uo: Array.ui List.ui Vector.ui 
Word8Vector.ui: Word8.ui 
Math.uo: Math.ui 
OS.uo: OS.ui 
Splaymap.uo: Splaymap.ui Splaytree.ui 
CharVector.uo: CharVector.ui String.ui Word8Vector.ui Char.ui 
CharArraySlice.uo: CharArraySlice.ui CharVector.ui CharArray.ui \
    Word8ArraySlice.ui CharVectorSlice.ui 
Time.ui: StringCvt.ui 
Path.uo: Path.ui String.ui CharVector.ui List.ui Substring.ui 
Word.uo: Word.ui String.ui StringCvt.ui Char.ui 
Intmap.uo: Intmap.ui 
Array.ui: Vector.ui 
Arraysort.ui: Array.ui 
Word8ArraySlice.ui: Word8Array.ui Word8.ui Word8Vector.ui \
    Word8VectorSlice.ui 
Word8Vector.uo: Word8Vector.ui String.ui List.ui Word8.ui 
List.uo: List.ui 
Polyhash.uo: Polyhash.ui Array.ui 
PP.uo: PP.ui String.ui List.ui Vector.ui Array.ui TextIO.ui 
CharArraySlice.ui: CharVector.ui CharArray.ui CharVectorSlice.ui 
Redblackmap.uo: Redblackmap.ui 
Random.uo: Random.ui 
Nonstdio.uo: Nonstdio.ui BasicIO.ui CharArray.ui 
CharVectorSlice.ui: CharVector.ui Substring.ui Char.ui 
BasicIO.uo: BasicIO.ui 
VectorSlice.ui: Vector.ui 
Unix.uo: Unix.ui BinIO.ui Signal.ui Word.ui Dynlib.ui Option.ui Vector.ui \
    OS.ui TextIO.ui Obj.uo 
Byte.uo: Byte.ui String.ui Word8.ui Word8ArraySlice.ui Substring.ui \
    Word8Vector.ui Char.ui Word8VectorSlice.ui 
Byte.ui: String.ui Word8Array.ui Word8.ui Word8ArraySlice.ui Substring.ui \
    Word8Vector.ui Char.ui Word8VectorSlice.ui 
Signal.ui: Word.ui 
Socket.uo: Socket.ui Word.ui Dynlib.ui Word8Array.ui Vector.ui \
    Word8Vector.ui Time.ui Word8VectorSlice.ui 
Parsing.ui: Lexing.ui Vector.ui Obj.uo 
Nonstdio.ui: BasicIO.ui CharArray.ui Char.ui 
Polygdbm.uo: Polygdbm.ui List.ui Gdbm.ui 
Process.uo: Process.ui List.ui BasicIO.ui Time.ui 
TextIO.ui: StringCvt.ui Char.ui 
Word8Array.ui: Word8.ui Word8Vector.ui 
Time.uo: Time.ui Real.ui StringCvt.ui Char.ui 
Gdbm.uo: Gdbm.ui Dynlib.ui List.ui 
Signal.uo: Signal.ui Word.ui 
Lexing.uo: Lexing.ui CharArray.ui Obj.uo 
Misc.uo: Misc.ui String.ui List.ui Option.ui Vector.ui Array.ui TextIO.ui \
    Char.ui 
Polygdbm.ui: Gdbm.ui 
Mysql.uo: Mysql.ui String.ui Real.ui Dynlib.ui StringCvt.ui Msp.ui List.ui \
    Date.ui Option.ui Word8Array.ui Vector.ui Substring.ui Int.ui 
Date.ui: StringCvt.ui Time.ui 
Lexing.ui: CharArray.ui Obj.uo 
Word8Array.uo: Word8Array.ui List.ui Word8.ui Word8Vector.ui 
Unix.ui: BinIO.ui Signal.ui OS.ui TextIO.ui 
Dynarray.uo: Dynarray.ui Array.ui 
Word8.ui: Word.ui StringCvt.ui 
Date.uo: Date.ui String.ui Real.ui StringCvt.ui Option.ui Vector.ui Int.ui \
    Time.ui Char.ui 
Word8VectorSlice.ui: Word8.ui Word8Vector.ui 
CharVectorSlice.uo: CharVectorSlice.ui CharVector.ui Substring.ui Char.ui \
    Word8VectorSlice.ui 
BinIO.ui: Word8.ui Word8Vector.ui 
Bool.uo: Bool.ui StringCvt.ui Char.ui 
Susp.uo: Susp.ui 
Mosmlcookie.uo: Mosmlcookie.ui String.ui List.ui Date.ui Option.ui \
    Process.ui Substring.ui Bool.ui 
Postgres.uo: Postgres.ui String.ui Real.ui Dynlib.ui StringCvt.ui Msp.ui \
    List.ui Date.ui Option.ui Word8Array.ui Vector.ui Substring.ui Int.ui \
    Bool.ui 
Binaryset.uo: Binaryset.ui List.ui 
Hashset.uo: Hashset.ui List.ui Array.ui 
Rbset.uo: Rbset.ui List.ui Int.ui 
CommandLine.uo: CommandLine.ui Vector.ui 
TextIO.uo: TextIO.ui String.ui Char.ui 
Location.ui: BasicIO.ui Lexing.ui 
AppleScript.uo: AppleScript.ui 
Timer.uo: Timer.ui Time.ui 
VectorSlice.uo: VectorSlice.ui Vector.ui 
Int.uo: Int.ui String.ui StringCvt.ui Char.ui 
CharArray.ui: CharVector.ui Char.ui 
Word8VectorSlice.uo: Word8VectorSlice.ui Word8.ui Word8Vector.ui 
Array2.uo: Array2.ui List.ui Vector.ui Array.ui VectorSlice.ui \
    ArraySlice.ui 
StringCvt.uo: StringCvt.ui 
Arraysort.uo: Arraysort.ui Array.ui 
Misc.ui: Array.ui 
String.uo: String.ui List.ui Strbase.ui Char.ui 
Weak.uo: Weak.ui 
Location.uo: Location.ui CharVector.ui Parsing.ui BasicIO.ui Nonstdio.ui \
    Lexing.ui 
ListPair.uo: ListPair.ui List.ui 
Mosml.ui: Word8Vector.ui 
String.ui: Char.ui 
ArraySlice.uo: ArraySlice.ui Vector.ui Array.ui VectorSlice.ui 
Array2.ui: Vector.ui 
Callback.uo: Callback.ui Polyhash.ui 
Regex.uo: Regex.ui Word.ui Dynlib.ui List.ui Vector.ui Substring.ui 
Mysql.ui: Msp.ui Date.ui Word8Array.ui Vector.ui 
CharVector.ui: Char.ui 
BinIO.uo: BinIO.ui Word8.ui TextIO.ui Word8Vector.ui 
Real.ui: StringCvt.ui 
Int.ui: StringCvt.ui 
Word8ArraySlice.uo: Word8ArraySlice.ui Word8Array.ui Word8.ui \
    Word8Vector.ui Word8VectorSlice.ui 
Intset.uo: Intset.ui List.ui 
SML90.uo: SML90.ui String.ui BasicIO.ui 
Dynlib.uo: Dynlib.ui 
Bool.ui: StringCvt.ui 
Listsort.uo: Listsort.ui List.ui 
Strbase.uo: Strbase.ui List.ui 
Vector.uo: Vector.ui List.ui 
FileSys.ui: Time.ui 
Splaytree.uo: Splaytree.ui 
Real.uo: Real.ui StringCvt.ui Char.ui 
Buffer.uo: Buffer.ui String.ui Substring.ui 
Word8.uo: Word8.ui Word.ui String.ui StringCvt.ui Char.ui 
Option.uo: Option.ui 
Mosmlcgi.uo: Mosmlcgi.ui String.ui StringCvt.ui List.ui Option.ui \
    Process.ui Substring.ui Splaymap.ui TextIO.ui Int.ui Char.ui 
FileSys.uo: FileSys.ui Path.ui List.ui Time.ui 
Help.uo: Help.ui String.ui StringCvt.ui List.ui BasicIO.ui Vector.ui \
    TextIO.ui Char.ui 
Process.ui: Time.ui 
Mosmlcookie.ui: Date.ui 
Socket.ui: Word8Array.ui Word8Vector.ui Time.ui 
Gdimage.uo: Gdimage.ui Dynlib.ui Vector.ui 
