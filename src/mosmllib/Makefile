# Unix Makefile for Moscow ML library

# Library is compiled with -imptypes, even when used with value polymorphism
COMPFLAGS=-imptypes

include ../Makefile.inc


# with the supplied compiler
all: Array.uo Array2.uo Arraysort.uo \
     BasicIO.uo Binaryset.uo Binarymap.uo BinIO.uo Bool.uo Byte.uo \
     Callback.uo Char.uo CharArray.uo CharVector.uo CommandLine.uo Date.uo \
     Dynarray.uo Dynlib.uo FileSys.uo Help.uo Int.uo Intmap.uo Intset.uo \
     List.uo ListPair.uo Listsort.uo Location.uo \
     Math.uo Misc.uo Mosml.uo NJ93.uo Option.uo \
     OS.uo Path.uo Polyhash.uo PP.uo Process.uo Random.uo Real.uo \
     SML90.uo Splaymap.uo Splayset.uo Splaytree.uo \
     Strbase.uo String.uo StringCvt.uo Substring.uo Susp.uo \
     TextIO.uo Time.uo Timer.uo Vector.uo \
     Weak.uo Word.uo Word8.uo Word8Array.uo Word8Vector.uo \
     Obj.uo Nonstdio.uo Lexing.uo Parsing.uo Mosmlcgi.uo Gdbm.uo Polygdbm.uo

# with the current compiler
current:
	make clean
	make all MOSMLC=../camlrunm\ ../compiler/mosmlcmp\ -stdlib\ .\ -P\ none

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak
	rm -f Array.sml FileSys.sml Help.sml Int.sml Mosml.sml
	rm -f OS.sml Path.sml Process.sml Strbase.sml 
	rm -f Vector.sml Word.sml Word8Array.sml Word8Vector.sml Weak.sml

install:
	${INSTALL_DATA} README $(LIBDIR)
	${INSTALL_DATA} *.ui $(LIBDIR)
	${INSTALL_DATA} *.uo $(LIBDIR)
	${INSTALL_DATA} *.sig $(LIBDIR)
	${INSTALL_DATA} General.fke $(LIBDIR)/General.sig
	${INSTALL_DATA} Meta.fke $(LIBDIR)/Meta.sig
	${INSTALL_DATA} OS.fke $(LIBDIR)/OS.sig
	${INSTALL_DATA} helpsigs.val $(LIBDIR)
	ln -fs $(BINDIR)/camlrunm $(LIBDIR)/camlrunm

depend: Array.sml FileSys.sml Help.sml Int.sml Mosml.sml OS.sml Path.sml \
    Process.sml Strbase.sml Vector.sml Word.sml Word8Array.sml \
    Word8Vector.sml Weak.sml
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLCUT) < Makefile.bak > Makefile
	$(MOSMLDEP) >> Makefile

### DO NOT DELETE THIS LINE
Callback.uo: Callback.ui Polyhash.ui 
Word8Vector.uo: Word8Vector.ui List.ui Word8.ui 
Word8Array.uo: Word8Array.ui List.ui Word8.ui Word8Vector.ui 
Word.uo: Word.ui String.ui StringCvt.ui Char.ui 
Weak.uo: Weak.ui 
Vector.uo: Vector.ui List.ui 
Strbase.uo: Strbase.ui List.ui 
Path.uo: Path.ui String.ui List.ui Substring.ui 
Mosml.uo: Mosml.ui Timer.ui FileSys.ui BinIO.ui String.ui List.ui Vector.ui \
    Word8.ui Process.ui Byte.ui TextIO.ui Word8Vector.ui Time.ui 
Int.uo: Int.ui String.ui StringCvt.ui Char.ui 
Help.uo: Help.ui String.ui StringCvt.ui List.ui BasicIO.ui Vector.ui \
    TextIO.ui Char.ui 
FileSys.uo: FileSys.ui Path.ui List.ui OS.uo Time.ui 
Array.uo: Array.ui List.ui Vector.ui 
AppleScript.uo: AppleScript.ui 
Dynlib.uo: Dynlib.ui 
Polygdbm.uo: Polygdbm.ui List.ui Gdbm.ui 
Polygdbm.ui: Gdbm.ui 
Gdbm.uo: Gdbm.ui Dynlib.ui List.ui 
CommandLine.uo: CommandLine.ui Vector.ui 
SML90.uo: SML90.ui String.ui BasicIO.ui 
Option.uo: Option.ui 
Location.ui: BasicIO.ui Lexing.ui 
Location.uo: Location.ui CharVector.ui Parsing.ui BasicIO.ui Nonstdio.ui \
    Lexing.ui 
Mosmlcgi.uo: Mosmlcgi.ui String.ui StringCvt.ui List.ui Option.ui \
    Process.ui Substring.ui Splaymap.ui TextIO.ui Int.ui Char.ui 
BinIO.uo: BinIO.ui Word8.ui TextIO.ui Word8Vector.ui 
BinIO.ui: Word8.ui Word8Vector.ui 
TextIO.uo: TextIO.ui String.ui Char.ui 
TextIO.ui: StringCvt.ui Char.ui 
PP.uo: PP.ui String.ui List.ui Vector.ui Array.ui TextIO.ui 
Susp.uo: Susp.ui 
Splaytree.uo: Splaytree.ui 
Intmap.uo: Intmap.ui 
Intset.uo: Intset.ui List.ui 
Array2.uo: Array2.ui List.ui Vector.ui Array.ui 
Array2.ui: Vector.ui 
Random.uo: Random.ui 
Arraysort.ui: Array.ui 
Arraysort.uo: Arraysort.ui Array.ui 
Dynarray.uo: Dynarray.ui Array.ui 
Bool.uo: Bool.ui StringCvt.ui Char.ui 
Bool.ui: StringCvt.ui 
String.uo: String.ui List.ui Strbase.ui Char.ui 
Int.ui: StringCvt.ui 
Mosml.ui: Word8Vector.ui 
Polyhash.uo: Polyhash.ui Array.ui 
Misc.uo: Misc.ui String.ui List.ui Option.ui Vector.ui Array.ui TextIO.ui \
    Char.ui 
Date.uo: Date.ui String.ui Real.ui StringCvt.ui Option.ui Vector.ui Int.ui \
    Time.ui Char.ui 
Timer.ui: Time.ui 
Process.uo: Process.ui List.ui BasicIO.ui 
Listsort.uo: Listsort.ui List.ui 
StringCvt.uo: StringCvt.ui 
Misc.ui: Array.ui 
Timer.uo: Timer.ui Time.ui 
NJ93.uo: NJ93.ui String.ui List.ui BasicIO.ui TextIO.ui 
Splayset.uo: Splayset.ui List.ui Splaytree.ui 
Substring.uo: Substring.ui String.ui Strbase.ui 
Splaymap.uo: Splaymap.ui Splaytree.ui 
Math.uo: Math.ui 
Binaryset.uo: Binaryset.ui List.ui 
Real.uo: Real.ui StringCvt.ui Char.ui 
Binarymap.uo: Binarymap.ui 
ListPair.uo: ListPair.ui List.ui 
Time.uo: Time.ui String.ui StringCvt.ui Char.ui 
Real.ui: StringCvt.ui 
Time.ui: StringCvt.ui 
Word8Vector.ui: Word8.ui 
Word8Array.ui: Word8.ui Word8Vector.ui 
Word8.ui: Word.ui StringCvt.ui 
Char.uo: Char.ui Strbase.ui 
Parsing.ui: Lexing.ui Vector.ui Obj.uo 
Nonstdio.ui: BasicIO.ui CharArray.ui Char.ui 
Word.ui: StringCvt.ui 
Lexing.ui: CharArray.ui Obj.uo 
CharArray.ui: CharVector.ui Char.ui 
CharArray.uo: CharArray.ui CharVector.ui Word8Array.ui Char.ui 
Date.ui: StringCvt.ui Time.ui 
Byte.ui: String.ui Word8Array.ui Word8.ui Substring.ui Word8Vector.ui \
    Char.ui 
BasicIO.uo: BasicIO.ui 
Array.ui: Vector.ui 
Parsing.uo: Parsing.ui Lexing.ui Vector.ui Obj.uo 
Nonstdio.uo: Nonstdio.ui BasicIO.ui CharArray.ui 
List.uo: List.ui 
Lexing.uo: Lexing.ui CharArray.ui Obj.uo 
CharVector.uo: CharVector.ui Word8Vector.ui Char.ui 
CharVector.ui: Char.ui 
Word8.uo: Word8.ui Word.ui String.ui StringCvt.ui Char.ui 
Byte.uo: Byte.ui String.ui Word8Array.ui Word8.ui Substring.ui \
    Word8Vector.ui Char.ui 
FileSys.ui: Time.ui 
String.ui: Char.ui 
