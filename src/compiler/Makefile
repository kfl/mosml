# Unix Makefile for Moscow ML compiler

COMPFLAGS=
# Option `-g' exports the exception table for mosmltop to read:
LINKFLAGS=-g -noheader

include ../Makefile.inc

COMP_OBJS = \
    Miscsys.uo Printexc.uo Filename.uo Fnlib.uo Sort.uo Stack.uo \
    Arg.uo Hasht.uo Memory.uo Config.uo Mixture.uo \
    Const.uo Prim.uo Instruct.uo \
    Lambda.uo Smlprim.uo Globals.uo Location.uo \
    Units.uo Types.uo Smlexc.uo Smlperv.uo Asynt.uo Asyntfn.uo \
    Parser.uo Lexer.uo Primdec.uo Infixst.uo Ovlres.uo \
    Synchk.uo Infixres.uo Elab.uo Tr_env.uo Pr_lam.uo \
    Match.uo Front.uo Back.uo Pr_zam.uo \
    Opcodes.uo Prim_opc.uo Buffcode.uo Labels.uo Code_dec.uo Reloc.uo \
    Emitcode.uo Emit_phr.uo Sigmtch.uo Compiler.uo

C_LIBOBJS= \
    List.uo Strbase.uo Char.uo String.uo BasicIO.uo TextIO.uo \
    Vector.uo Array.uo Misc.uo \
    StringCvt.uo Word.uo Word8.uo Word8Vector.uo CharVector.uo \
    Word8Array.uo CharArray.uo Obj.uo Nonstdio.uo \
    Lexing.uo Parsing.uo PP.uo

C_OBJS= \
    Mainc.uo

L_LIBOBJS= \
    List.uo Strbase.uo Char.uo String.uo BasicIO.uo TextIO.uo \
    Vector.uo Array.uo Misc.uo \
    StringCvt.uo Word.uo Word8.uo Word8Vector.uo CharVector.uo \
    Word8Array.uo CharArray.uo Obj.uo Nonstdio.uo PP.uo

L_OBJS= \
    Miscsys.uo Printexc.uo Filename.uo \
    Fnlib.uo Arg.uo Hasht.uo Config.uo Mixture.uo Const.uo \
    Opcodes.uo Code_dec.uo \
    Predef.uo Prim_c.uo Symtable.uo Patch.uo Tr_const.uo \
    Link.uo Readword.uo Mainl.uo

T_LIBOBJS= \
    List.uo Strbase.uo Char.uo String.uo BasicIO.uo TextIO.uo \
    Vector.uo Array.uo Misc.uo \
    StringCvt.uo Word.uo Word8.uo Word8Vector.uo CharVector.uo \
    Word8Array.uo CharArray.uo Obj.uo Nonstdio.uo \
    Substring.uo Path.uo Time.uo OS.uo FileSys.uo \
    Lexing.uo Parsing.uo PP.uo

T_OBJS= \
    Predef.uo Prim_c.uo Symtable.uo Patch.uo Tr_const.uo \
    Rtvals.uo Load_phr.uo Exec_phr.uo Smltop.uo Maint.uo

all: mosmlcmp mosmllnk mosmltop

dos: mosmlcmp.dos mosmllnk.dos mosmltop.dos

w32: mosmlcmp.w32 mosmllnk.w32 mosmltop.w32
	echo 'Remember to set the CPP for win32 in Makefile.inc'

mosmlcmp: $(COMP_OBJS) $(C_OBJS)
	#$(MOSMLL) $(LINKFLAGS) -o mosmlcmp $(C_LIBOBJS) $(COMP_OBJS) Mainc.uo
	$(MOSMLL) $(LINKFLAGS) -o mosmlcmp Mainc.uo

mosmlcmp.dos: $(COMP_OBJS) $(C_OBJS)
	$(MOSMLLDOS) $(LINKFLAGS) -o mosmlcmp Mainc.uo

mosmlcmp.w32: $(COMP_OBJS) $(C_OBJS)
	$(MOSMLL) $(LINKFLAGS) -o mosmlcmp.w32 Mainc.uo

Opcodes.sml: $(CAMLRT)/instruct.h
	$(PERL) $(MOSMLTOOLS)/mksmlopc $(CAMLRT)/instruct.h > Opcodes.sml

Parser.sml Parser.sig: Parser.grm
	$(MOSMLYACC) -v Parser.grm

mosmllnk: $(L_OBJS)
	#$(MOSMLL) $(LINKFLAGS) -o mosmllnk $(L_LIBOBJS) $(L_OBJS) 
	$(MOSMLL) $(LINKFLAGS) -o mosmllnk Mainl.uo

mosmllnk.dos: $(L_OBJS)
	$(MOSMLLDOS) $(LINKFLAGS) -o mosmllnk Mainl.uo

mosmllnk.w32: $(L_OBJS)
	$(MOSMLL) $(LINKFLAGS) -o mosmllnk.w32 Mainl.uo

Predef.sml : $(CAMLRT)/globals.h 
	$(PERL) $(MOSMLTOOLS)/mksmlpre $(CAMLRT)/globals.h > Predef.sml

Prim_c.sml : $(CAMLRT)/primitives
	$(PERL) $(MOSMLTOOLS)/mksmlprc $(CAMLRT)/primitives > Prim_c.sml

mosmltop: $(COMP_OBJS) $(T_OBJS)
	#$(MOSMLL) $(LINKFLAGS) -o mosmltop $(T_LIBOBJS) $(COMP_OBJS) $(T_OBJS) 
	$(MOSMLL) $(LINKFLAGS) -o mosmltop Maint.uo 

mosmltop.dos: $(COMP_OBJS) $(T_OBJS)
	$(MOSMLLDOS) $(LINKFLAGS) -o mosmltop Maint.uo

mosmltop.w32: $(COMP_OBJS) $(T_OBJS)
	$(MOSMLL) $(LINKFLAGS) -o mosmltop.w32 Maint.uo

clean:
	rm -f mosmlcmp
	rm -f mosmllnk
	rm -f mosmltop
	rm -f *.ui
	rm -f *.uo
	rm -f Opcodes.sml
	rm -f Parser.sml
	rm -f Parser.sig
	rm -f Parser.output
	rm -f Lexer.sml
	rm -f Config.sml
	rm -f Filename.sml
	rm -f Predef.sml
	rm -f Prim_c.sml
	rm -f Makefile.bak

install:
	${INSTALL_DATA} mosmlcmp $(LIBDIR)
	${INSTALL_DATA} mosmllnk $(LIBDIR)
	${INSTALL_DATA} mosmltop $(LIBDIR)

promote:
	test -f ../mosmlcmp.orig || cp ../mosmlcmp ../mosmlcmp.orig
	test -f ../mosmllnk.orig || cp ../mosmllnk ../mosmllnk.orig
	test -f ../mosmllex.orig || cp ../mosmllex ../mosmllex.orig
	test -f ../mosmllib/Callback.sml.orig || (echo "patching Callback.sml";mv ../mosmllib/Callback.sml ../mosmllib/Callback.sml.orig;cp patch/Callback.sml ../mosmllib/Callback.sml)
	#$(MOSMLL) $(LINKFLAGS) -o mosmlcmp $(C_LIBOBJS) $(COMP_OBJS) $(C_OBJS)
	#$(MOSMLL) $(LINKFLAGS) -o mosmllnk $(L_LIBOBJS) $(L_OBJS) 
	#$(MOSMLL) $(LINKFLAGS) -o mosmltop $(T_LIBOBJS) $(T_OBJS) 
	test -f mosmllnk && cp mosmllnk ../mosmllnk
	test -f mosmlcmp && cp mosmlcmp ../mosmlcmp
	test -f ../lex/mosmllex && cp ../lex/mosmllex ../mosmllex

distribution:
	cvs diff || (echo "!!!There are uncommitted changes" && false)
	cd /tmp;tar -zxvf ${HOME}/mos144src.tar.gz 
	cd /tmp/mosml/src;tar -zcf compiler.orig compiler;rm -r -f /tmp/mosml/src/compiler;cvs export -D today compiler
	cd /tmp;tar -zcf mos144cvr3src.tar.gz mosml

revert:
	test -f ../mosmlcmp.orig && (rm ../mosmlcmp;mv ../mosmlcmp.orig ../mosmlcmp)
	test -f ../mosmllnk.orig && (rm ../mosmllnk;mv ../mosmllnk.orig ../mosmllnk)
	test -f ../mosmllex.orig && (rm ../mosmllex;mv ../mosmllex.orig ../mosmllex)
	test -f ../mosmllib/Callback.sml.orig && (rm ../mosmllib/Callback.sml;mv ../mosmllib/Callback.sml.orig ../mosmllib/Callback.sml)

depend: Filename.sml Config.sml Opcodes.sml Parser.sml Parser.sig Lexer.sml \
    Predef.sml Prim_c.sml
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLCUT) < Makefile.bak > Makefile
	$(MOSMLDEP) >> Makefile

### DO NOT DELETE THIS LINE
Predef.uo: Const.uo 
Lexer.uo: Lexer.ui Parser.ui Const.uo Fnlib.ui Config.uo Stack.ui \
    Mixture.ui Hasht.ui Memory.uo 
Parser.ui: Asynt.uo Const.uo 
Parser.uo: Parser.ui Asynt.uo Const.uo Fnlib.ui Config.uo Types.ui \
    Asyntfn.ui Globals.uo Location.ui Mixture.ui 
Config.uo: Fnlib.ui 
Filename.uo: Filename.ui 
Units.uo: Units.ui Const.uo Fnlib.ui Config.uo Globals.uo Location.ui \
    Mixture.ui Hasht.ui Filename.ui 
Units.ui: Const.uo Fnlib.ui Globals.uo Location.ui Mixture.ui Hasht.ui 
Types.uo: Types.ui Const.uo Fnlib.ui Config.uo Globals.uo Smlprim.uo \
    Location.ui Mixture.ui Units.ui Hasht.ui 
Types.ui: Const.uo Fnlib.ui Globals.uo Smlprim.uo Location.ui Mixture.ui \
    Units.ui 
Tr_env.uo: Tr_env.ui Asynt.uo Const.uo Fnlib.ui Prim.uo Types.ui Asyntfn.ui \
    Globals.uo Mixture.ui Units.ui Hasht.ui Lambda.uo 
Tr_env.ui: Asynt.uo Const.uo Mixture.ui Lambda.uo 
Tr_const.uo: Const.uo Symtable.ui 
Synchk.uo: Synchk.ui Asynt.uo Const.uo Fnlib.ui Asyntfn.ui Globals.uo \
    Location.ui Mixture.ui Units.ui 
Synchk.ui: Asynt.uo 
Symtable.uo: Symtable.ui Const.uo Fnlib.ui Config.uo Predef.uo Prim_c.uo \
    Mixture.ui Hasht.ui Miscsys.ui 
Symtable.ui: Const.uo 
Stack.uo: Stack.ui 
Sort.uo: Sort.ui 
Smltop.uo: Smltop.ui Compiler.ui Const.uo Fnlib.ui Patch.uo Emit_phr.uo \
    Rtvals.ui Config.uo Code_dec.uo Lexer.ui Types.ui Globals.uo Smlprim.uo \
    Smlexc.uo Smlperv.ui Opcodes.uo Location.ui Symtable.ui Emitcode.ui \
    Mixture.ui Units.ui Load_phr.ui Hasht.ui Miscsys.ui Memory.uo \
    Filename.ui Exec_phr.ui 
Smlprim.uo: Const.uo Prim.uo 
Smlperv.uo: Smlperv.ui Const.uo Fnlib.ui Prim.uo Types.ui Globals.uo \
    Smlprim.uo Smlexc.uo Units.ui Hasht.ui 
Smlexc.uo: Const.uo Fnlib.ui Config.uo Types.ui Mixture.ui 
Sigmtch.uo: Sigmtch.ui Front.ui Const.uo Back.ui Fnlib.ui Emit_phr.uo \
    Prim.uo Types.ui Globals.uo Mixture.ui Units.ui Hasht.ui Lambda.uo 
Sigmtch.ui: Units.ui 
Rtvals.uo: Rtvals.ui Const.uo Fnlib.ui Config.uo Types.ui Globals.uo \
    Smlexc.uo Symtable.ui Mixture.ui Units.ui Miscsys.ui Memory.uo 
Rtvals.ui: Const.uo Types.ui Globals.uo 
Reloc.uo: Const.uo Buffcode.uo Code_dec.uo Hasht.ui 
Printexc.uo: Printexc.ui 
Primdec.uo: Const.uo Fnlib.ui Prim.uo Smlprim.uo 
Prim_opc.uo: Fnlib.ui Prim.uo Opcodes.uo 
Prim.uo: Const.uo 
Pr_zam.uo: Asynt.uo Const.uo Fnlib.ui Config.uo Pr_lam.ui Instruct.uo \
    Mixture.ui 
Pr_lam.uo: Pr_lam.ui Asynt.uo Const.uo Prim.uo Mixture.ui Lambda.uo 
Pr_lam.ui: Prim.uo Lambda.uo 
Patch.uo: Code_dec.uo Symtable.ui 
Ovlres.uo: Ovlres.ui Asynt.uo Const.uo Fnlib.ui Prim.uo Types.ui Globals.uo \
    Smlprim.uo Location.ui Mixture.ui Units.ui 
Ovlres.ui: Asynt.uo 
Mixture.uo: Mixture.ui Fnlib.ui Config.uo Hasht.ui Miscsys.ui Filename.ui 
Mixture.ui: Hasht.ui 
Miscsys.uo: Miscsys.ui 
Match.uo: Match.ui Asynt.uo Const.uo Fnlib.ui Prim.uo Asyntfn.ui Tr_env.ui \
    Location.ui Mixture.ui Hasht.ui Lambda.uo 
Match.ui: Asynt.uo Tr_env.ui Location.ui Lambda.uo 
Maint.uo: Maint.ui Compiler.ui Fnlib.ui Rtvals.ui Config.uo Types.ui Arg.ui \
    Printexc.ui Smlperv.ui Location.ui Smltop.ui Mixture.ui Units.ui \
    Miscsys.ui Memory.uo Exec_phr.ui 
Mainl.uo: Fnlib.ui Config.uo Arg.ui Printexc.ui Link.ui Symtable.ui \
    Readword.uo Mixture.ui Miscsys.ui Filename.ui 
Mainc.uo: Mainc.ui Compiler.ui Fnlib.ui Config.uo Lexer.ui Types.ui Arg.ui \
    Printexc.ui Smlperv.ui Location.ui Mixture.ui Units.ui Miscsys.ui \
    Filename.ui 
Location.uo: Location.ui Fnlib.ui Config.uo Mixture.ui 
Load_phr.uo: Load_phr.ui Const.uo Reloc.uo Fnlib.ui Buffcode.uo Patch.uo \
    Rtvals.ui Types.ui Opcodes.uo Symtable.ui Labels.uo Emitcode.ui \
    Instruct.uo Tr_const.uo Mixture.ui Memory.uo 
Load_phr.ui: Instruct.uo 
Link.uo: Link.ui Const.uo Fnlib.ui Patch.uo Config.uo Code_dec.uo \
    Opcodes.uo Symtable.ui Tr_const.uo Mixture.ui Hasht.ui Miscsys.ui \
    Filename.ui 
Lexer.ui: Parser.ui 
Lambda.uo: Const.uo Prim.uo Instruct.uo 
Labels.uo: Fnlib.ui Buffcode.uo Instruct.uo 
Instruct.uo: Const.uo Config.uo Prim.uo 
Infixst.uo: Infixst.ui Asynt.uo Const.uo Fnlib.ui Globals.uo Location.ui \
    Mixture.ui 
Infixst.ui: Asynt.uo Fnlib.ui Globals.uo Location.ui Mixture.ui 
Infixres.uo: Infixres.ui Asynt.uo Primdec.uo Const.uo Fnlib.ui Infixst.ui \
    Types.ui Asyntfn.ui Globals.uo Smlprim.uo Smlexc.uo Location.ui \
    Mixture.ui Units.ui 
Infixres.ui: Asynt.uo Globals.uo 
Hasht.uo: Hasht.ui 
Globals.uo: Const.uo Fnlib.ui Smlprim.uo Mixture.ui 
Front.uo: Front.ui Asynt.uo Const.uo Fnlib.ui Config.uo Prim.uo Types.ui \
    Asyntfn.ui Globals.uo Smlprim.uo Smlexc.uo Tr_env.ui Location.ui \
    Match.ui Mixture.ui Units.ui Lambda.uo 
Front.ui: Asynt.uo Globals.uo Smlprim.uo Tr_env.ui Lambda.uo 
Fnlib.uo: Fnlib.ui 
Exec_phr.uo: Exec_phr.ui Asynt.uo Front.ui Compiler.ui Const.uo Back.ui \
    Fnlib.ui Pr_zam.uo Infixst.ui Elab.ui Rtvals.ui Types.ui Ovlres.ui \
    Globals.uo Tr_env.ui Symtable.ui Mixture.ui Units.ui Load_phr.ui \
    Infixres.ui Miscsys.ui 
Exec_phr.ui: Asynt.uo 
Emitcode.uo: Emitcode.ui Const.uo Reloc.uo Fnlib.ui Buffcode.uo Config.uo \
    Prim.uo Opcodes.uo Labels.uo Instruct.uo Mixture.ui Prim_opc.uo 
Emitcode.ui: Instruct.uo 
Emit_phr.uo: Const.uo Reloc.uo Buffcode.uo Code_dec.uo Labels.uo \
    Emitcode.ui Instruct.uo Mixture.ui 
Elab.uo: Elab.ui Asynt.uo Primdec.uo Synchk.ui Const.uo Fnlib.ui Config.uo \
    Types.ui Asyntfn.ui Globals.uo Smlprim.uo Smlexc.uo Location.ui \
    Mixture.ui Units.ui 
Elab.ui: Asynt.uo Globals.uo 
Const.uo: Fnlib.ui Config.uo Mixture.ui 
Compiler.uo: Compiler.ui Asynt.uo Front.ui Parser.ui Const.uo Back.ui \
    Fnlib.ui Pr_zam.uo Elab.ui Emit_phr.uo Lexer.ui Config.uo Sigmtch.ui \
    Types.ui Ovlres.ui Globals.uo Smlperv.ui Tr_env.ui Location.ui \
    Mixture.ui Units.ui Infixres.ui Hasht.ui 
Compiler.ui: Asynt.uo Globals.uo Mixture.ui 
Code_dec.uo: Const.uo Mixture.ui Hasht.ui 
Buffcode.uo: Fnlib.ui Config.uo Opcodes.uo Mixture.ui 
Back.uo: Back.ui Sort.ui Const.uo Fnlib.ui Prim.uo Instruct.uo Stack.ui \
    Mixture.ui Lambda.uo 
Back.ui: Instruct.uo Lambda.uo 
Asyntfn.uo: Asyntfn.ui Asynt.uo Const.uo Fnlib.ui Types.ui Globals.uo \
    Location.ui Mixture.ui 
Asyntfn.ui: Asynt.uo Const.uo Fnlib.ui Types.ui Globals.uo Location.ui \
    Mixture.ui 
Arg.uo: Arg.ui Fnlib.ui Miscsys.ui 
Asynt.uo: Const.uo Fnlib.ui Types.ui Globals.uo Location.ui Mixture.ui \
    Lambda.uo 
