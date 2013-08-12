(* Translate Zam instructions to C--                                         *)
(* Created by Ken Friis Larsen <ken@friislarsen.net> 2001-02-14                       *)
structure CmmEmitcode =
struct
local open CmmAST 
      datatype Instruction = datatype Instruct.ZamInstruction
      datatype SCon = datatype Const.SCon
      datatype BlockTag = datatype Const.BlockTag
      datatype StructConstant = datatype Const.StructConstant
      val fatal = Fnlib.fatalError
      infix o
      fun f o g = fn x => f(g x)

fun tooManyError kind = 
    (Mixture.msgIBlock 0;
     Mixture.errPrompt ("Too many " ^ kind ^ "; unable to generate bytecode");
     Mixture.msgEOL();
     Mixture.msgEBlock();
     raise Mixture.Toplevel);
    

fun checkGlobals n =
    if n > 0xFFFF then tooManyError "globals" else ()

fun checkLocals n =
    if n > 0xFFFF then tooManyError "local variables" else ()

fun checkFields n =
    if n > 0xFFFF then tooManyError "fields" else ()


in


(* Make the C-- life easier *)

fun litInt i     = LitInt (Int.toString i)
fun litWord w    = LitInt (Word.toString w)
fun litChar c    = LitChar (Char.toCString c)
fun litCharInt c = litInt (Char.ord c)

val bits32 = Bits32
val bits8  = Bits8

val reg = Fetch o Var

fun memaccess e = Mem(bits32, e, NONE)
val memread     = Fetch o memaccess
val lvmem       = memaccess

fun ccall n args = Call([], Ccall, reg n, args, [], [])
fun call e args = Call([], Cmm, e, args, [], [])
fun assignCcall regs e args = Call(regs, Ccall, e, args, [], [])
fun assignCall regs e args  = Call(regs, Cmm, e, args, [], [])
fun jump e args = Jump(Cmm, e, args, [])
fun cjump n args = Jump(Ccall, reg n, args, [])

fun binary opr (e1, e2)   = Prim(opr, [e1, e2])
(* for cmmc *)
val shl   = binary "<<"
val shr   = binary ">>"
val add   = binary "+" 
val minus = binary "-" 
val lt    = binary "<"
val le    = binary "<="   
val gt    = binary ">"
val ge    = binary ">="
val eq    = binary "=="   
val ne    = binary "!="

local val temp = ref 0;
in
fun newTemp name = 
    let val s = String.concat["_._", name, Int.toString(!temp)]
    in  temp := !temp + 1;
        (s, DeclReg(bits32, s))
    end

fun resetTemp() = temp := 0
end

local val lab = ref 0;
in
fun newLab name = 
    let val s = String.concat["_._", name, "_", Int.toString(!lab)]
    in  lab := !lab + 1;
        s
    end

fun resetLab() = lab := 0
end
   



(* C-- is not as friendly as C when it comes to working with addresses. *)
(* For now hard wired to 32 bit                                         *)
fun wordSize i = shl (litInt i, litInt 2)
fun wordIncr e = add(e, wordSize 1)
fun wordIndex e = shl (e, litInt 2)

fun addreg r 0 = reg r
  | addreg r i = add(reg r, wordSize i)

fun incr r = Assign(Var r, wordIncr(reg r))
fun incrWith r i = Assign(Var r, addreg r i)

fun wordDecr e     = minus(e, wordSize 1)
fun decr r       = Assign(Var r, wordDecr(reg r))
fun decrWith r i = Assign(Var r, minus(reg r, wordSize i))

(* The real deal *)

fun makeLetName i = "__let_bound"^Int.toString i

fun getLetName (Kname lbl :: _) = makeLetName lbl
  | getLetName _                = fatal "Unnamed let-bound function ?"

fun makeLabel lbl = "__"^Int.toString lbl

(* Mosml primitives *)
fun Atom tag = 
    let val hp = add (memread(reg"first_atoms"), wordSize tag)
    in  EComment(wordIncr hp, "Atom")
    end

val Val_unit = Atom 0
fun Val_long x = add(shl(x, litInt 1), litInt 1)
fun Long_val x = shr(x, litInt 1)

fun Field(x, i) = memaccess(add (x, wordSize i))
fun FieldE(x, e) = memaccess(add (x, wordIndex e))


fun Code_val x = memread x
fun Code_val_lv x = lvmem x

fun Env_val x = Field(x, 1) 

val Max_young_wosize = 256

(* For little endian *)
fun Tag_val x =
    Fetch(Mem(bits8, minus(x, wordSize 1), NONE))

(* For big endian *)
(*fun Tag_val x =
    MemRead(bits8, minus(x, litInt 1), NONE)
*)


(*
#define Wosize_val(val) (Wosize_hd (Hd_val (val)))
#define Hd_val(val) (((header_t * ) (val)) [-1])
#define Wosize_hd(hd) ((mlsize_t) ((hd) >> 10))
*)
fun Wosize_val v =
    let val hp = minus(v, wordSize 1)
    in  shr(memread hp, litInt 10)   (* 32 bits? *)
    end


(* mosml registers *)
val accu = reg"accu"
val sp   = reg"sp"

fun assignAccu e   = Assign(Var "accu", e)
val assignAccuInt  = assignAccu o Val_long o litInt
val assignAccuWord = assignAccu o Val_long o litWord
val assignAccuChar = assignAccu o Val_long o litCharInt

val pushAccu = Block[decr "sp", Assign(lvmem sp, accu)]

fun assignSp n e = Assign(lvmem (addreg "sp" n), e)
fun readSp n = memread (addreg "sp" n)


(* Stacks *)
val setExternsp = Assign(lvmem(reg"extern_sp"), sp)
val getExternsp = Assign(Var"sp", memread(reg"extern_sp"))
val checkStack =
    If(lt(sp, memread(reg"stack_threshold")),
        [setExternsp,
         ccall "realloc_stack" [],
         getExternsp],NONE)

val Setup_for_gc =
    Block[ decrWith "sp" 2
         , assignSp 0 accu
         , assignSp 1 (reg"env")
         , setExternsp
         ]

val Restore_after_gc =
    Block[ assignAccu (readSp 0)
         , Assign(Var"env", readSp 1)
         , incrWith "sp" 2
         ]



fun modify e1 e2 = ccall "modify" [e1, e2]
    
fun getGlobal uid = assignAccu (Fetch(Field(reg"global_data", uid)))

fun getField n =
    assignAccu (Fetch(Field(accu, n)))


(* correspond to r = *sp++ *)
fun readTop r = Block[Assign(Var r, readSp 0), incr "sp"]


(* The following corresponds to:
      modify(&Field( *sp++, n), accu);
      accu = Val_unit; /* Atom(0); */
*)
fun setField n =
    let val (oldsp, decl) = newTemp "oldsp"
        val old = memread(reg oldsp)
        val modify_dest = add(old, wordSize n)
        val modi  = modify modify_dest accu
    in  Block[ decl
             , readTop oldsp
             , modi
             , assignAccu Val_unit
             ]
    end

(* FIXME: Here we should check for signals *)
fun apply n =
    let val extra = Assign(Var"extra_args", litInt (n-1))
        val env   = Assign(Var"env", accu)
        val cp    = Code_val accu
    in  Block[Comment "APPLY START", extra, env,
              checkStack,
              Call(["sp", "accu", "env","extra_args"], Cmm, cp, 
                   [sp, accu, reg"env",reg"extra_args"], [], []),
              Comment "APPLY END"]
    end

val pushRetAddr =
    Block[ decrWith "sp" 2
         , assignSp 0 (reg"env")
         , assignSp 1 (Val_long(reg"extra_args"))
         ]


(* for-loop from 0 to (excluding) n *)
fun forUp n body =
    let val ltop = newLab "FOR_LOOP_TOP"
        val lend = newLab "FOR_LOOP_END"
        val (i, idecl) = newTemp "i"
    in  Block[ idecl
             , Assign(Var i, litInt 0)
             , Goto lend
             , Label ltop
             , body (reg i)
             , Assign(Var i, add(reg i, litInt 1))
             , Label lend
             , If(lt(reg i, n), [Goto ltop], NONE)
             ]
    end

(* for-loop from init to (and including) 0 *)
fun forDown init body =
    let val ltop = newLab "FOR_LOOP_TOP"
        val lend = newLab "FOR_LOOP_END"
        val (i, idecl) = newTemp "i"
    in  Block[ idecl
             , Assign(Var i, init)
             , Goto lend
             , Label ltop
             , body (reg i)
             , Assign(Var i, minus(reg i, litInt 1))
             , Label lend
             , If(ge(reg i, litInt 0), [Goto ltop], NONE)
             ]
    end



(* FIXME: Here we should check for signals *)
fun appterm nargs slotsize =
    let val n = nargs - 1
        val (newsp, newspdecl) = newTemp "newsp"
        val setnewsp = Assign(Var newsp, 
                              minus(addreg "sp" slotsize,
                                    wordSize nargs))
        fun copy i =
            let fun iIndex r = add(reg r, wordIndex i) 
                (* newsp[i] = sp[i] *)
                val copy = Assign(lvmem (iIndex newsp), memread (iIndex "sp"))
            in  copy
            end
        val spass = Assign(Var"sp", reg newsp)
        val extra = incrWith "extra_args" n
        val env   = Assign(Var"env", accu)
        val cp    = Code_val accu 
    in  Block[ Comment "APPTERM START"
             , newspdecl
             , setnewsp
             , forDown (litInt n) copy
             , spass
             , extra
             , env
             , jump cp [sp, accu, reg"env", reg"extra_args"]
             , Comment "APPTERM END"
             ]
    end

(* FIXME: Here we should check for signals *)
fun return n =
    let val incsp = incrWith "sp" n
        val pos = Return(Cmm, [sp, accu, 
                               accu, minus(reg"extra_args", litInt 1)])
        val zero = 
            [Assign(Var"env",        readSp 0),
             Assign(Var"extra_args", Long_val(readSp 1)),
             incrWith "sp" 2,
             Return(Cmm, [sp, accu, reg"env", reg"extra_args"])]
    in  Block[ Comment "RETURN START"
             , incsp
             , If(gt(reg"extra_args", litInt 0), [pos], SOME zero)
             , Comment "RETURN STOP"
             ]
    end

fun alloc tmp n t = assignCcall [tmp] (reg"alloc") [n, litInt t]
fun Alloc_small tmp n t = 
    Block[ Setup_for_gc
         , alloc tmp (litInt n) t
         , Restore_after_gc
         ]
fun Alloc_smallE tmp n t = 
    Block[ Setup_for_gc
         , alloc tmp n t
         , Restore_after_gc
         ]

fun makeblock (CONtag(t,_)) n =
    (*if n < Max_young_wosize
    then*) 
    let val (tmp, tmpdecl) = newTemp "tmp"
        val setLast = Assign(Field(reg tmp, n-1), accu)
        fun body i =
            let val set = Assign(FieldE(reg tmp, i), sp)
                val incsp = incr "sp"
            in  Block[set, incsp]
            end
    in Block[ tmpdecl 
            , Alloc_small tmp n t
            , setLast
            , forDown (litInt (n-2)) body 
            , assignAccu (reg tmp)
            ]
    end

val Num_tags    = Word.toInt(Word.<<(0w1, 0w8))
val No_scan_tag = Num_tags - 5
val Closure_tag = No_scan_tag - 2

fun closure lbl nvars =
    let val (tmp, tmpdecl) = newTemp "tmp"
    in  Block[ tmpdecl
             , if nvars > 0 then pushAccu else NOP
             , Alloc_small tmp (1 + nvars) Closure_tag
             , assignAccu (reg tmp)
             , Assign(Code_val_lv accu, reg(makeLetName lbl))
             , forUp (litInt nvars) 
                     (fn i => Assign(FieldE(accu, add(i, litInt 1)), 
                                     memread(add(sp, i))))
             , incrWith "sp" nvars
             ]
    end

fun closrec lbl nvars =
    let val (tmp, tmpdecl) = newTemp "tmp"
    in  Block[ tmpdecl
             , if nvars > 0 then pushAccu else NOP
             , Alloc_small tmp (2 + nvars) Closure_tag
             , assignAccu (reg tmp)
             , Assign(Code_val_lv accu, reg(makeLetName lbl))
             , forUp (litInt nvars) 
                     (fn i => Assign(FieldE(accu, add(i, litInt 2)), 
                                     memread(add(sp, i))))
             , incrWith "sp" nvars
             , modify (addreg "accu" 1) accu
             ]
    end

fun restart grab_lbl =
    let val (nargs, nargsdecl) = newTemp "nargs"
        fun stackEnv i = Assign(memaccess(add(sp, wordIndex i)), 
                                Fetch(FieldE(reg"env", add(i, litInt 2))))
    in  Block[ nargsdecl
             , Assign(Var nargs, minus(Wosize_val(reg"env"), litInt 2))
             , Assign(Var"sp", minus(sp, reg nargs))
             , forUp (reg nargs) stackEnv
             , Assign(Var"env", Fetch(Field(reg"env", 1)))
             , Assign(Var"extra_args", add(reg"extra_args", reg nargs))
             , jump (reg(makeLetName grab_lbl)) 
                    [sp, accu, reg"env", reg"extra_args"]
             ]
    end

fun grab restart_lbl required =
    let val (nargs, nargsdecl) = newTemp "nargs"
        val (tmp, tmpdecl)     = newTemp "tmp"
        val setnargs    = Assign(Var nargs, add(litInt 1, reg"extra_args"))
        val clos        = 
            Alloc_smallE tmp (add(litInt 2, reg nargs)) Closure_tag
        val setaccu     = assignAccu (reg tmp)
        fun saveStack i = Assign(FieldE(accu, add(i, litInt 2)),
                                 memread (add(sp, wordIndex i)))
    in  If(ge(reg"extra_args", litInt required),
           [Assign(Var"extra_args", minus(reg"extra_args", litInt required))],
           SOME[ nargsdecl
               , tmpdecl
               , setnargs
               , clos
               , setaccu
               , Assign(Field(accu, 1), reg"env")
               , forUp (reg nargs) saveStack
               , Assign(Code_val_lv(accu), reg(makeLetName restart_lbl))
               , Assign(Var"sp", add(sp, wordIndex (reg nargs)))
               , Assign(Var"env", readSp 0)
               , Assign(Var"extra_args", Long_val(readSp 1))
               , incrWith "sp" 3
               , Return(Cmm, [sp, accu, reg"env", reg"extra_args"]) 
               ])
    end

(* FIXME: Here we should check for overflow *)
val smladdint =
    let val sp  = Long_val(readSp 0)
        val tmp = add(sp, Long_val accu)
    in  Block[incr "sp", assignAccu (Val_long tmp)]
    end

(* FIXME: Here we should check for overflow *)
val smlsuccint =
    let val tmp    = add(Long_val accu, litInt 1)
    in  assignAccu (Val_long tmp)
    end

(* FIXME: Here we should check for overflow *)
val smlpredint =
    let val tmp    = minus(Long_val accu, litInt 1)
    in  assignAccu (Val_long tmp)
    end

fun Trap_pc tp = tp
fun Trap_link tp = addreg tp 1

fun nameCont lbl = "_$_cont_"^Int.toString lbl

fun pushtrap lbl =
    Block [ decrWith "sp" 4
          , Assign(lvmem (Trap_pc sp), reg(nameCont lbl))
          , Assign(lvmem (Trap_link "sp"), memread(reg"trapsp"))
          , assignSp 2 (reg"env")
          , assignSp 3 (Val_long(reg"extra_arg"))
          , Assign(lvmem(reg"trapsp"), sp)
          ]

val poptrap =
    Block[ Assign(lvmem(reg"trapsp"), memread(Trap_link "sp"))
         , incrWith "sp" 4
         ]

val makeCut =
     Block[ Assign(Var"sp", memread(reg"trapsp"))
          , Assign(lvmem(reg"trapsp"), memread(Trap_link "sp"))
          , Assign(Var"env", addreg "sp" 2)
          , Assign(Var"extra_args", Long_val(addreg "sp" 3))
          , Cut(Trap_pc sp, [sp, accu, reg"env", reg"extra_args"], [])
          ]


fun inttest tst lbl =
    If(tst(readSp 0, accu), [incr "sp", Goto(makeLabel lbl)], 
       SOME[incr "sp"])


fun switch lblarr =
    let val len     = Array.length lblarr
        val range   = SOME(0, len-1)
        fun collect (i, lbl, [])                    = [(lbl, [i])]
          | collect (i, lbl1, ((lbl2, ts) :: rest)) = 
            if lbl1 = lbl2 then (lbl2, i :: ts) :: rest
            else (lbl2, ts) :: collect (i, lbl1, rest)

        val arms = Array.foldli collect [] lblarr
        fun mkSwt (lbl, ts) = Swt(ts, [Goto(makeLabel lbl)])
        val swts = List.map mkSwt arms
    in
        Switch(accu ,range, swts)
    end


fun emit_zam zam =
    case zam of
        Kquote(ATOMsc(INTscon i))        => assignAccuInt i 
      | Kquote(ATOMsc(WORDscon w))       => assignAccuWord w 
      | Kquote(ATOMsc(CHARscon c))       => assignAccuChar c 
      | Kquote(BLOCKsc(CONtag(t,_), [])) => assignAccu (Atom t) 

      (* FIXME!*)
      | Kget_global (qid, i) => 
        assignAccu (memread(reg(Const.showQualId qid))) 
      (*| Kquote sc =>       (out GETGLOBAL; slot_for_literal sc)
      | Kget_global uid => (out GETGLOBAL; slot_for_get_global uid)
      | Kset_global uid => (out SETGLOBAL; slot_for_set_global uid)
       *)
      | Kgetfield n => (checkFields n; getField n)
      | Ksetfield n => (checkFields n; setField n)
      | Kaccess n   => (checkLocals n; assignAccu(readSp n))
      | Kenvacc m   =>
        let val n = m + 1
        in
          checkLocals n;
          assignAccu(memread(addreg"env" n)) 
        end
      | Kassign n       => (checkLocals n; assignSp n accu)
      | Kapply n        => apply n
      | Kappterm (n,z)  => (checkLocals z; appterm n z)
      | Kpush_retaddr _ => pushRetAddr
      | Kpop n          => (checkLocals n; incrWith "sp" n)
      | Kpush           => pushAccu
      | Kreturn n       => (checkLocals n; return n)
      | Klabel lbl      =>
        if lbl = Instruct.Nolabel then fatal "emit_zam: undefined label"
        else Label(makeLabel lbl) 
      | Kname lbl       => Label(makeLabel lbl)

      | Kmakeblock(tag,n) => makeblock tag n

      | Kcontinuation lbl => Continuation(nameCont lbl,
                                          ["sp", "accu", "env", "extra_args"])
      | Kpushtrap lbl     => pushtrap lbl
      | Kpoptrap          => poptrap
      | Kraise            => makeCut   

      | Kclosure    (lbl, sz) => closure lbl sz
      | Kclosurerec (lbl, sz) => closrec lbl sz
      | Knewrestart lbl       => restart lbl
      | Knewgrab    (lbl, sz) => grab lbl sz

      | Kprim Prim.Psmladdint  => smladdint
      | Kprim Prim.Psmlsuccint => smlsuccint
      | Kprim Prim.Psmlpredint => smlpredint
      | Kprim Prim.Ptag_of     => assignAccu (Val_long(Tag_val accu))

      | Kbranch lbl    => 
        Goto(makeLabel lbl)
      | Kbranchif lbl  => 
        If(ne(Tag_val accu, litInt 0), [Goto(makeLabel lbl)], NONE)
      | Kbranchifnot lbl => 
        If(eq(Tag_val accu, litInt 0), [Goto(makeLabel lbl)], NONE)
      | Kstrictbranchif lbl    => 
        If(ne(Tag_val accu, litInt 0), [Goto(makeLabel lbl)], NONE)
      | Kstrictbranchifnot lbl => 
        If(eq(Tag_val accu, litInt 0), [Goto(makeLabel lbl)], NONE)
      | Kbranchinterval(low, high, lbl_low, lbl_high) =>
        let val setaccu = assignAccu (minus(accu, litInt(low + 1)))
        in
            Block[ readTop "accu"
                 , if low = high andalso lbl_low = lbl_high 
                   then If(ne(accu, litInt low),
                           [Goto(makeLabel lbl_low)], NONE) 
                   else Block[If(lt(accu, litInt low),
                                 [Goto(makeLabel lbl_low)], NONE),
                              If(gt(accu, litInt high),
                                 [Goto(makeLabel lbl_high)], NONE)] 
                 , setaccu] 
        end
      | Kswitch lblarr => switch lblarr
      | Ktest(tst,lbl) =>
        let open Prim 
        in  case tst of 
                Peq_test          => inttest eq lbl
              | Pnoteq_test       => inttest ne lbl
              | Pint_test PTeq    => inttest eq lbl
              | Pint_test PTnoteq => inttest ne lbl
              | Pint_test PTlt    => inttest lt lbl
              | Pint_test PTle    => inttest le lbl
              | Pint_test PTgt    => inttest gt lbl
              | Pint_test PTge    => inttest ge lbl
              | _                 => fatal"Ktest"
        end

      | i => ( Misc.print "Unknown instruction: "
             ; Pr_zam.printZamInstr i
             ; Misc.print "\n"
             ; raise fatal"emit_zam")
     (* 
    | Ktest(tst,lbl) =>
        (case tst of
             Peq_test =>
               (out BRANCHIFEQ; out_label lbl)
           | Pnoteq_test =>
               (out BRANCHIFNEQ; out_label lbl)
           | Pint_test(PTnoteqimm i) =>
               (out PUSH; out_push_int_const i;
                out EQ; out POPBRANCHIFNOT; out_label lbl)
           | Pint_test x =>
               (out_bool_test BRANCHIFEQ x; out_label lbl)
           | Pfloat_test(PTnoteqimm f) =>
               (out PUSH; out PUSH_GETGLOBAL;
                slot_for_literal (ATOMsc(REALscon f));
                out EQFLOAT; out POPBRANCHIFNOT; out_label lbl)
           | Pfloat_test x =>
               (out_bool_test EQFLOAT x; out BRANCHIF; out_label lbl)
           | Pstring_test(PTnoteqimm s) =>
               (out PUSH; out PUSH_GETGLOBAL;
                slot_for_literal (ATOMsc(STRINGscon s));
                out EQSTRING; out POPBRANCHIFNOT; out_label lbl)
           | Pstring_test x =>
               (out_bool_test EQSTRING x; out BRANCHIF; out_label lbl)
           | Pword_test(PTnoteqimm w) =>
               (out PUSH; out_push_word_const w;
                out EQUNSIGN; out POPBRANCHIFNOT; out_label lbl)
           | Pword_test x =>
               (out_bool_test EQUNSIGN x; out BRANCHIF; out_label lbl)
           | Pnoteqtag_test tag =>
               (out BRANCHIFNEQTAG; out_tag tag; out_label lbl)
         )
  
    | Kprim p =>
        (case p of
            Pdummy n =>
              (checkLocals n; out DUMMY; out_short n)
          | Ptest tst =>
              (case tst of
                  Peq_test => out EQ
                | Pnoteq_test => out NEQ
                | Pint_test tst => out_bool_test EQ tst
                | Pfloat_test tst => out_bool_test EQFLOAT tst
                | Pstring_test tst => out_bool_test EQSTRING tst
                | Pword_test tst => out_bool_test EQUNSIGN tst
                | _ => fatalError "emit_zam : Kprim, Ptest")
          | Patom t =>
              if t < 10 then out (ATOM0 + t) else (out ATOM; out t)
          | Pccall(name, arity) =>
              (if arity <= 5 then
                 out (C_CALL1 + arity - 1)
               else
                 (out C_CALLN; out arity);
               slot_for_c_prim name)
          | Pfloatprim p =>
              out(opcode_for_float_primitive p)
          | Pidentity =>
              ()
          | p =>
              out(opcode_for_primitive p)
         )
  
    | Kcheck_signals => out CHECK_SIGNALS

fun emit zams =
  case zams of
      [] => ()
    | Kpush :: Kquote(ATOMsc(INTscon i)) :: C =>
        (out_push_int_const i; emit C)
    | Kpush :: Kquote(ATOMsc(WORDscon w)) :: C =>
        (out_push_word_const w; emit C)
    | Kpush :: Kquote(ATOMsc(CHARscon c)) :: C =>
        (out_push_int_const (Char.ord c); emit C)
    | Kpush :: Kquote(BLOCKsc(CONtag(t,_), [])) :: C =>
	((if t = 0 then out PUSHATOM0 else (out PUSHATOM; out t));
	 emit C)
    | Kpush :: Kquote sc :: C => (out PUSH_GETGLOBAL; slot_for_literal sc; emit C)
    | Kpush :: Kaccess n :: C =>
        (checkLocals n;
         if n < 8 then out(PUSHACC0 + n) else (out PUSHACC; out_short n);
         emit C)
    | Kpush :: Kenvacc 0 :: Kapply n :: C =>
        (checkArguments n;
         if n < 5 then 
	     out(PUSH_ENV1_APPLY1 + n - 1)
	 else 
	     (out PUSHENV1;
	      out APPLY; out n);
         emit C)
    | Kpush :: Kenvacc 0 :: Kappterm (n,z) :: C =>
        ((if n < 5 then 
	      out(PUSH_ENV1_APPTERM1 + n - 1)
	  else 
	      (checkArguments n; out PUSHENV1; out APPTERM; out n));
         checkLocals z; out_short z;
         emit C)
    | Kpush :: Kenvacc m :: C =>
        let val n = m + 1
        in
          checkLocals n;
          if n < 8 then out(PUSHENV1 + m) else (out PUSHENVACC; out_short n);
          emit C
        end
    | Kpush :: Kget_global uid :: Kapply n :: C =>
        (if n < 5 then 
	     (out(PUSH_GETGLOBAL_APPLY1 + n - 1);
	      slot_for_get_global uid)
	 else 
	     (checkArguments n;
	      out PUSH_GETGLOBAL;
	      slot_for_get_global uid;
	      out APPLY; out n);
         emit C)
    | Kpush :: Kget_global uid :: Kappterm (n,z) :: C =>
        (if n < 5 then 
	     (out(PUSH_GETGLOBAL_APPTERM1 + n - 1);
	      checkLocals z; out_short z;
	      slot_for_get_global uid)
	 else 
	     (checkArguments n; 
	      out PUSH_GETGLOBAL;
	      slot_for_get_global uid;
	      out APPTERM; out n;
	      checkLocals z;
	      out_short z);
         emit C)
    | Kpush :: Kget_global uid :: C =>
        (out PUSH_GETGLOBAL;
         slot_for_get_global uid;
         emit C)
    | Kgetfield 0 :: Kgetfield 0 :: C => 
	(out GETFIELD0_0; emit C)
    | Kgetfield 0 :: Kgetfield 1 :: C => 
	(out GETFIELD0_1; emit C)
    | Kgetfield 1 :: Kgetfield 0 :: C => 
	(out GETFIELD1_0; emit C)
    | Kgetfield 1 :: Kgetfield 1 :: C => 
	(out GETFIELD1_1; emit C)
    | zam :: C =>
       (emit_zam zam; emit C)
;

*)
end
end
