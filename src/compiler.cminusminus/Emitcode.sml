open
  Obj Fnlib Config Mixture Const Instruct Prim
  Opcodes Prim_opc Buffcode Labels Reloc
;

(* 1996.07.13 -- e *)

prim_val lshift_    : int -> int -> int = 2 "shift_left";
prim_val rshiftsig_ : int -> int -> int = 2 "shift_right_signed";
prim_val rshiftuns_ : int -> int -> int = 2 "shift_right_unsigned";


(* Generation of bytecode for .uo files *)

fun tooManyError kind = 
    (msgIBlock 0;
     errPrompt ("Too many " ^ kind ^ "; unable to generate bytecode");
     msgEOL();
     msgEBlock();
     raise Toplevel);
    
fun checkArguments n =
    if n > maxint_byte then tooManyError "arguments" else ()

(* This won't happen unless there's a bug in the switch compilation: *)
fun checkBranches n =
    if n > maxint_byte then tooManyError "switch branches" else ()

fun checkGlobals n =
    if n > 0xFFFF then tooManyError "globals" else ()

fun checkLocals n =
    if n > 0xFFFF then tooManyError "local variables" else ()

fun checkFields n =
    if n > 0xFFFF then tooManyError "fields" else ()

fun out_bool_test tst =
  fn PTeq    => out tst
   | PTnoteq => out (tst + 1)
   | PTlt    => out (tst + 2)
   | PTgt    => out (tst + 3)
   | PTle    => out (tst + 4)
   | PTge    => out (tst + 5)
   | _       => fatalError "out_bool_test"
;

fun out_int_const i =
  if i >= minint_short andalso i <= maxint_short then
    if i >= 0 andalso i <= 3
    then
      out (CONST0 + i)
    else
      let val ii1 = i+i+1 in
        if ii1 >= minint_byte andalso ii1 <= maxint_byte then
          (out CONSTBYTE; out (ii1))
        else if ii1 >= minint_short andalso ii1 <= maxint_short then
          (out CONSTSHORT; out_short (ii1))
        else
          (out CONSTINT; out_long i)
      end
  else if i >= minint_int31 andalso i <= maxint_int31 then
    (out CONSTINT; out_long i)
  else (* This happens only in a 64 bit runtime system: *)
    (out GETGLOBAL; slot_for_literal (ATOMsc(INTscon i)));
fun out_word_const w =
    let prim_val w2i : word -> int = 1 "identity"
    in out_int_const (w2i w) end;

fun out_push_int_const i =
  if i >= minint_short andalso i <= maxint_short then
    if i >= 0 andalso i <= 3
    then
      out (PUSHCONST0 + i)
    else
      let val ii1 = i+i+1 in
        if ii1 >= minint_byte andalso ii1 <= maxint_byte then
          (out PUSH; out CONSTBYTE; out (ii1))
        else if ii1 >= minint_short andalso ii1 <= maxint_short then
          (out PUSH; out CONSTSHORT; out_short (ii1))
        else
          (out PUSHCONSTINT; out_long i)
      end
  else if i >= minint_int31 andalso i <= maxint_int31 then
    (out PUSHCONSTINT; out_long i)
  else (* This happens only in a 64 bit runtime system: *)
    (out PUSH_GETGLOBAL; slot_for_literal (ATOMsc(INTscon i)));

fun out_push_word_const w =
    let prim_val w2i : word -> int = 1 "identity"
    in out_push_int_const (w2i w) end;

fun out_tag (CONtag(t,_)) = out t;

fun out_header (n, tag) =
(
  out_tag tag;
  out (lshift_ n 2);
  out (rshiftuns_ n 6);
  out (rshiftuns_ n 14)
);

fun emit_zam zam =
  case zam of
      Kquote(ATOMsc(INTscon i)) => out_int_const i
    | Kquote(ATOMsc(WORDscon w)) => out_word_const w
    | Kquote(ATOMsc(CHARscon c)) => out_int_const (Char.ord c)
    | Kquote(BLOCKsc(CONtag(t,_), [])) =>
	  if t < 10 then out (ATOM0 + t) else (out ATOM; out t)
    | Kquote sc =>       (out GETGLOBAL; slot_for_literal sc)
    | Kget_global uid => (out GETGLOBAL; slot_for_get_global uid)
    | Kset_global uid => (out SETGLOBAL; slot_for_set_global uid)
    | Kgetfield n =>
        (checkFields n;
	 if n < 4 then out (GETFIELD0 + n)
         else (out GETFIELD; out_short n))
    | Ksetfield n =>
        (checkFields n;
	 if n < 4 then out (SETFIELD0 + n)
         else (out SETFIELD; out_short n))
    | Kaccess n =>
        (checkLocals n;
         if n < 8 then out(ACC0 + n) else (out ACCESS; out_short n))
    | Kenvacc m =>
        let val n = m + 1
        in
          checkLocals n;
          if n < 8 then out(ENV1 + m) else (out ENVACC; out_short n)
        end
    | Kassign n =>
        (checkLocals n; out ASSIGN; out_short n)
    | Kapply n =>
        (checkArguments n;
         if n < 5 then out(APPLY1 + n - 1) else (out APPLY; out n))
    | Kappterm (n,z) =>
        (checkArguments n;
         if n < 5 then out(APPTERM1 + n - 1) else (out APPTERM; out n);
         checkLocals z;
         out_short z)
    | Kpop n    => (checkLocals n; out POP; out_short n)
    | Kgrab n   => (checkArguments n; out GRAB; out n)
    | Kreturn n => 
	(checkLocals n; 
	 if n < 3 then out(RETURN1 + n - 1) else (out RETURN; out_short n))
    | Kmakeblock(tag,n) =>
        (if n <= 0 then
           fatalError "emit_zam : Kmakeblock"
         else if n < 5 then
           (out (MAKEBLOCK1 + n - 1);
            out_tag tag)
         else
          (out MAKEBLOCK;
           out_header(n, tag)))
    | Klabel lbl =>
        if lbl = Nolabel then fatalError "emit_zam: undefined label"
        else (define_label lbl)
    | Kclosure (lbl,sz) => (out CLOSURE; out sz; out_label lbl)
    | Kclosurerec (lbl,sz) => (out CLOSREC; out (sz - 1); out_label lbl)
    | Kpushtrap lbl => (out PUSHTRAP; out_label lbl)
    | Kpush_retaddr lbl => (out PUSH_RETADDR; out_label lbl)
    | Kbranch lbl => (out BRANCH; out_label lbl)
    | Kbranchif lbl => (out BRANCHIF; out_label lbl)
    | Kbranchifnot lbl => (out BRANCHIFNOT; out_label lbl)
    | Kstrictbranchif lbl => (out BRANCHIF; out_label lbl)
    | Kstrictbranchifnot lbl => (out BRANCHIFNOT; out_label lbl)
    | Kswitch lblvect =>
        let val len = Array.length lblvect
            val ()  = out SWITCH;
            val ()  = out len;
            val orig = !out_position
        in
	    checkBranches len;
	    for (fn i => out_label_with_orig orig (Array.sub(lblvect, i)))
	        0 (len-1)
        end
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
    | Kbranchinterval(low, high, lbl_low, lbl_high) =>
        (out_push_int_const low;
         if low <> high then out_push_int_const high else out PUSH;
         out BRANCHINTERVAL;
         out_label lbl_low;
         out_label lbl_high
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
    | Kpush => out PUSH
    | Kraise => out RAISE
    | Krestart => out RESTART
    | Kpoptrap => out POPTRAP
    | Kcheck_signals => out CHECK_SIGNALS
;

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
