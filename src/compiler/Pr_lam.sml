local
  open Mixture Const Prim Lambda Asynt;
in

(* Printing lambda expressions for debugging purposes. *)

fun printPrimTest printer = fn
    PTeq => msgString "eq"
  | PTnoteq => msgString "noteq"
  | PTnoteqimm a => (msgString "noteqimm "; printer a)
  | PTlt => msgString "lt"
  | PTle => msgString "le"
  | PTgt => msgString "gt"
  | PTge => msgString "ge"
;

val rec printPrim = fn
    Pidentity => msgString "identity"
  | Pget_global (qualid, stamp) =>
      (msgString "get_global "; printQualId qualid;
       msgString "/"; msgInt stamp)
  | Pset_global (qualid, stamp) =>
      (msgString "set_global "; printQualId qualid;
       msgString "/"; msgInt stamp)
  | Pdummy n => (msgString "dummy "; msgInt n)
  | Pupdate => msgString "update"
  | Ptest btest => (msgString "test:"; printBoolTest btest)
  | Pmakeblock ctag => (msgString "makeblock "; printCTag ctag)
  | Ptag_of => msgString "tag_of"
  | Pfield n => (msgString "field "; msgInt n)
  | Psetfield n => (msgString "setfield "; msgInt n)
  | Pccall(name, arity) =>
      (msgString "ccall"; msgInt arity;
       msgString " "; msgString name)
  | Praise => msgString "raise"
  | Pnot => msgString "not"
  | Paddint => msgString "unsaddint"
  | Psubint => msgString "unssubint"
  | Pmulint => msgString "unsmulint"
  | Pdivint => msgString "unsdivint"
  | Pmodint => msgString "unsmodint"
  | Pandint => msgString "andint"
  | Porint => msgString "orint"
  | Pxorint => msgString "xorint"
  | Pshiftleftint => msgString "shiftleftint"
  | Pshiftrightintsigned => msgString "shiftrightintsigned"
  | Pshiftrightintunsigned => msgString "shiftrightintunsigned"
  | Pintoffloat => msgString "intoffloat"
  | Pfloatprim fprim => (msgString "floatprim "; printFloatPrim fprim)
  | Pstringlength => msgString "stringlength"
  | Pgetstringchar => msgString "getstringchar"
  | Psetstringchar => msgString "setstringchar"
  | Pmakevector => msgString "makevector"
  | Pvectlength => msgString "vectlength"
  | Pgetvectitem => msgString "getvectitem"
  | Psetvectitem => msgString "setvectitem"
  | Psmlnegint => msgString "smlnegint"
  | Psmlsuccint => msgString "smlsuccint"
  | Psmlpredint => msgString "smlpredint"
  | Psmladdint => msgString "smladdint"
  | Psmlsubint => msgString "smlsubint"
  | Psmlmulint => msgString "smlmulint"
  | Psmldivint => msgString "smldivint"
  | Psmlmodint => msgString "smlmodint"
  | Pmakerefvector => msgString "makerefvector"
  | Patom t     => (msgString "atom "; msgInt t)
  | Psmlquotint => (msgString "smlquotint")
  | Psmlremint  => (msgString "smlremint")
  | Pclosure (lbl,sz) => 
      (msgString "closure "; msgInt lbl; msgString " "; msgInt sz)
  | Pswap => msgString "swap"

and printFloatPrim = fn
    Pfloatofint => msgString "floatofint"
  | Psmlnegfloat => msgString "smlnegfloat"
  | Psmladdfloat => msgString "smladdfloat"
  | Psmlsubfloat => msgString "smlsubfloat"
  | Psmlmulfloat => msgString "smlmulfloat"
  | Psmldivfloat => msgString "smldivfloat"

and printBoolTest = fn
    Peq_test => msgString "eq_test"
  | Pnoteq_test => msgString "noteq_test"
  | Pint_test test => printPrimTest msgInt test
  | Pfloat_test test => printPrimTest msgReal test
  | Pstring_test test => printPrimTest msgString test
  | Pword_test test => printPrimTest msgWord test
  | Pnoteqtag_test ct =>
      (msgString "noteqtag_test "; printCTag ct)
;

fun printLam lam =
  case lam of
    Lvar i => (msgString "var:"; msgInt i)
  | Lconst cst => printStrConst cst
  | Lapply(func, args) =>
      (msgString "(app "; printLam func; msgString " ";
       printSeq printLam " " args; msgString ")")
  | Lfn lam => (msgString "(fn "; printLam lam; msgString ")")
  | Llet(args, scope) =>
      (msgString "let "; printSeq printLam " " args;
       msgString " in "; printLam scope; msgString " end")
  | Lletrec(args, scope) =>
      (msgString "letrec "; printSeq printLam " " args;
       msgString " in "; printLam scope; msgString " end")
  | Lprim(prim, args) =>
      (msgString "(prim ("; printPrim prim; msgString ") ";
       printSeq printLam " " args; msgString ")")
  | Lcase(arg, clauses) =>
      (msgString "(case "; printLam arg; msgString " of ";
       printSeq printClause " " clauses; msgString ")")
  | Lswitch(n, arg, clauses) =>
      (msgString "(switch:"; msgInt n; msgString " ";
       printLam arg; msgString " of ";
       printSeq printSwClause " " clauses; msgString ")")
  | Lstaticfail => msgString "staticfail"
  | Lstatichandle(lam1, lam2) =>
      (msgString "("; printLam lam1; msgString " statichandle ";
       printLam lam2; msgString ")")
  | Lhandle(lam1, lam2) =>
      (msgString "("; printLam lam1; msgString " handle ";
       printLam lam2; msgString ")")
  | Lif(lam0, lam1, lam2) =>
      (msgString "if"; printLam lam0; msgString " then (";
       printLam lam1; msgString ") else "; printLam lam2)
  | Lseq(lam1, lam2) =>
      (msgString "("; printLam lam1; msgString "; "; printLam lam2;
       msgString ")")
  | Lwhile(lam1, lam2) =>
      (msgString "while "; printLam lam1; msgString " do ";
       printLam lam2)
  | Landalso(lam1, lam2) =>
      (msgString "("; printLam lam1; msgString " andalso ";
       printLam lam2; msgString ")")
  | Lorelse(lam1, lam2) =>
      (msgString "("; printLam lam1; msgString " orelse ";
       printLam lam2; msgString ")")
  | Lunspec =>
      msgString "unspec"
  | Lshared(lam_ref, lbl) =>
      (msgString "(shared:"; msgInt (!lbl); msgString " ";
       printLam (!lam_ref); msgString ")")
  | Lassign(i,lam) => (msgString "assign:"; msgInt i;
                       msgString " <- "; printLam lam)

and printClause (scon, lam) =
  (printSCon scon; msgString " : "; printLam lam)

and printExClause (lam1, lam2) =
  (printLam lam1; msgString " : "; printLam lam2)

and printSwClause (ct, lam) =
  (printCTag ct; msgString " : "; printLam lam)
;

end;
