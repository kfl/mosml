local
  open Obj Fnlib Config Mixture Const Instruct Asynt Pr_lam;
in

(* 1996.07.05 -- e *)

val printZamInstr = fn
    Kquote sc =>
      printStrConst sc
  | Kget_global (qualid, stamp) =>
      (msgString "get_global "; printQualId qualid;
       msgString "/"; msgInt stamp)
  | Kset_global (qualid, stamp) =>
      (msgString "set_global "; printQualId qualid;
       msgString "/"; msgInt stamp)
  | Kaccess i =>
      (msgString "access "; msgInt i)
  | Kenvacc i =>
      (msgString "envacc "; msgInt i)
  | Kassign i =>
      (msgString "assign "; msgInt i)
  | Kgetfield i =>
      (msgString "getfield "; msgInt i)
  | Ksetfield i =>
      (msgString "setfield "; msgInt i)
  | Kpush =>
      msgString "push"
  | Kpop i =>
      (msgString "pop "; msgInt i)
  | Krestart =>
      msgString "restart"
  | Kgrab i =>
      (msgString "grab "; msgInt i)
  | Kapply n =>
      (msgString "apply "; msgInt n)
  | Kappterm (n,z) =>
      (msgString "appterm "; msgInt n; msgString " "; msgInt z)
  | Kpush_retaddr i =>
      (msgString "push_retaddr "; msgInt i)
  | Kcheck_signals =>
      msgString "check_signals"
  | Kreturn i =>
      (msgString "return "; msgInt i)
  | Kclosure (i,n) =>
      (msgString "closure "; msgInt i; msgString " "; msgInt n)
  | Kclosurerec (i,n) =>
      (msgString "closurerec "; msgInt i; msgString " "; msgInt n)
  | Kraise =>
      msgString "raise"
  | Kmakeblock(ct, i) =>
      (msgString "makeblock "; printCTag ct; msgString " "; msgInt i)
  | Kprim prim =>
      printPrim prim
  | Kpushtrap i =>
      (msgString "pushtrap "; msgInt i)
  | Kpoptrap =>
      msgString "poptrap"
  | Klabel i =>
      (msgString "label "; msgInt i)
  | Kbranch i =>
      (msgString "branch "; msgInt i)
  | Kbranchif i =>
      (msgString "branchif "; msgInt i)
  | Kbranchifnot i =>
      (msgString "branchifnot "; msgInt i)
  | Kstrictbranchif i =>
      (msgString "strictbranchif "; msgInt i)
  | Kstrictbranchifnot i =>
      (msgString "strictbranchifnot "; msgInt i)
  | Ktest(tst, i) =>
      (msgString "test:"; printBoolTest tst;
       msgString " "; msgInt i)
  | Kbranchinterval(i1, i2, i3, i4) =>
      (msgString "branchinterval "; msgInt i1;
       msgString " "; msgInt i2;
       msgString " "; msgInt i3;
       msgString " "; msgInt i4)
  | Kswitch v =>
      let val () = msgString "switch "
          val len = Array.length v
      in
        for (fn i =>
               (msgInt (Array.sub(v, i-1));
                if i < len then msgString " " else ()))
            1 len
      end
;

fun printZamSeq zams = printSeq printZamInstr "; " zams;

fun printZamPhrase
        { kph_is_pure=is_pure, kph_inits=inits, kph_funcs=funcs } =
(
  msgIBlock 0;
  msgString "***kph_is_pure*** = ";
  msgString (if is_pure then "true;" else "false;");
  msgEOL();
  msgString "***kph_inits*** = ";
  printSeq printZamInstr "; " inits;
  msgEOL(); msgString "***kph_funcs*** = ";
  printSeq printZamInstr "; " funcs;
  msgEOL();
  msgEBlock()
);

end;
