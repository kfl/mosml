open List Fnlib Mixture Const Globals Location Types Asynt;

fun mkIdInfo (loc, qualid) withOp =
  { qualid = qualid,
    info = { idLoc=loc, withOp=withOp,
             idKind= ref { qualid=qualid, info=VARik },
             idFields = ref ([]:int list)}}
;

fun getConInfo (ii : IdInfo) =
  case #info(! (#idKind (#info ii))) of
      CONik ci => ci
    | _ => fatalError "getConInfo"
;

fun getExConInfo (ii : IdInfo) =
  case #info(!(#idKind (#info ii))) of
      EXCONik ei => ei
    | _ => fatalError "getExConInfo"
;

fun pairExp e1 e2 =
  (xxLR e1 e2, RECexp(ref (RECre(mkPairRow e1 e2))))
;

fun tupleExp (loc, exps) =
  (loc, RECexp(ref (RECre(mkTupleRow exps))))
;

val qQUOTE  = { qual = "General", id = ["QUOTE"] };
val qANTIQUOTE = { qual = "General", id = ["ANTIQUOTE"] };

fun quoteExp exp =
  let val loc = xLR exp in
    (loc, APPexp((loc,
       VIDPATHexp(ref (RESvidpath(mkIdInfo (loc, qQUOTE) false)))), exp))
  end
;

fun antiquoteExp exp =
  let val loc = xLR exp in
    (loc, APPexp((loc,
       VIDPATHexp(ref (RESvidpath(mkIdInfo (loc, qANTIQUOTE) false)))), exp))
  end
;

val qNil  = { qual = "", id = ["nil"] };
val qCons = { qual = "", id = ["::"] };

fun listExp (Loc(l,r), exps) =
  let val locR = Loc(r-1,r) in
    foldR (fn e1 => fn e2 =>
             let val locO = xxLR e1 e2
                 val locI = xxRL e1 e2
             in
               (locO, APPexp((locI,
                        VIDPATHexp(ref (RESvidpath(mkIdInfo (locI, qCons) false)))),
                          pairExp e1 e2))
             end)
          (locR, VIDPATHexp(ref (RESvidpath(mkIdInfo (locR,qNil) false)))) exps
  end;

fun seqExp exps =
  foldR1 (fn e1 => fn e2 =>
            let val loc12 = xxLR e1 e2 in (loc12, SEQexp(e1,e2)) end)
         exps
;

val qX = { qual = "", id = ["~x"] };

fun hashLabelExp (loc, lab) =
  let val pat =
        (loc, RECpat(ref
                (RECrp([(lab, (loc, VARpat(mkIdInfo (loc,qX) false)))],
                       SOME (fresh3DotType())))))
      and exp =
        (loc, VIDPATHexp(ref (RESvidpath(mkIdInfo (loc, qX) false))))
  in (loc, FNexp [MRule(ref([pat]),exp)]) end
;

fun mkLabPatOfId (locId as (loc, id)) ty_opt pat_opt =
  let val lab = STRINGlab id
      val var = (loc, VARpat(mkIdInfo (loc, { qual="", id=[id] }) false))
  in
    case (ty_opt, pat_opt) of
        (SOME ty, SOME pat) =>
          (lab, (xxLR locId pat, LAYEREDpat(var,
                   (xxLR ty pat, TYPEDpat(pat, ty)))))
      | (NONE, SOME pat) =>
          (lab, (xxLR locId pat, LAYEREDpat(var, pat)))
      | (SOME ty, NONE) =>
          (lab, (xxLR locId ty, TYPEDpat(var, ty)))
      | (NONE, NONE) =>
          (lab, var)
  end;

fun pairPat p1 p2 =
  let val loc = xxLR p1 p2 in
    (loc, RECpat(ref (RECrp(mkPairRow p1 p2, NONE))))
  end;

fun tuplePat (loc, pats) =
  (loc, RECpat(ref (RECrp(mkTupleRow pats, NONE))))
;

fun listPat (Loc(l,r), exps) =
  let val locR = Loc(r-1,r) in
    foldR (fn e1 => fn e2 =>
             let val locO = xxLR e1 e2
                 val locI = xxRL e1 e2
             in
               (locO, CONSpat(mkIdInfo (locI,qCons) true, pairPat e1 e2))
             end)
          (locR, (VARpat (mkIdInfo (locR, qNil) true))) exps
  end;

fun tupleTy [t] = t
  | tupleTy ts =
      let val loc = xxLR (hd ts) (last ts) in
        (loc, RECty (mkTupleRow ts))
      end
;

val qIt = { qual = "", id = ["it"] };

fun mkValIt exp =
  let val loc = xLR exp in
    (loc, VALdec
      ([], ([ValBind(ref(loc, (VARpat (mkIdInfo (loc, qIt) false))), exp)], [])))
  end;

fun domPatAcc (_, pat') ids =
  case pat' of
    SCONpat _ => ids
  | VARpat ii => (hd(#id(#qualid ii))) :: ids
  | WILDCARDpat => ids
  | NILpat _ => ids
  | CONSpat(_, p) => domPatAcc p ids
  | EXNILpat _ => ids
  | EXCONSpat(_, p) => domPatAcc p ids
  | EXNAMEpat _ => fatalError "domPatAcc"
  | REFpat p => domPatAcc p ids
  | RECpat(ref (RECrp(fs, _))) =>
      foldL_map domPatAcc snd ids fs
  | RECpat(ref (TUPLErp ps)) =>
      foldL domPatAcc ids ps
  | VECpat ps =>
      foldL domPatAcc ids ps
  | INFIXpat _ => fatalError "domPatAcc"
  | PARpat p => domPatAcc p ids
  | TYPEDpat(p,_) => domPatAcc p ids
  | LAYEREDpat(p1,p2) => domPatAcc p2 (domPatAcc p1 ids)
;

fun domPat pat = domPatAcc pat [];

fun varsOfPatAcc (_, pat') iis =
  case pat' of
    SCONpat _ => iis
  | VARpat ii => ii :: iis
  | WILDCARDpat => iis
  | NILpat _ => iis
  | CONSpat(_, p) => varsOfPatAcc p iis
  | EXNILpat _ => iis
  | EXCONSpat(_, p) => varsOfPatAcc p iis
  | EXNAMEpat _ => fatalError "varsOfPatAcc"
  | REFpat p => varsOfPatAcc p iis
  | RECpat(ref (RECrp(fs, _))) => foldL_map varsOfPatAcc snd iis fs
  | RECpat(ref (TUPLErp _)) => fatalError "varsOfPatAcc"
  | VECpat ps => foldL varsOfPatAcc iis ps
  | INFIXpat _ => fatalError "varsOfPatAcc"
  | PARpat p => varsOfPatAcc p iis
  | TYPEDpat(p,_) => varsOfPatAcc p iis
  | LAYEREDpat(p1,p2) => varsOfPatAcc p2 (varsOfPatAcc p1 iis)
;

fun curriedness (MRule(ref pats,_) :: _) = List.length pats
  | curriedness _ = fatalError "curriedness"
;


fun printIdInfo (ii : IdInfo) =
  let val {qualid, info} = ii in
    if #withOp info then msgString "op " else ();
    printQualId qualid
  end;

fun printLocString (loc,string) = msgString string;;
fun printVId vid  = printLocString vid ;;
fun printTyCon tycon  = printLocString tycon;
fun printModId modid  = printLocString modid;
fun printModId modid  = printLocString modid;
fun printFunId funid  = printLocString funid;
fun printSigId sigid  = printLocString sigid;


fun printTyVarSeq [] = ()
  | printTyVarSeq [ii] =
      (printIdInfo ii; msgString " ")
  | printTyVarSeq iis =
      (msgString "("; printSeq printIdInfo ", " iis;
       msgString ") ")
;


fun printTyConPath (_, tyconpath') = 
  case tyconpath' of
    LONGtyconpath ii =>
       printIdInfo ii 
  | WHEREtyconpath (ii,(loc,modid),modexp) =>
      (printIdInfo ii;
       msgString " where ";
       msgString modid;
       msgString " = ";
       printModExp modexp)

and printTy (_, ty') =
  case ty' of
    TYVARty ii =>
      msgString (hd (#id (#qualid ii)))
  | RECty fs =>
      (msgString "{"; printSeq printRecTyField ", " fs; msgString "}")
  | CONty(ts, tyconpath) =>
      (printTySeq ts; printTyConPath tyconpath)
  | FNty(t, t') =>
      (msgString "("; printTy t; msgString " -> "; printTy t';
       msgString ")")
  | PACKty sigexp => 
      (msgString "{";  printSigExp sigexp;
       msgString "}")    
  | PARty ty => 
      (msgString "(";  printTy ty;
       msgString ")")    

and printRecTyField (lab, ty) =
  (msgIBlock 0; printLab lab; msgString " :"; msgBreak(1, 2); printTy ty;
   msgEBlock())

and printTySeq [] = ()
  | printTySeq [t] =
      (printTy t; msgString " ")
  | printTySeq ts =
      (msgString "("; printSeq printTy ", " ts;
       msgString ")")

and printOvlType ovltype tau =
(
  msgString
    (case ovltype of
         REGULARo => "</ "
       | OVL1NNo => "<num -> num/ "
       | OVL1NSo => "<numtext -> string/ "
       | OVL2NNBo => "<num * num -> bool/ "
       | OVL2NNNo => "<num * num -> num/ "
       | OVL1TXXo => "<'a -> 'a/ "
       | OVL1TPUo => "<(ppstream -> 'a -> unit) -> unit/ "
       | OVL2EEBo => "<''a * ''a -> bool/ ");
  printType tau;
  msgString " > "
)


and printVIdPathInfo (ref(RESvidpath longvid)) = 
      printIdInfo longvid
  | printVIdPathInfo (ref(OVLvidpath (longvid,ovltype,tau))) =
      (printIdInfo longvid;
       printOvlType ovltype tau)       

and printInfixExp ie = 
  case ie of
    UNRESinfixexp es =>
      (msgString "(UNRES ";
       printSeq printExp " " es;
       msgString ")")
  | RESinfixexp exp =>
       printExp exp

and printExp (_, exp') =
  case exp' of
    SCONexp (scon, _) =>
      printSCon scon
  | VIDPATHexp vidpathinfo =>
      printVIdPathInfo vidpathinfo
  | RECexp(ref (RECre fs)) =>
      (msgString "{"; printSeq printExpField ", " fs;
       msgString "}")
  | RECexp(ref (TUPLEre es)) =>
      (msgString "("; printSeq printExp ", " es;
       msgString ")")
  | VECexp es =>
      (msgString "#["; printSeq printExp ", " es;
       msgString "]")
  | PARexp e => printExp e
  | FNexp mrules =>
      (msgString "(fn "; printSeq printMRule " | " mrules;
       msgString ")")
  | APPexp (e1,e2) =>
      (msgString "("; printSeq printExp " " [e1,e2];
       msgString ")")
  | LETexp (dec,exp) =>
      (msgString "let "; printDec dec; msgString " in ";
       printExp exp; msgString " end")
  | INFIXexp (ref infixexp) =>
      (msgString "(INFIXexp ";
       printInfixExp infixexp;
       msgString ")")
  | TYPEDexp(exp,ty) =>
      (msgString "("; printExp exp; msgString " : ";
       printTy ty; msgString ")")
  | ANDALSOexp(exp1,exp2) =>
      (printExp exp1; msgString " andalso "; printExp exp2)
  | ORELSEexp(exp1,exp2) =>
      (printExp exp1; msgString " orelse "; printExp exp2)
  | HANDLEexp(exp, mrules) =>
      (msgString "("; printExp exp; msgString " handle ";
       printSeq printMRule " | " mrules; msgString ")")
  | RAISEexp exp =>
      (msgString "raise "; printExp exp)
  | IFexp(exp0,exp1,exp2) =>
      (msgString "if "; printExp exp0; msgString " then ";
       printExp exp1; msgString " else "; printExp exp2)
  | WHILEexp(exp1,exp2) =>
      (msgString "while "; printExp exp1; msgString " do ";
       printExp exp2)
  | SEQexp(exp1,exp2) =>
      (msgString "("; printExp exp1; msgString "; ";
       printExp exp2; msgString ")")
  | STRUCTUREexp(modexp,sigexp,_) =>
      (msgString "[structure "; printModExp modexp; msgString " as ";
       printSigExp sigexp;
       msgString "]")
  | FUNCTORexp(modexp,sigexp,_) =>
      (msgString "[functor "; printModExp modexp; msgString " as ";
       printSigExp sigexp;
       msgString "]")
and printExpField (lab, e) =
  (msgIBlock 0; printLab lab; msgString " ="; msgBreak(1, 2);
   printExp e; msgEBlock())

and printMRule (MRule(ref ps, e)) =
      (printSeq printPat " => " ps; msgString " => "; printExp e)

and printInfixPat ip = 
  case ip of
    UNRESinfixpat ps =>
      (msgString "(UNRES ";
       printSeq printPat " " ps;
       msgString ")")
  | RESinfixpat pat =>
       printPat pat

and printPat (_, pat') =
  case pat' of
    SCONpat (scon , _) => printSCon scon
  | VARpat ii => printIdInfo ii
  | WILDCARDpat => msgString "_"
  | NILpat ii => printIdInfo ii
  | CONSpat(ii, p) =>
      (msgString "("; printIdInfo ii; printPat p; msgString ")")
  | EXNILpat ii => printIdInfo ii
  | EXCONSpat(ii,p) =>
      (msgString "("; printIdInfo ii; printPat p; msgString ")")
  | EXNAMEpat lam =>
      (msgString "<exnname access>")
  | REFpat p =>
      (msgString "("; msgString "ref "; printPat p; msgString ")")
  | RECpat(ref (RECrp(fs, dots))) =>
      (msgString "{"; printSeq printPatField ", " fs;
       case dots of
           NONE =>
             msgString "}"
         | SOME _ =>
             msgString ", ...}")
  | RECpat(ref (TUPLErp ps)) =>
      (msgString "("; printSeq printPat ", " ps; msgString ")")
  | VECpat ps =>
      (msgString "#["; printSeq printPat ", " ps; msgString "]")
  | PARpat p =>
       printPat p
  | INFIXpat (ref infixpat) =>
      (msgString "(INFIXpat";
       printInfixPat infixpat;
       msgString ")")
  | TYPEDpat(pat, ty) =>
      (msgString "("; printPat pat; msgString " : ";
       printTy ty; msgString ")")
  | LAYEREDpat(pat1, pat2) =>
      (msgString "("; printPat pat1; msgString " as ";
       printPat pat2; msgString ")")

and printPatField (lab, pat) =
  (msgIBlock 0; printLab lab; msgString " ="; msgBreak(1, 2);
   printPat pat; msgEBlock())

and printDec (_, dec') =
  case dec' of
    VALdec (tvs, (pvbs, rvbs)) =>
      (msgString "val "; printTyVarSeq tvs; 
       case (pvbs, rvbs) of
          (_, []) => printValBindSeq pvbs
        | ([], _) => (msgString "rec "; printValBindSeq rvbs)
        | (_, _) => (printValBindSeq pvbs; msgString " and rec ";
                     printValBindSeq rvbs))
  | PRIM_VALdec (tvs,vbs) =>
      (msgString "prim_val "; printTyVarSeq tvs; 
       printSeq printPrimValBind " and " vbs)
  | FUNdec (ref (UNRESfundec (tvs, fvalbind))) =>
      (msgString "fun "; printTyVarSeq tvs; 
       printSeq printFValBind " and " fvalbind)
  | FUNdec (ref (RESfundec dec)) =>
      printDec dec
  | TYPEdec tbs =>
      (msgString "type "; printSeq printTypBind " and " tbs)
  | PRIM_TYPEdec(eq, tbs) =>
      (msgString "prim_";
       msgString
         (case eq of
              FALSEequ => ""
            | TRUEequ  => "eq"
            | REFequ   => "EQ"
	    | _ => fatalError "printDec");
       msgString "type "; printSeq printPrimTypBind " and " tbs)
  | DATATYPEdec(dbs, tbs_opt) =>
      (msgString "datatype "; printSeq printDatBind " and " dbs;
       printWithtype tbs_opt)
  | DATATYPErepdec(tycon,tyconpath) =>
      (msgString "datatype ";
       printTyCon tycon;
       msgString " = datatype "; 
       printTyConPath tyconpath
      )
  | ABSTYPEdec(dbs, tbs_opt, dec) =>
      (msgString "abstype "; printSeq printDatBind " and " dbs;
       printWithtype tbs_opt;
       msgString " with "; printDec dec)
  | EXCEPTIONdec ebs =>
      (msgString "exception "; printSeq printExBind " and " ebs)
  | LOCALdec(dec1,dec2) =>
      (msgString "local "; printDec dec1; msgString " in ";
       printDec dec2)
  | OPENdec  longmodidinfos =>
      (msgString "open "; printSeq printIdInfo " " (map #1 longmodidinfos))
  | STRUCTUREdec mbs =>
      (msgString "structure "; printSeq printModBind " and " mbs)
  | FUNCTORdec fbs =>
      (msgString "functor "; printSeq printFunBind " and " fbs)
  | SIGNATUREdec sbs =>
      (msgString "signature "; printSeq printSigBind " and " sbs)
  | EMPTYdec => ()
  | SEQdec(dec1,dec2) =>
      (printDec dec1; msgString "; "; printDec dec2)
  | FIXITYdec(status, ids) =>
      (case status of
           INFIXst i =>
             (msgString "INFIX "; msgInt i; msgString " ")
         | INFIXRst i =>
             (msgString "INFIXR "; msgInt i; msgString " ")
         | NONFIXst =>
             msgString "NONFIX ";
       printSeq msgString " " ids)

and printValBindSeq vbs =
  printSeq printValBind " and " vbs

and printValBind (ValBind(ref p, e)) =
  (msgIBlock 0; printPat p; msgString " ="; msgBreak(1, 2);
   printExp e; msgEBlock())

and printPrimValBind(ii, ty, arity, name) =
  (msgIBlock 0; printIdInfo ii;
   msgString " :"; msgBreak(1, 2);
   printTy ty; msgString " ="; msgBreak(1, 2);
   msgInt arity; msgString " "; printSCon (STRINGscon name);
   msgEBlock())

and printFValBind (_, fclauses) =
  (printSeq printFClause " | " fclauses)

and printFClause (FClause (ref pats, exp)) =
  (msgIBlock 0; printSeq printPat " " pats; msgString " ="; msgBreak(1, 2);
   printExp exp; msgEBlock())

and printWithtype (SOME tbs) =
      (msgString " withtype "; printSeq printTypBind " and " tbs)
  | printWithtype NONE = ()

and printTypBind (tvs, tc, t) =
  (msgIBlock 0; printTyVarSeq tvs; printTyCon tc;
   msgString " ="; msgBreak(1, 2);
   printTy t; msgEBlock())

and printPrimTypBind (tvs, tc) =
  (printTyVarSeq tvs; printTyCon tc)

and printDatBind (tvs, tc, cbs) =
  (msgIBlock 0; printTyVarSeq tvs; printTyCon tc;
   msgString " ="; msgBreak(1, 2);
   printSeq printConBind " | " cbs; msgEBlock())

and printConBind (ConBind(ii, SOME t)) =
      (printIdInfo ii; msgString " of "; printTy t)
  | printConBind (ConBind(ii, NONE)) =
      printIdInfo ii

and printExBind (EXDECexbind(ii, SOME t)) =
      (printIdInfo ii; msgString " of "; printTy t)
  | printExBind (EXDECexbind(ii, NONE)) =
      printIdInfo ii
  | printExBind (EXEQUALexbind(ii, ii')) =
      (msgIBlock 0; printIdInfo ii; msgString " ="; msgBreak(1, 2);
       printIdInfo ii'; msgEBlock())

and printModBind (MODBINDmodbind(modid, me)) =
  (msgIBlock 0; printModId modid; msgString " ="; msgBreak(1, 2);
   printModExp me; msgEBlock())
  | printModBind (ASmodbind(modid,sigexp,exp)) =
  (msgIBlock 0; printModId modid;msgString " as"; msgBreak(1,2); 
   printSigExp sigexp; msgString " ="; 
   msgBreak(1, 2);printExp exp; 
   msgEBlock())

and (*
  printFunBind (FUNBINDfunbind(funid, modid, sigexp, modexp)) =
  (msgIBlock 0; printFunId funid; 
                msgString "("; printModId modid; msgString ":"; 
                               printSigExp sigexp;
                msgString ")";
   msgString " ="; msgBreak(1, 2);
   printModExp modexp; msgEBlock()) *)
  printFunBind (FUNBINDfunbind(funid, me)) =
  (msgIBlock 0; printFunId funid; msgString " ="; msgBreak(1, 2);
   printModExp me; msgEBlock())
 | printFunBind (ASfunbind(funid,sigexp,exp)) =
  (msgIBlock 0; printFunId funid;msgString " as"; msgBreak(1,2); 
   printSigExp sigexp; msgString " ="; 
   msgBreak(1, 2);printExp exp; 
   msgEBlock())

and printSigBind (SIGBINDsigbind(sigid, sigexp)) =
  (msgIBlock 0; printSigId sigid; msgString " ="; msgBreak(1, 2);
   printSigExp sigexp; msgEBlock())

and printModExp _ = msgString "<ModExp>"
and printSigExp _ = msgString "<SigExp>";

;







