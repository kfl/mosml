open List Fnlib Mixture Const Smlexc Smlprim Globals Location;
open Units Types Asynt Asyntfn Primdec Infixst;

fun checkInfixIds loc ids msg =
  if duplicates ids then
    errorMsg loc msg
  else ()
;

fun lookup_iBas (iBas : InfixBasis) id =
  lookupEnv iBas id
  handle Subscript => NONFIXst
;

fun asId_Exp (_, VIDPATHexp(ref (RESvidpath  ii))) =
      let val { qualid, info } = ii in
        if #qual qualid <> "" 
           orelse #withOp info 
           orelse case (#id qualid) of [_] => false | _ => true
      then NONE 
      else SOME ii
      end
  | asId_Exp (_, _) = NONE
;

fun applyId_Exp (ii : IdInfo) exp =
  let val { qualid, info } = ii
      val { idLoc, ... } = info
  in
    (xLR exp, APPexp((idLoc, VIDPATHexp(ref (RESvidpath ii))), exp))
  end
;


fun applyObj_Exp exp1 exp2 = (xxLR exp1 exp2, APPexp(exp1, exp2));

val theExpStack =
{
  pair=pairExp, asId=asId_Exp,
  applyId=applyId_Exp, applyObj=applyObj_Exp
};

fun resolveInfixExp (iBas : InfixBasis) loc exps =
  resolveInfix theExpStack (lookup_iBas iBas) exps
  handle WrongInfix =>
    errorMsg loc "Ill-formed infix expression"
       | MixedAssociativity =>
    errorMsg loc "Mixed left- and right-associative operators of equal precedence"
;

fun asId_Pat (_, VARpat ii) =
      let val { qualid, info } = ii in
        if #qual qualid <> ""
           orelse #withOp info 
           orelse case #id qualid of [_] => false | _ => true
       then NONE else SOME ii
      end
  | asId_Pat (_, _) = NONE
;

fun applyId_Pat ii pat = (xLR pat, CONSpat(ii, pat ));

fun applyObj_Pat pat1 pat2 =
  case pat1 of
      (_, VARpat ii) => (xxLR pat1 pat2, CONSpat(ii, pat2))
    | (loc, _) => errorMsg loc "Non-identifier applied to a pattern"
;

val thePatStack =
  {
    pair=pairPat, asId=asId_Pat,
    applyId=applyId_Pat, applyObj=applyObj_Pat
  }
;

fun resolveInfixPat iBas loc pats =
  resolveInfix thePatStack (lookup_iBas iBas) pats
  handle WrongInfix =>
    errorMsg loc "Ill-formed infix pattern"
       | MixedAssociativity =>
    errorMsg loc "Mixed left- and right-associative operators of equal precedence"
;



fun isInfix iBas id =
  case lookup_iBas iBas id of
    INFIXst _ => true
  | INFIXRst _ => true
  | NONFIXst => false
;

fun patOfIdent (ii : IdInfo) =
  (#idLoc (#info ii), VARpat ii)
;

fun checkNoInfixes iBas (loc, pat') =
  case pat' of
    VARpat{qualid={qual="", id=[id]}, info={withOp=false, ...}} =>
      if isInfix iBas id then
        errorMsg loc "Ill-placed infix in a fun clause"
      else ()
  | _ => ()
;

fun mergeFCIds [] = fatalError "mergeFCIds"
  | mergeFCIds [(ii, cl)] = (ii, [cl])
  | mergeFCIds ((ii, cl) :: rest) =
      let val (ii', cls) = mergeFCIds rest in
        if #id(#qualid ii) <> #id(#qualid ii') then
          errorMsg (#idLoc (#info ii')) "Different function names in clauses"
        else ();
        (ii : IdInfo, cl::cls)
      end
;

datatype 'a Category = INFIXED of 'a | OTHER;

fun categorize iBas (_, pat') =
  case pat' of
    VARpat {info={withOp=true, ...}, ...} => OTHER
  | VARpat (ii as {qualid={qual="", id=[id]}, info={withOp=false, ...}}) =>
      if (isInfix iBas id) then (INFIXED ii) else OTHER
  | _ => OTHER
;


fun resolveFClauseArgs iBas (pats : Pat list) =
  case map (categorize iBas) pats of
    [OTHER, INFIXED ii, OTHER] =>
      (* SUCCESS: case (4) *)
      (case pats of
           [ap1,_,ap2] => (ii, [pairPat ap1 ap2])
         | _ => fatalError "resolveFClauseArgs")
  | OTHER :: _ =>
      (* Try for cases (1)/(2)/(3) *)
      (case pats of
           (_, PARpat(_, INFIXpat (ref (UNRESinfixpat ([ap1,ap2,ap3]))))) :: rest =>
             (* Try for case (3) *)
             (case categorize iBas ap2 of
                  INFIXED ii =>
                    (* SUCCESS: case (3) *)
                    (ii, pairPat ap1 ap3 :: rest)
                | OTHER =>
                    (* `fun (<ap1> <junk> <ap2>)' *)
                    errorMsg (xLR ap2)
                      "Expecting infixed identifier")
         | fst :: snd :: rest =>
             (* Try for cases (1)/(2)... *)
             (case fst of
                 (_, VARpat ii) =>
                   (* ii can't be an infix, because it matches OTHER *)
                   (ii, snd :: rest)
               | (_, _) =>
                   (* `fun <junk> <junk> ...' *)
                   errorMsg (xxLR fst snd) "Ill-formed clause start")
         | _ =>
             (* `fun <ap> = ...' *)
             errorMsg (xLR (hd pats))
               "Ill-formed left hand side of a clause")
  | _ =>
     (* `fun +' or something *)
     errorMsg (xLR (hd pats))
       "Expecting function name or infix pattern"
;

fun resolvePatOp (iBas : InfixBasis) (pat as (loc, pat')) =
  case pat' of
      SCONpat _ => ()
    | VARpat _ => ()
    | WILDCARDpat => ()
    | NILpat _ => fatalError "resolvePatOp"
    | CONSpat(ii, p) => resolvePatOp iBas p
    | EXNILpat _ => fatalError "resolvePatOp"
    | EXCONSpat _ => fatalError "resolvePatOp"
    | EXNAMEpat _ => fatalError "resolvePatOp"
    | REFpat _ => fatalError "resolvePatOp"
    | RECpat(ref (RECrp(fs, dots))) =>
        app_field (resolvePatOp iBas) fs
    | RECpat(ref (TUPLErp _)) => fatalError "resolvePatOp"
    | VECpat ps => app (resolvePatOp iBas) ps
    | PARpat p => resolvePatOp iBas p
    | INFIXpat (InfixPat as ref (UNRESinfixpat ps)) =>
        let val () = app (resolvePatOp iBas) ps
            val pat = resolveInfixPat iBas loc ps
        in
            InfixPat := RESinfixpat pat
        end
    | INFIXpat (ref (RESinfixpat _)) => fatalError "resolvePatOp"
    | TYPEDpat(p,ty) =>
        (resolvePatOp iBas p;resolveTyOp iBas ty)
    | LAYEREDpat(pat1, pat2) =>
        (resolvePatOp iBas pat1;resolvePatOp iBas pat2)

and resolveFClause iBas (FClause(ref pats, exp)) =
  let val (ii, args) = resolveFClauseArgs iBas pats
      val () = app (checkNoInfixes iBas) args
      val () = app (resolvePatOp iBas) args
      val () = resolveExpOp iBas exp
  in (ii, MRule(ref args, exp)) end

and resolveFClauseList iBas fclauses =
  mergeFCIds (map (resolveFClause iBas) fclauses)

and resolveFValBind iBas (loc, fclauses) =
  let val (ii, (mrules : Match)) =
              resolveFClauseList iBas fclauses
      val numArgs = curriedness mrules
  in
    app (fn MRule(ref pats,_) =>
           if numArgs <> List.length pats then
             errorMsg loc "Mismatch in the number of curried arguments"
           else ())
        mrules;
    ValBind(ref (patOfIdent ii), (loc, FNexp mrules))
  end

and resolveVIdPathInfoOp iBas (RESvidpath vidpath') =
      resolveVIdPath'Op iBas vidpath'
  | resolveVIdPathInfoOp iBas (OVLvidpath _) =
      fatalError "resolveVIdPathInfo"
and resolveExpOp iBas (exp as (loc, exp')) =
  case exp' of
    SCONexp _ => ()
  | VIDPATHexp (ref vidpathinfo) => 
      resolveVIdPathInfoOp iBas vidpathinfo
  | FNexp mrules =>
      app (resolveMRuleOp iBas) mrules
  | APPexp(e1, e2) =>
      (resolveExpOp iBas e1; 
       resolveExpOp iBas e2)
  | LETexp(dec, body) =>
      let val iBas' = resolveDecOp iBas dec in
          resolveExpOp (plusEnv iBas iBas') body
      end
  | RECexp(ref (RECre fs)) =>
      (app_field (resolveExpOp iBas) fs)
  | RECexp(ref (TUPLEre _)) =>
      fatalError "resolveExpOp"
  | VECexp es =>
      app (resolveExpOp iBas) es
  | PARexp e =>
      resolveExpOp iBas e
  | INFIXexp (infixexp as ref (UNRESinfixexp es))  =>
      let val _ = app (resolveExpOp iBas) es
          val exp = resolveInfixExp iBas loc es
      in  infixexp := RESinfixexp exp
      end
  | INFIXexp (infixexp as ref (RESinfixexp e))  =>
     fatalError "resolveExpOp"
  | TYPEDexp(e, ty) =>
      (resolveExpOp iBas e;
       resolveTyOp iBas ty)
  | ANDALSOexp(e1, e2) =>
      (resolveExpOp iBas e1; resolveExpOp iBas e2)
  | ORELSEexp(e1, e2) =>
      (resolveExpOp iBas e1; resolveExpOp iBas e2)
  | HANDLEexp(e, mrules) =>
      (resolveExpOp iBas e; 
       app (resolveMRuleOp iBas) mrules)
  | RAISEexp e =>
      resolveExpOp iBas e
  | IFexp(e0, e1, e2) =>
      (resolveExpOp iBas e0;
       resolveExpOp iBas e1;
       resolveExpOp iBas e2)
  | WHILEexp(e1, e2) =>
      (resolveExpOp iBas e1; resolveExpOp iBas e2)
  | SEQexp(e1,e2) =>
      (resolveExpOp iBas e1; resolveExpOp iBas e2)
  | STRUCTUREexp(modexp,sigexp,_) =>
      (resolveModExpOp iBas modexp;resolveSigExpOp iBas sigexp)
  | FUNCTORexp(modexp,sigexp,_) =>
      (resolveModExpOp iBas modexp;resolveSigExpOp iBas sigexp)

and resolveMRuleOp iBas (MRule(ref pats,exp)) =
  (app (resolvePatOp iBas) pats; resolveExpOp iBas exp)

and resolveDecOp (iBas : InfixBasis) (dec as (loc, dec')) =
  case dec' of
    VALdec (tvs, (pvbs, rvbs)) =>
      (app (resolveValBindOp iBas) pvbs; 
       app (resolveValBindOp iBas) rvbs;
       NILenv)
  | PRIM_VALdec (tvs,pvbs) => 
      (app (fn (idinfo,ty,i,s) => resolveTyOp iBas ty) pvbs; 
       NILenv)
  | FUNdec (fundec as (ref (UNRESfundec (tvs, fvbs)))) =>
      let val rvbs = map  (resolveFValBind iBas) fvbs 
      in
          fundec :=  RESfundec (loc, VALdec (tvs,([],rvbs)));
          NILenv
      end
  | FUNdec (ref (RESfundec _)) => fatalError "resolveDecOp"
  | TYPEdec tbds => (app (resolveTypBindOp iBas) tbds;NILenv)
  | PRIM_TYPEdec (tynamequ, typdescs) => NILenv
  | DATATYPEdec (dbds,SOME tbds) => (app (resolveDatBindOp iBas) dbds; app (resolveTypBindOp iBas) tbds;NILenv)
  | DATATYPEdec (dbds,NONE) => (app (resolveDatBindOp iBas) dbds;NILenv)
  | DATATYPErepdec (tycon, tyconpath) =>
      (resolveTyConPathOp iBas tyconpath;
       NILenv)
  | ABSTYPEdec(dbds, SOME tbds, dec2) =>
      (app (resolveDatBindOp iBas) dbds;
       app (resolveTypBindOp iBas) tbds;
       resolveDecOp iBas dec2)
  | ABSTYPEdec(dbds, NONE, dec2) =>
      (app (resolveDatBindOp iBas) dbds;
       resolveDecOp iBas dec2)
  | EXCEPTIONdec ebs => (app (resolveExBindOp iBas) ebs; NILenv)
  | LOCALdec(dec1, dec2) =>
      let val iBas' = resolveDecOp iBas dec1
          val iBas'' = resolveDecOp (plusEnv iBas iBas') dec2 
      in iBas'' end
  | OPENdec modids => NILenv
  | EMPTYdec  => NILenv
  | SEQdec(dec1, dec2) =>
      let val iBas'  = resolveDecOp iBas dec1
          val iBas'' = resolveDecOp (plusEnv iBas iBas') dec2 
      in plusEnv iBas' iBas'' end
  | FIXITYdec(status, ids) =>
      (checkInfixIds loc ids "An identifier appears twice in a fixity declaration";
       (foldL (fn id => fn env => bindInEnv env id status) NILenv ids))
  | STRUCTUREdec modbinds =>
      (app (resolveModBindOp iBas) modbinds;
       NILenv)
  | FUNCTORdec funbinds =>
      (app (resolveFunBindOp iBas) funbinds;
       NILenv)
  | SIGNATUREdec sigbinds =>
      (app (resolveSigBindOp iBas) sigbinds;
       NILenv)

and resolveValBindOp iBas (ValBind(ref pat, exp)) =
  (resolvePatOp iBas pat;resolveExpOp iBas exp)
and resolveExBindOp iBas (EXDECexbind (ii, SOME ty)) =
      resolveTyOp iBas ty
  | resolveExBindOp iBas _ = ()
and resolveTyOp iBas (_,ty') = 
    case ty' of
      TYVARty _ => ()
    | RECty tyrow => (app_field (resolveTyOp iBas) tyrow)
    | CONty (tys,tyconpath) => 
        (app (resolveTyOp iBas) tys;
         resolveTyConPathOp iBas tyconpath)
    | FNty (ty1,ty2) => 
        (resolveTyOp iBas ty1; resolveTyOp iBas ty2)
    | PACKty sigexp =>  resolveSigExpOp iBas sigexp
    | PARty ty => resolveTyOp iBas ty

and resolveTyConPathOp iBas (_,tyconpath) = 
    case tyconpath of
      LONGtyconpath _ => ()
    | WHEREtyconpath (_,_,modexp) => 
        resolveModExpOp iBas modexp
and resolveVIdPath'Op iBas vidpath = ()
and resolveModExpOp iBas (_,(modexp,_)) = 
    case modexp of
      DECmodexp dec => 
        (resolveDecOp iBas dec; ())
    | LONGmodexp _ => ()
    | LETmodexp (dec,modexp) =>
        let val iBas' = resolveDecOp iBas dec 
        in
            resolveModExpOp (plusEnv iBas iBas') modexp 
        end  
   | PARmodexp modexp => resolveModExpOp iBas modexp
   | CONmodexp (modexp,sigexp) =>
       (resolveModExpOp iBas modexp; resolveSigExpOp iBas sigexp)
   | ABSmodexp  (modexp,sigexp) =>
       (resolveModExpOp iBas modexp; resolveSigExpOp iBas sigexp)
   | FUNCTORmodexp (_,modid,_, sigexp, modexp) =>
       (resolveSigExpOp iBas sigexp;resolveModExpOp iBas modexp)
   | APPmodexp (modexp,modexp') =>
       (resolveModExpOp iBas modexp;resolveModExpOp iBas modexp')
   | RECmodexp (modid,_,sigexp,modexp) =>
       (resolveSigExpOp iBas sigexp;resolveModExpOp iBas modexp)
and resolveModBindOp iBas (MODBINDmodbind(modid,modexp)) =
      resolveModExpOp iBas modexp
  | resolveModBindOp iBas (ASmodbind(modid,sigexp,exp)) =
      (resolveSigExpOp iBas sigexp;
       resolveExpOp iBas exp)
and resolveSigBindOp iBas (SIGBINDsigbind(sigid,sigexp)) =
      resolveSigExpOp iBas sigexp
and resolveFunBindOp iBas (FUNBINDfunbind(funid,modexp)) =
     resolveModExpOp iBas modexp
  | resolveFunBindOp iBas (ASfunbind(funid,sigexp,exp)) =
      (resolveSigExpOp iBas sigexp;
       resolveExpOp iBas exp)
and resolveSigExpOp iBas (loc,sigexp) =
  case sigexp of
    SPECsigexp spec => 
	(foldEnv (fn id => fn _ =>  fn ids => 
	   if member id ids
	      then (errorMsg loc 
		    ("Illegal duplicate fixity specification for identifier "^id))
	   else id::ids) [] (resolveSpecOp iBas spec);
	 ())
  | SIGIDsigexp _ => ()
  | WHEREsigexp (sigexp, tyvarseq, longtycon, ty) =>
           (resolveSigExpOp iBas sigexp; resolveTyOp iBas ty)
  | FUNSIGsigexp (_,modid, sigexp,sigexp') =>
           (resolveSigExpOp iBas sigexp;resolveSigExpOp iBas sigexp')
  | RECsigexp (modid, sigexp,sigexp') =>
           (resolveSigExpOp iBas sigexp;resolveSigExpOp iBas sigexp')
and resolveSpecOp (iBas : InfixBasis) (spec as (loc, spec')) =
  case spec' of
    VALspec (tvs, vds) => 
     (app (fn (ii,ty) => resolveTyOp iBas ty) vds;
      NILenv)
  | PRIM_VALspec (tvs,pvbs) => 
     (app (fn (idinfo,ty,i,s) => resolveTyOp iBas ty) pvbs;
      NILenv)
  | TYPEDESCspec _ => 
      NILenv
  | TYPEspec tbds => 
      (app (resolveTypBindOp iBas) tbds;
       NILenv)
  | DATATYPEspec (dbds,SOME tbd) => 
      (app (resolveDatBindOp iBas) dbds;
       app (resolveTypBindOp iBas) tbd;
       NILenv)
  | DATATYPEspec (dbds,NONE) => 
      (app (resolveDatBindOp iBas) dbds;
       NILenv)
  | DATATYPErepspec (tycon, tyconpath) =>
      (resolveTyConPathOp iBas tyconpath;
       NILenv)
  | EXCEPTIONspec eds => 
      (app (resolveExDescOp iBas) eds;
       NILenv)
  | LOCALspec(spec1, spec2) =>
      (resolveSpecOp iBas spec1;
       resolveSpecOp iBas spec2;
       NILenv)
  | OPENspec _ => 
       NILenv
  | EMPTYspec => 
       NILenv
  | SEQspec(spec1, spec2) =>
      let val iBas'  = resolveSpecOp iBas spec1
          val iBas'' = resolveSpecOp (plusEnv iBas iBas') spec2 
      in plusEnv iBas' iBas'' end
  | INCLUDEspec sigexp => 
      (resolveSigExpOp iBas sigexp;
       NILenv)
  | STRUCTUREspec moddescs => 
      (app (resolveModDescOp iBas) moddescs;
       NILenv)
  | FUNCTORspec fundescs => 
      (app (resolveFunDescOp iBas) fundescs;
       NILenv)
  | SHARINGTYPEspec (spec, longtycons) => 
      (resolveSpecOp iBas spec;
       NILenv)
  | SHARINGspec (spec, longmodids) => 
      (resolveSpecOp iBas spec;
       NILenv)
  | FIXITYspec(status, ids) =>
      (checkInfixIds loc ids "An identifier appears twice in a fixity specification";
       (foldL (fn id => fn env => bindInEnv env id status) NILenv ids))
  | SIGNATUREspec sigdescs =>
      (app (resolveSigBindOp iBas) sigdescs;
       NILenv)
and resolveModDescOp iBas (MODDESCmoddesc(modid,sigexp)) =
       resolveSigExpOp iBas sigexp
and resolveFunDescOp iBas (FUNDESCfundesc(funid,sigexp)) =
       resolveSigExpOp iBas sigexp
and resolveTypBindOp iBas (ii,tycon,ty) =
       resolveTyOp iBas ty
and resolveExDescOp iBas (ii,SOME ty) =
       resolveTyOp iBas ty
  | resolveExDescOp iBas (ii,NONE) = ()
and resolveDatBindOp iBas (tyvarseq, tycon, cbds) =
       app (resolveConBindOp iBas) cbds
and resolveConBindOp iBas (ConBind (ii, NONE)) = ()
  | resolveConBindOp iBas (ConBind (ii, SOME ty)) = resolveTyOp iBas ty
;

(* --- resolveToplevelSpec --- *)

fun resolveToplevelSigExp sigexp =
  let val () = resolveSigExpOp (mkGlobalInfixBasis()) sigexp
  in sigexp end
;

fun resolveToplevelSpec spec =
  let val iBas' = resolveSpecOp (mkGlobalInfixBasis()) spec
  in (cleanEnv iBas', spec) end
;

(* --- resolveToplevelDec --- *)


fun resolveToplevelDec dec =
  let val iBas' = resolveDecOp (mkGlobalInfixBasis()) dec
  in (cleanEnv iBas', dec)
  end
;




































