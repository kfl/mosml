open List;
open Fnlib Config Mixture Const Smlexc;
open Globals Location Units Asynt Asyntfn Types;
open Primdec Smlprim; 

 
type UEnv = (string * Type) list;   (* Syntax TyVars to TypeVars *)

val piRef = mkPrimInfo 1 MLPref;

fun mkPrimStatus arity name =
  PRIMname(mkPrimInfo arity (findPrimitive arity name))
;

(* --- Warning printing --- *)

fun isFunType tau =
  case normType tau of
      ARROWt _ => true
    |        _ => false
;

fun unitResultExpected exp tau =
  if isFunType tau then
    (msgIBlock 0;
     errLocation (xLR exp);
     errPrompt "Warning: function-type result is being discarded.";
     msgEOL(); msgEOL();
     msgEBlock())
  else ()
;

(* --- Error printing --- *)

fun typeClash tau1 tau2 reason =
  under_binder 
  (fn (tau1,tau2,reason) =>
    let fun isEqVar tau = case normType tau of 
	                       VARt var => #tvEqu (!var)
			     | _        => false
	fun isExVar tau = case normType tau of 
	                       VARt var => isExplicit var
			     | _        => false
	fun msgTy tau = 
	    if (case reason of 
		    UnifyEquality => true 
		  | _ => false)
		andalso isEqVar tau then 
		(msgString "equality type "; printNextType tau)
	    else if (case reason of 
			 UnifyExplicit => true 
		       | UnifyOther => true
		       | UnifyEquality => true
		       | UnifyScope _ => true
		       | _ => false) 
		    andalso isExVar tau then 
		(msgString "explicit type "; printNextType tau)
	    else 
		(msgString "type"; msgEOL();
		 errPrompt "  "; printNextType tau)
     in
        collectExplicitVars tau1;
	collectExplicitVars tau2; 
        msgString " of "; msgTy tau1; msgEOL();
	errPrompt "cannot have "; msgTy tau2; msgEOL();
	(case reason of
	     UnifyCircular => 
		 (errPrompt "because of circularity"; msgEOL())
	   | UnifyEquality => ()
           | UnifyExplicit => ()
	   | UnifyScope (var,TYNAMEsv tn) => 
		 (errPrompt "because of a scope violation:";
		  msgEOL();
                  errPrompt "the type constructor ";
		  prTyName false tn;
                  msgString " is a parameter " ;
                  msgEOL();
		  errPrompt "that is declared within \
                	    \the scope of ";
		  prTypeVar var;
		  msgEOL()) 
	   | UnifyScope (var,TYPEVARsv tv) => 
		 (errPrompt "because of a scope violation:";
                  msgEOL();
                  errPrompt "the type variable ";
		  prTypeVar tv;
                  msgString " is a parameter " ;
                  msgEOL();
		  errPrompt "that is declared within \
                	    \the scope of ";
		  prTypeVar var;
		  msgEOL())
	   | UnifyTup      => 
		 (errPrompt "because the tuple has the\
		             \ wrong number of components"; 
		  msgEOL())
	   | UnifyRec lab  => 
		 (errPrompt "because record label  "; 
		  printLab lab; msgString "  is missing"; msgEOL())
           | UnifyMod (reasonopt,reasonopt') =>
		 (case reasonopt of 
		       NONE => () 
		     | SOME reason => 
			   (errPrompt "because the first module type \
			               \does not match the second module type ...";
			    msgEOL();
			    errMatchReason "first module type" "second module type" reason);
		   case reasonopt' of 
		       NONE => () 
		     | SOME reason => 
			   (errPrompt "because the second module type \
			               \does not match the first module type ...";
			    msgEOL();
			    errMatchReason "second module type" "first module type" reason))
           | UnifyOther  => ())
    end)
  (tau1,tau2,reason);

fun typeClashId (ii : IdInfo) tau1 tau2 reason =
  let val {qualid, info} = ii in
    msgIBlock 0;
    errLocation (#idLoc info);
    errPrompt "Type clash: identifier "; msgString (showQualId qualid);
    typeClash tau1 tau2 reason;
    msgEBlock();
    raise Toplevel
  end
;

fun unifyId ii tau1 tau2 =
  unify tau1 tau2
  handle Unify reason => typeClashId ii tau1 tau2 reason
;

fun typeClashPat pat tau1 tau2 reason =
(
  msgIBlock 0;
  errLocation (xLR pat);
  errPrompt "Type clash: pattern";
  typeClash tau1 tau2 reason;
  msgEBlock();
  raise Toplevel
);

fun unifyPat pat tau1 tau2 =
  unify tau1 tau2
  handle Unify reason => typeClashPat pat tau1 tau2 reason
;

fun typeClashExp exp tau1 tau2 reason =
(
  msgIBlock 0;
  errLocation (xLR exp);
  errPrompt "Type clash: expression";
  typeClash tau1 tau2 reason;
  msgEBlock();
  raise Toplevel
);

fun unifyExp exp tau1 tau2 =
  unify tau1 tau2
  handle Unify reason => typeClashExp exp tau1 tau2 reason
;

fun unifyMatch mrules tau1 tau2 =
  unify tau1 tau2
  handle Unify reason =>
  let val MRule(ref pats, exp) = hd mrules in
    msgIBlock 0;
    errLocation (xxLR (hd pats) exp);
    errPrompt "Type clash: match rule";
    typeClash tau1 tau2 reason;
    msgEBlock();
    raise Toplevel
  end
;

fun looksLikeInfixId (ii : IdInfo) =
  case ii of
      {qualid={qual="", id = [_]}, info={withOp=false, ...}} => true
    | _ => false
;

fun isPairPat (_, pat') =
  case pat' of
      RECpat(ref (RECrp(fs, NONE))) => isPairRow fs
    | _ => false
;

(*
fun looksLikeInfixExp (_, exp') =
  case exp' of
    VARexp(ref(RESve{qualid={qual=[],...}, info={withOp=false,...}}))
      => true
  | VARexp(ref(OVLve({qualid={qual=[],...}, info={withOp=false,...}}, _, _)))
      => true
  | _ => false
;
*)

fun looksLikeInfixExp (_, exp') =
  case exp' of
    VIDPATHexp(ref(RESvidpath ({qualid={qual="",id=[_]}, info={withOp=false,...}})))
      => true
  | VIDPATHexp(ref(OVLvidpath ({qualid={qual="",id=[_]}, info={withOp=false,...}}, _, _)))
      => true
  | _ => false
;



fun isPairExp (_, exp') =
  case exp' of
    RECexp(ref (RECre fs)) => isPairRow fs
  | _ => false
;

fun newUnknownPair() = type_pair (newUnknown()) (newUnknown());

infix 6 U; infix 7 \\;

fun list_union [] ys = ys
  | list_union (x :: xs) ys =
      if member x ys then (list_union xs ys) else (x :: list_union xs ys)

fun list_subtract xs [] = xs
  | list_subtract xs ys =
      let fun h [] = []
            | h (x :: xs) = if member x ys then (h xs) else (x :: h xs)
      in h xs end
;

fun xs U ys = list_union xs ys;
fun U_map f = foldR_map list_union f [];
fun xs \\ ys = list_subtract xs ys;

infix 7 without;

fun xs without (tyvarseq:TyVarSeq) = 
    xs \\ (map (fn ii => hd(#id(#qualid ii))) tyvarseq);

fun unguardedExp (_, exp') =
  case exp' of
    SCONexp _ => []
  | VIDPATHexp (ref (RESvidpath (_))) => []
  | VIDPATHexp (ref (OVLvidpath (_,ovlty,ty))) => []
  | RECexp(ref (RECre fields)) =>
      U_map (fn(_, e) => unguardedExp e) fields
  | RECexp(ref (TUPLEre es)) => 
      U_map unguardedExp es
  | VECexp es =>
      U_map unguardedExp es
  | LETexp(dec, exp) =>
      unguardedDec dec U unguardedExp exp
  | PARexp exp => unguardedExp exp
  | APPexp(exp1, exp2) =>
      unguardedExp exp1 U unguardedExp exp2
  | INFIXexp (ref (UNRESinfixexp es)) => 
      U_map unguardedExp es
  | INFIXexp (ref (RESinfixexp e)) => unguardedExp e
  | TYPEDexp(exp, ty) =>
      unguardedExp exp U unguardedTy ty
  | ANDALSOexp(exp1, exp2) =>
      unguardedExp exp1 U unguardedExp exp2
  | ORELSEexp(exp1, exp2) =>
      unguardedExp exp1 U unguardedExp exp2
  | HANDLEexp(exp, mrules) =>
      unguardedExp exp U U_map unguardedMRule mrules
  | RAISEexp exp =>
      unguardedExp exp
  | IFexp(e0, e1, e2) =>
      unguardedExp e0 U unguardedExp e1 U unguardedExp e2
  | FNexp mrules =>
      U_map unguardedMRule mrules
  | WHILEexp(exp1, exp2) =>
      unguardedExp exp1 U unguardedExp exp2
  | SEQexp(exp1, exp2) =>
      unguardedExp exp1 U unguardedExp exp2
  | STRUCTUREexp(modexp,sigexp,_) => 
      unguardedModExp modexp U unguardedSigExp sigexp
  | FUNCTORexp(modexp,sigexp,_) => 
      unguardedModExp modexp U unguardedSigExp sigexp
and unguardedMRule (MRule(ref pats, exp)) =
  U_map unguardedPat pats U unguardedExp exp
and unguardedPat (_, pat') =
  case pat' of
    SCONpat _ => []
  | VARpat _ => []
  | WILDCARDpat => []
  | NILpat _ => []
  | CONSpat(_, p) => unguardedPat p
  | EXNILpat _ => []
  | EXCONSpat(_,p) => unguardedPat p
  | EXNAMEpat _ => fatalError "unguardedPat"
(* cvr: TODO review *)
  | REFpat p => unguardedPat p
  | RECpat(ref (RECrp(fs, _))) =>
      U_map (fn(_, p) => unguardedPat p) fs
  | RECpat(ref (TUPLErp _)) => fatalError "unguardedPat"
(* cvr: TODO review *)
  | VECpat ps =>
      U_map unguardedPat ps
  | INFIXpat (ref (RESinfixpat p))  => unguardedPat p
  | INFIXpat (ref (UNRESinfixpat _)) => fatalError "unguardedPat"
(* cvr: TODO review *)
  | PARpat pat => unguardedPat pat
  | TYPEDpat(pat, ty) =>
      unguardedPat pat U unguardedTy ty
  | LAYEREDpat(pat1, pat2) =>
      unguardedPat pat1 U unguardedPat pat2

and unguardedDec (_, dec') =
  case dec' of
    VALdec _ => []
  | PRIM_VALdec _ => []
  | FUNdec (ref (UNRESfundec (tyvarseq, fvbds))) => fatalError "unguardedDec"
(* cvr: TODO review *)
  | FUNdec (ref (RESfundec dec)) => unguardedDec dec
  | TYPEdec tbds => 
	U_map unguardedTypBind tbds
  | PRIM_TYPEdec _ => []
  | DATATYPEdec (dbds,SOME tbds) => 
	(U_map unguardedDatBind dbds) U 
	(U_map unguardedTypBind tbds)
  | DATATYPEdec (dbds,NONE) => 
 	 U_map unguardedDatBind dbds
  | DATATYPErepdec (tycon, tyconpath) =>       
	 unguardedTyConPath tyconpath
  | ABSTYPEdec(dbds,SOME tbds,dec) =>
      (U_map unguardedDatBind dbds) U 
      (U_map unguardedTypBind tbds) U 
      unguardedDec dec
  | ABSTYPEdec(dbds,NONE,dec) =>
      (U_map unguardedDatBind dbds) U 
      unguardedDec dec
  | EXCEPTIONdec ebs =>
      U_map unguardedExBind ebs
  | LOCALdec (dec1, dec2) =>
      unguardedDec dec1 U unguardedDec dec2
  | OPENdec _ => []
  | EMPTYdec => []
  | SEQdec (dec1, dec2) =>
      unguardedDec dec1 U unguardedDec dec2
  | FIXITYdec _ => []
  | STRUCTUREdec mbds => 
      U_map unguardedModBind mbds
  | FUNCTORdec fbds => 
      U_map unguardedFunBind fbds
  | SIGNATUREdec sbds => 
      U_map unguardedSigBind sbds
and unguardedExBind (EXDECexbind(_, SOME ty)) = unguardedTy ty
  | unguardedExBind (EXDECexbind(_, NONE)) = []
  | unguardedExBind (EXEQUALexbind(_,_)) = []
and unguardedValBind (ValBind(ref pat, exp)) =
  unguardedPat pat U unguardedExp exp
and unguardedPrimValBindList (pbs) =
  (U_map (fn (ii,ty,arity,n) => unguardedTy ty) pbs)
and unguardedValDec (pvbs, rvbs) =
  (U_map unguardedValBind pvbs) U
  (U_map unguardedValBind rvbs)
and unguardedTy (_, ty') =
  case ty' of
    TYVARty ii => [hd(#id(#qualid ii))]
  | RECty fs =>
      U_map (fn(_, ty) => unguardedTy ty) fs
  | CONty(tys, tyconpath) => 
      (U_map unguardedTy tys) U unguardedTyConPath tyconpath
  | FNty(ty1, ty2) =>
      unguardedTy ty1 U unguardedTy ty2
  | PACKty(sigexp) =>
      unguardedSigExp sigexp
  | PARty(ty) =>
      unguardedTy ty
and unguardedModBind (MODBINDmodbind(modid,modexp)) =
      unguardedModExp modexp
  | unguardedModBind (ASmodbind(modid,sigexp,exp)) =
      (unguardedSigExp sigexp;
       unguardedExp exp)
and unguardedSigBind (SIGBINDsigbind(sigid,sigexp)) =
      unguardedSigExp sigexp
and unguardedFunBind (FUNBINDfunbind(funid,modexp)) =
     unguardedModExp modexp
  | unguardedFunBind (ASfunbind(funid,sigexp,exp)) =
      (unguardedSigExp sigexp;
       unguardedExp exp)
and unguardedModExp (_,(modexp,_)) = 
    case modexp of
      DECmodexp dec => 
	  unguardedDec dec
   | LONGmodexp _ => []
   | LETmodexp (dec,modexp) =>
	  unguardedDec dec U unguardedModExp modexp
   | PARmodexp modexp => 
	  unguardedModExp modexp
   | CONmodexp (modexp,sigexp) =>
	  unguardedModExp modexp U unguardedSigExp sigexp
   | ABSmodexp (modexp,sigexp) =>
	  unguardedModExp modexp U unguardedSigExp sigexp
   | FUNCTORmodexp (_,modid,_, sigexp, modexp) =>
	  unguardedSigExp sigexp U unguardedModExp modexp 
   | APPmodexp (modexp,modexp') =>
	  unguardedModExp modexp U unguardedModExp modexp'           
   | RECmodexp (modid,_,sigexp, modexp) =>
	  unguardedSigExp sigexp U unguardedModExp modexp 
and unguardedSigExp (_,sigexp) =
  case sigexp of
    SPECsigexp spec => unguardedSpec spec
  | SIGIDsigexp _ => []
  | WHEREsigexp (sigexp, tyvarseq, longtycon, ty) =>
           (unguardedSigExp sigexp U (unguardedTy ty without tyvarseq))
  | FUNSIGsigexp (_,modid, sigexp,sigexp') =>
           (unguardedSigExp sigexp U unguardedSigExp sigexp')
  | RECsigexp (modid, sigexp,sigexp') =>
           (unguardedSigExp sigexp U unguardedSigExp sigexp')
and unguardedSpec (_, spec') = 
  case spec' of
    VALspec _ => []
  | PRIM_VALspec _ => []
  | TYPEDESCspec _ => []
  | TYPEspec tbds => U_map unguardedTypBind tbds
  | DATATYPEspec (dbds,SOME tbds) => 
       (U_map unguardedDatBind dbds) U 
       (U_map unguardedTypBind tbds)
  | DATATYPEspec (dbds,NONE) => 
       U_map unguardedDatBind dbds
  | DATATYPErepspec (tycon, tyconpath) =>       
       unguardedTyConPath tyconpath
  | EXCEPTIONspec eds => U_map unguardedExDesc eds
  | LOCALspec(spec1, spec2) =>
       unguardedSpec spec1 U unguardedSpec spec2
  | OPENspec _ => []
  | EMPTYspec => []
  | SEQspec(spec1, spec2) =>
       unguardedSpec spec1 U unguardedSpec spec2
  | INCLUDEspec sigexp => 
       unguardedSigExp sigexp
  | STRUCTUREspec moddescs => 
       U_map unguardedModDesc moddescs
  | FUNCTORspec fundescs => 
       U_map unguardedFunDesc fundescs
  | SHARINGTYPEspec (spec, longtycons) => 
       unguardedSpec spec
  | SHARINGspec (spec, longmodids) => 
       unguardedSpec spec
  | FIXITYspec _ => 
       []
  | SIGNATUREspec sigdescs =>
      U_map unguardedSigBind sigdescs
and unguardedModDesc (MODDESCmoddesc(modid,sigexp)) =
    unguardedSigExp sigexp
and unguardedFunDesc (FUNDESCfundesc(funid,sigexp)) =
    unguardedSigExp sigexp
and unguardedTyConPath (_,LONGtyconpath _) = []
  | unguardedTyConPath (_,WHEREtyconpath (_,_,modexp)) = 
       unguardedModExp modexp
and unguardedTypBind (tyvarseq,tycon,ty) =
       unguardedTy ty without tyvarseq
and unguardedExDesc (_,SOME ty) =
       unguardedTy ty
  | unguardedExDesc (_,NONE) = []
and unguardedDatBind (tyvarseq, tycon, cbds) =
       (U_map unguardedConBind cbds) without tyvarseq
and unguardedConBind (ConBind (ii, NONE)) = []
  | unguardedConBind (ConBind (ii, SOME ty)) = unguardedTy ty
and unguardedValDescList (vds) =
  (U_map (fn (ii,ty) => unguardedTy ty) vds)
;

(* cvr: TODO the original definition of scopedTyVars appears to be wrong,
   since a variable in pars will not be scoped,
   if it is already scoped in UE 
fun scopedTyVars UE pars unguardedTyVars =
  list_subtract (pars U unguardedTyVars) (map fst UE)
;
*)
(* cvr: REVIEW I think the correct definitions should be: *)
fun scopedTyVars loc UE pars unguardedTyVars =
   let val scopedtyvars = map fst UE
   in
       if (!currentCompliance) <> Liberal 
	   then (app (fn v =>
		      if member v scopedtyvars
			  then case !currentCompliance of
			      Orthodox => 
				  (msgIBlock 0;
				   errLocation loc;
				   errPrompt "Compliance Error: ";msgEOL();
				   errPrompt "The phrase, although accepted as a Moscow ML extension,";msgEOL();
				   errPrompt "is not supported by the Definition of Standard ML:"; msgEOL();
				   errPrompt "the explicit type variable ";msgEOL();
				   errPrompt "  "; msgString v;msgEOL();
				   errPrompt "is already in scope and should not be redeclared";
				   msgEOL();
				   msgEBlock();
				   raise Toplevel)
			    |  Conservative => 
				  (msgIBlock 0;
				   errLocation loc;
				   errPrompt "Compliance Warning: ";msgEOL();
				   errPrompt "The phrase, although accepted as a Moscow ML extension,";msgEOL();
				   errPrompt "is not supported by the Definition of Standard ML:"; msgEOL();
				   errPrompt "the explicit type variable ";msgEOL();
				   errPrompt "  "; msgString v;msgEOL();
				   errPrompt "is already in scope and should not be redeclared";
				   msgEOL();
				   msgEBlock())
			    | _  => ()
		      else ())
	       pars)
       else ();
       (pars U (list_subtract unguardedTyVars scopedtyvars))
   end

;

fun incrUE tyvars =
  map (fn tv => (tv, TypeOfTypeVar(newExplicitTypeVar tv))) tyvars
;

(* Modified to allow more forms of non-expansive expressions: *)

fun isExpansiveExp (_, exp') =
  case exp' of
    SCONexp _       => false
  | VIDPATHexp (ref (RESvidpath (_))) => false
  | VIDPATHexp (ref (OVLvidpath (_,ovlty,ty))) => false
  | PARexp exp      => isExpansiveExp exp
  | TYPEDexp(exp,_) => isExpansiveExp exp
  | FNexp _         => false
  | RECexp (ref (RECre exprow))    => 
	exists (fn (_, e) => isExpansiveExp e) exprow
  | RECexp (ref (TUPLEre explist)) =>
	exists isExpansiveExp explist
  | APPexp((_, VIDPATHexp (ref(RESvidpath ii))), exp) =>
	isExpansiveExp exp orelse
	let val {info = {idKind, ...}, ...} = ii
	in case !idKind of
	    {info = CONik _, qualid = {id, qual}} => id = ["ref"]
	  | {info = EXCONik _, ...}               => false
	  | _                                     => true
	end 
  | APPexp((_,VIDPATHexp (ref(OVLvidpath(ii,_,_)))),exp) =>
	isExpansiveExp exp orelse
	let val {info = {idKind, ...}, ...} = ii
	in case !idKind of
	    {info = CONik _, qualid = {id, qual}} => id = ["ref"]
	  | {info = EXCONik _, ...}               => false
	  | _                                     => true
	end 
  | INFIXexp (ref (RESinfixexp e)) => 
      isExpansiveExp e   
  | INFIXexp (ref (UNRESinfixexp _)) => fatalError "isExpansiveExp: unresolved infix exp"
  | STRUCTUREexp (modexp,_,_) => isExpansiveModExp modexp
  | FUNCTORexp (modexp,_,_) => isExpansiveModExp modexp  
  | _ => true
and isExpansiveModExp (_, (modexp',_)) = 
    case modexp' of
      DECmodexp _ => true
    | LONGmodexp _ => false
    | LETmodexp _ => true
    | PARmodexp modexp => isExpansiveModExp modexp
    | CONmodexp (modexp,_) => isExpansiveModExp modexp
    | ABSmodexp  (modexp,_) => isExpansiveModExp modexp
    | FUNCTORmodexp _ => false
    | APPmodexp _ => true
    | RECmodexp (_,_,_, modexp) => isExpansiveModExp modexp
;   


fun expansiveIdsInValBind (ValBind(ref pat, exp)) acc =
  if (isExpansiveExp exp) then (domPatAcc pat acc) else acc
;

fun closeValBindVE loc (pvbs: ValBind list) VE =
  let val exIds = foldR expansiveIdsInValBind [] pvbs in
    mapEnv (fn id => fn {qualid, info = (t,sc)} => 
        {qualid=qualid,info = (generalization (member id exIds) t,sc)}) VE
  end
;

fun findAndMentionStrSig loc i = 
    let  val cu = findAndMentionSig loc i 
    in  case modeOfSig cu of 
	STRmode => cu
      |	TOPDECmode => (* cvr: TODO in the near future this should be an error, not just a warning *)
	    ((msgIBlock 0;
	      errLocation loc;
	      errPrompt "Warning: this unit was compiled as a sequence of toplevel declarations,";msgEOL();
	      errPrompt "but is being used as if it had been compiled as a structure.";msgEOL();
	      msgEBlock());
	    cu)
    end
;

fun findLongModIdForOpen ME loc q =
  case q of 
     {qual, id = []} => fatalError "findLongModIdForOpen"
   | {qual, id = [i] } => 
	 (let val {qualid,info=RS} = lookupEnv ME i
	      val S = SofRecStr RS
	  in
	      ([],{qualid = qualid,
		   info =  (MEofStr S, 
			    FEofStr S, 
			    NILenv, 
			    VEofStr S,
			    TEofStr S)})
	  end handle Subscript => 
	      let val i = normalizedUnitName i (* cvr: REVIEW *)
	      in
		  if i = #uName(!currentSig) then
		      (msgIBlock 0;
		       errLocation loc;
		       errPrompt "the free structure identifier may not refer to the current unit: ";
		       printQualId q; msgEOL();
		       msgEBlock();
		       raise Toplevel)
		  else
		      let val cu = findAndMentionStrSig loc i (* cvr: REVIEW maybe findAndMention? *)
		      in
			  ([],{qualid = {qual = i,id = []},
			       info = (bindTopInEnv NILenv (#uModEnv cu), 
				       bindTopInEnv NILenv (#uFunEnv cu), 
				       bindTopInEnv NILenv (#uSigEnv cu), 
				       bindTopInEnv NILenv (#uVarEnv cu),
				       bindTopInEnv NILenv (#uTyEnv cu))
			       })
		      end
	      end)
   | _ =>  let val (fields,{qualid,info=RS}) = findLongModId ME loc q
	       val S = SofRecStr RS
	   in
	       (fields,{qualid = qualid,
			info =  (MEofStr S, 
				 FEofStr S, 
				 NILenv, 
				 VEofStr S,
				 TEofStr S)})
	   end
and findLongModId ME loc q =
  case q of 
     {qual, id = []} => fatalError "findLongModId"  
  |  {qual, id = [i] } => 
	 (let val modglobal = lookupEnv ME i
	  in ([],modglobal)
	  end handle Subscript => 
	      let val i = normalizedUnitName i (* cvr: REVIEW *)
	      in
		  if i = #uName(!currentSig) then
		      (msgIBlock 0;
		       errLocation loc;
		       errPrompt  "the free structure identifier may not refer to the current unit: ";
		       printQualId q; msgEOL();
		       msgEBlock();
		       raise Toplevel)
		  else
		      let val cu = findAndMentionStrSig loc i
		      in
			  ([],{qualid = {qual = i,id = []},
			       info =
			         NONrec  (STRstr(bindTopInEnv NILenv (#uModEnv cu), 
					     bindTopInEnv NILenv (#uFunEnv cu), 
					     bindTopInEnv NILenv (#uSigEnv cu), 
					     bindTopInEnv NILenv (#uTyEnv cu), 
					     bindTopInEnv NILenv (#uVarEnv cu)))
			       })
		      end
	      end)
 | {qual, id = i::id} =>
     let val (fields,{qualid = {qual = qual',id = id'}, info = RS}) = findLongModId ME loc {qual = qual , id = id}
     in 
	 let val (field,modglobal) = lookupMEofStr (SofRecStr RS) i  
	 in if isGlobalName (#qualid modglobal) 
	    then ([],modglobal) 
	    else (field::fields,
		  {qualid = {qual = qual', id = i::id'},
		   info = #info modglobal})
	 end handle Subscript => 
	     errorMsg loc ("Unbound structure component: "^(showQualId q))
     end
;


fun findLongVId ME VE loc q =
  case q of 
    {qual, id = []} => fatalError "findLongVId"
 |  {qual, id = [i] } => 
	(([],lookupEnv VE i)
	 handle Subscript => 
	     errorMsg loc ("Unbound value identifier: "^(showQualId q)))
 | {qual, id = i::id} =>
	let val (fields,{qualid = {qual = qual', id = id'},info = RS}) = 
	    findLongModId ME loc {qual = qual , id = id}
	in 
	    let val (field,info) = lookupVEofStr (SofRecStr RS) i 
	    in if isGlobalName (#qualid info)
	       then ([],info) (* inline globals *)	
	       else (field::fields,
		     {qualid = {qual = qual', id = i::id'},
		      info = #info info})
	    end handle Subscript => 
		errorMsg loc ("Unbound value component: "^(showQualId q))
      end
;

fun findLongFunId ME FE loc q =
  case q of 
    {qual, id = []} => fatalError "findLongFunId"
 |  {qual, id = [i] } => 
	(([],lookupEnv FE i)
	 handle Subscript => 
	     errorMsg loc ("Unbound functor identifier: "^(showQualId q)))
 | {qual, id = i::id} =>
	let val (fields,{qualid = {qual = qual', id = id'},info = RS}) = 
	    findLongModId ME loc {qual = qual , id = id}
	in 
	    let val (field,info) = lookupFEofStr (SofRecStr RS) i 
	    in  if isGlobalName (#qualid info)
		then ([],info) (* inline globals *)	
		else (field::fields,
		      {qualid = {qual = qual', id = i::id'},
		       info = #info info})
	    end handle Subscript => 
		errorMsg loc ("Unbound functor component: "^(showQualId q))
      end
;

(* cvr: *)
fun findLongTyCon ME TE loc q =
  case q of 
    {qual, id = []} => fatalError "findLongTyCon"
 |  {qual, id = [i] } => 
      ((lookupEnv TE i)
       handle Subscript => 
           errorMsg loc ("Unbound type constructor: "^(showQualId q)))
 | {qual, id = i::id} =>
      let val (_,{info = RS,...}) = findLongModId ME loc {qual = qual, id = id} 
      in
	  ((lookupEnv (TEofStr (SofRecStr RS)) i) 
	   handle Subscript => 
	       errorMsg loc ("Unbound type component: "^(showQualId q)))
      end
;

fun findLongModIdInStr S loc q =
  case q of 
     {qual, id = []} => fatalError "findLongModIdInStr"         
  |  {qual, id = [i] } => 
      (let val (field,modglobal) = lookupMEofStr S i
       in if isGlobalName (#qualid modglobal) 
          then ([],modglobal) 
	  else ([field],modglobal)
       end handle Subscript => 
       errorMsg loc ("Unbound structure component: "^(showQualId q)))
 | {qual, id = i::id} =>
     let val (fields,{qualid = {qual = qual',id = id'}, info = RS'}) =
	 findLongModIdInStr S loc {qual = qual,id = id} 
     in 
	 let val (field,modglobal) = lookupMEofStr (SofRecStr RS') i  
	 in if isGlobalName (#qualid modglobal) 
	    then ([],modglobal) 
	    else (field::fields,
		  {qualid = {qual = qual', id = i::id'},
		   info = #info modglobal})
	 end handle Subscript => 
	     errorMsg loc ("Unbound structure component: "^(showQualId q))
     end
;

fun findLongTyConInStr S loc q =
  case q of 
    {qual, id = []} => fatalError "findLongTyConInStr"
 |  {qual, id = [i] } => 
      ((lookupEnv (TEofStr S) i)
       handle Subscript => 
           errorMsg loc ("Unbound type component: "^(showQualId q)))
 | {qual, id = i::id} =>
      let val (_,{info = RS',...}) = findLongModIdInStr S loc {qual = qual, id = id} 
      in
	  ((lookupEnv (TEofStr (SofRecStr RS')) i) 
	   handle Subscript => 
	       errorMsg loc ("Unbound type component: "^(showQualId q)))
      end
;


fun findSigId GE loc sigid =
     lookupEnv GE sigid
     handle Subscript => 
	      let val i = normalizedUnitName sigid  (* cvr: TODO review *)
	      in
		  if i = #uName(!currentSig) then
		      (msgIBlock 0;
		       errLocation loc;
		       errPrompt "The free signature identifier may not refer to the current unit: ";
		       msgString sigid; msgEOL();
		       msgEBlock();
		       raise Toplevel)
		  else
		      let val cu = findAndMentionStrSig loc i 
		      (* cvr: TODO review - using the unit's signature 
		         probably shouldn't imply using its implementation *)
		      in
			 case !(strOptOfSig cu) of 
			     NONE => 
				 (msgIBlock 0;
				  errLocation loc;
				  errPrompt "The signature identifier\
				  \ refers to a unit interface, \
				  \ but the unit was not defined \
				  \ with an explicit signature.";
				  msgEOL();
				  msgEBlock();
				  raise Toplevel)
			   | SOME RS =>
				{qualid = {qual = i,id = []},
				 info = (* cvr: remove copySig [] [] *)
				 (LAMBDAsig(!(tyNameSetOfSig cu),
					    STRmod RS))}
		      end
	      end
;

(* Expectations are used by elabModExp to resolve 
   ambiguous longmodid's either longstrids or longfunids 
*)

datatype Expectation = FUNexpected | STRexpected | MODexpected;

fun expectMod (STRmod S) = STRexpected
|   expectMod (FUNmod F) = FUNexpected
;

fun resolveExpectation (MODexpected,true) = FUNexpected
|   resolveExpectation (MODexpected,false) = STRexpected
|   resolveExpectation (expectation,_) = expectation
;

fun reportExpectation expectation ({info = {withOp,...},qualid}:LongModId) =
 case expectation of 
  MODexpected =>
     let val (expected,intended) = 
	 case withOp of
	    false => ("structure.","functor")
	  | true => ("functor.","structure")				    	 in
	  msgIBlock 0;
          errPrompt "(In this context,\
		     \ it is assumed that ";
	     msgEOL();
	  errPrompt "   ";
	     (if withOp then msgString "op " else ());
	     msgString (showQualId qualid);
	     msgEOL();
	  errPrompt " refers to a "; msgString expected; msgEOL();
	  errPrompt " If you actually meant the "; 
	     msgString intended;
	     msgString " of the same name,";
	     msgEOL();
	  errPrompt " you must use the syntax: ";
	     msgEOL();
	  errPrompt "   ";
	     (if withOp then () else msgString "op ");
	     msgString (showQualId qualid);
	     msgEOL();
	  errPrompt " to indicate this.)";msgEOL();
	  raise Toplevel
      end
  | _ => raise Toplevel
;

fun lookup_VE (ME:ModEnv) (VE : VarEnv) (ii : IdInfo) =
  let val {qualid, info} = ii
      val {idLoc, ...} = info
  in
    let val (_,{ qualid = csqualid, info = (sch,_)}) =
             findLongVId ME VE idLoc qualid
    in
    specialization(sch)
    end
    handle Subscript =>
      fatalError "lookup_VE"
  end;


fun lookup_UE (UE : UEnv) loc (ii : IdInfo) =
  let val id = hd(#id(#qualid ii)) in
    lookup id UE
    handle Subscript => errorMsg loc ("Unbound type variable: " ^ id)
  end;

fun lookup_VEForPat ME VE (ii : IdInfo) =
 let val { qualid, info } = ii
     val { idLoc = loc, ... } = info
 in
  case qualid of 
    {qual, id = []} => fatalError "lookup_VEForPat"
 |  {qual, id = [i] } => 
      (let val {qualid = csqualid, info = (_,cs)} = lookupEnv VE i
       in  ([],{qualid = csqualid, info = cs})
       end
       (* Otherwise ii is being defined in the pattern... *)
       handle Subscript => 
          ([],{ qualid = qualid, info=VARname REGULARo }))
 | {qual, id = id as (_::_)} =>
      let val (fields,{qualid = csqualid, info = (_,cs)}) =
              (findLongVId ME VE loc qualid)
      in  (fields,{qualid = csqualid, info = cs})
      end
 end
;

(* syntactic checks *)

fun appOpt f u (SOME x) = f x
  | appOpt f u NONE     = u
;

fun illegalVal id =
    id = "true" orelse id = "false" 
    orelse id = "nil" orelse id = "::" orelse id = "ref"

fun illegalCon id = illegalVal id orelse id = "it"

fun checkRebinding illegal ({qualid={id=lid, ...}, info = {idLoc, ...}} : IdInfo) =
    if illegal (longIdentAsIdent lid "checkRebinding") then
	errorMsg idLoc "Illegal rebinding or respecification"
    else
	();

fun checkAsPatSource (loc, pat') =
  case pat' of
    VARpat _ => ()
  | TYPEDpat((_, VARpat _), _) => ()
  | INFIXpat (ref (UNRESinfixpat _)) => fatalError "checkAsPatSource"
  | INFIXpat (ref (RESinfixpat p)) => 
        checkAsPatSource p
  | _ => errorMsg loc "Ill-formed source of a layered pattern"
;

fun checkRecTy (loc, fs) =
       if duplicates (map fst fs) then
         errorMsg loc "The same label is bound twice in a record type"
       else ()
;


fun checkRecPat (loc, fs) =
       if duplicates (map fst fs) then
         errorMsg loc "The same label is bound twice in a record pattern"
       else ()
;

fun isFnExp (_, exp') =
  case exp' of
    PARexp exp => isFnExp exp
  | TYPEDexp(exp, ty) => isFnExp exp
  | FNexp _ => true
  | _ => false
;

fun checkFnExp exp =
  if isFnExp exp then () else
  errorMsg (xLR exp) "Non-functional rhs expression in val rec declaration"
;

fun inIds (ii : IdInfo) (iis : IdInfo list) =
  exists (fn ii' => #id(#qualid ii) = #id(#qualid ii')) iis;



fun checkDuplIds (iis : IdInfo list) msg =
  case iis of
      [] => ()
    | ii :: iis' =>
        if inIds ii iis' then
          errorMsg (#idLoc (#info ii)) msg
        else checkDuplIds iis' msg
;

fun checkAllIdsIn loc [] iis desc = ()
|   checkAllIdsIn loc (v::vs) iis desc = 
    (if exists (fn (ii':IdInfo) => [v] = #id(#qualid ii'))  iis
     then ()
     else (case (!currentCompliance) of
	       Orthodox => 
		(msgIBlock 0;
		 errLocation loc;
		 errPrompt "Compliance Error: ";msgEOL();
		 errPrompt "The phrase, although accepted as a Moscow ML extension,";msgEOL();
		 errPrompt "is not supported by the Definition of Standard ML:"; msgEOL();
		 errPrompt "the type variable";msgEOL();
		 errPrompt "  "; msgString v;msgEOL();
		 errPrompt "should be a parameter of the ";
		 msgString desc;msgEOL();
		 msgEBlock();
		 raise Toplevel)
	  |  Conservative => 
		(msgIBlock 0;
		 errLocation loc;
		 errPrompt "Compliance Warning: ";msgEOL();
		 errPrompt "The phrase, although accepted as a Moscow ML extension,";msgEOL();
		 errPrompt "is not supported by the Definition of Standard ML:"; msgEOL();
		 errPrompt "the type variable";msgEOL();
		 errPrompt "  "; msgString v;msgEOL();
		 errPrompt "should be a parameter of the ";
		 msgString desc;msgEOL();
		 msgEBlock();
		 checkAllIdsIn loc (drop (fn v' => v = v') vs) iis desc)
	  | _  => ()))
;

fun checkTypBind (tyvars, tycon, ty as (loc,_)) =
( checkDuplIds tyvars
    "Duplicate parameter in a type binding";
  if (!currentCompliance) <> Liberal 
      then checkAllIdsIn loc (unguardedTy ty) tyvars "type binding"  
  else ()
);

fun checkDatBind (tyvars, tycon, cbs) =
(
  app (fn ConBind(ii, SOME (ty as (loc,_))) =>
             (if (!currentCompliance)<> Liberal 
		  then checkAllIdsIn loc (unguardedTy ty) tyvars "datatype binding"  
	      else ())
        | ConBind(ii, NONE) => ())
       cbs;
  checkDuplIds tyvars
    "Duplicate parameter in a datatype binding"
);

fun checkTypDesc (tyvars, tycon) =
  checkDuplIds tyvars
    "Duplicate parameter in a prim_type binding"
; 

(* checkApplicativeModExp dec is used to ensures that module values are
   not opened at top-level within (both generative and applicative) functor bodies 
   (doing so is unsound in the presence of applicative functors).
*)
fun checkApplicativeModExp (_,(modexp,_)) = 
    case modexp of
      DECmodexp dec => 
	  checkApplicativeDec dec
   | LONGmodexp _ => ()
   | LETmodexp (dec,modexp) =>
	  (checkApplicativeDec dec;
	   checkApplicativeModExp modexp)
   | PARmodexp modexp => 
	  checkApplicativeModExp modexp
   | CONmodexp (modexp,sigexp) =>
	  checkApplicativeModExp modexp
   | ABSmodexp (modexp,sigexp) =>
	  checkApplicativeModExp modexp
   | FUNCTORmodexp (_,modid,_, sigexp, modexp) =>
	  ()
          (* checkApplicativeModExp modexp is already ensured by the
             elaboration of modexp *)
   | APPmodexp (modexp,modexp') =>
	  (checkApplicativeModExp modexp;
	   checkApplicativeModExp modexp')
   | RECmodexp (modid,_,sigexp, modexp) =>
	  checkApplicativeModExp modexp 
and checkApplicativeDec (loc,dec') = 
  case dec' of
    ABSTYPEdec(_, _, dec2) =>
      checkApplicativeDec dec2
  | LOCALdec (dec1, dec2) =>
      (checkApplicativeDec dec1;checkApplicativeDec dec2)
  | SEQdec (dec1, dec2) =>
      (checkApplicativeDec dec1;checkApplicativeDec dec2)
  | STRUCTUREdec mbs => 
      app (fn ASmodbind ((loc,_),_,_) =>
	        errorMsg loc "Illegal structure binding: \
		             \a structure value cannot be opened in a functor body"
	   | MODBINDmodbind (_,modexp') => 
		checkApplicativeModExp modexp')
           mbs
  | FUNCTORdec fbs => 
      app (fn ASfunbind ((loc,_),_,_) =>
	        errorMsg loc "Illegal functor binding: \
		             \a functor value cannot be opened in a functor body"
	   | FUNBINDfunbind (_,modexp') => 
		checkApplicativeModExp modexp')
           fbs
  | _ => ()
;

(* semantic checks *)

val bindOnceInEnv = fn env => fn (loc,id) => fn info => fn msg => 
                         (lookupEnv env id;
                          errorMsg loc ("Illegal rebinding of "^id^": "^msg)
                         )
                         handle Subscript => bindInEnv env id info
;

local
(* cvr: TODO share code *)
fun checkNoRebindingsTyEnv loc ids VE msg = 
       foldEnv (fn id => fn _ =>  fn ids => 
		if member id ids
		    then (errorMsg loc 
			  ("Illegal rebinding of type constructor "^id^": "^msg))
		else id::ids) ids VE 
and checkNoRebindingsModEnv loc modids ME msg = 
       foldEnv (fn id => fn _ =>  fn ids => 
		if member id ids
		    then errorMsg loc ("Illegal rebinding of structure identifier "^id^": "^msg)
		else id::ids) modids ME 
and checkNoRebindingsVarEnv  loc vids VE msg = 
       foldEnv (fn id => fn _ =>  fn ids =>
		if member id ids
		    then errorMsg loc ("Illegal rebinding of value identifier "^id^": "^msg) 
		else id::ids) vids VE 
and checkNoRebindingsFunEnv  loc funids FE msg = 
       foldEnv (fn id => fn _ =>  fn ids =>
		if member id ids
		    then errorMsg loc ("Illegal rebinding of functor identifier "^id^": "^msg)
		else id::ids) funids FE 
and checkNoRebindingsSigEnv loc sigids GE msg = 
       foldEnv (fn id => fn _ =>  fn ids =>
		if member id ids
		    then errorMsg loc ("Illegal rebinding of signature identifier "^id^": "^msg)
		else id::ids) sigids GE 
and checkNoRebindingsStr loc (modids,funids,sigids,tycons,vids) S msg = 
    case S of 
       STRstr (ME,FE,GE,TE,VE) =>
        (checkNoRebindingsModEnv loc modids ME msg,
	 checkNoRebindingsFunEnv loc funids FE msg,
	 checkNoRebindingsSigEnv loc sigids GE msg,
         checkNoRebindingsTyEnv loc tycons TE msg,
         checkNoRebindingsVarEnv loc vids VE msg)
    |  SEQstr (S,S') => 
            checkNoRebindingsStr loc (checkNoRebindingsStr loc (modids,funids,sigids,tycons,vids) S msg) S' msg
in
   val checkNoRebindingsStr = fn loc => fn S => fn msg => (checkNoRebindingsStr loc ([],[],[],[],[]) S msg;())
   val checkNoRebindingsTyEnv = fn loc => fn TE => fn msg => (checkNoRebindingsTyEnv loc [] TE msg;())
   val checkNoRebindingsVarEnv = fn loc => fn VE => fn msg => (checkNoRebindingsVarEnv loc [] VE msg;())
end
;

fun errorVarAsCon (ii : IdInfo) =
  errorMsg (#idLoc (#info ii)) "A constructor name expected"
;

fun errorPrimAsCon (ii : IdInfo) =
  errorMsg (#idLoc (#info ii)) "A constructor name expected"
;

fun resolvePatCon ME VE (pat as (loc, pat')) =
  case pat' of
      SCONpat _ => pat
    | VARpat ii =>
        let val (fields,cs) = lookup_VEForPat ME VE ii
            val {qualid, info} = ii
        in
          case #info cs of
            VARname _ =>
              (if #qual qualid <> "" orelse 
                  case #id qualid of [_] => false | _ => true
               then
                 errorMsg (#idLoc info)
                   "Variable names in patterns cannot be qualified"
               else ();
               pat)
          | PRIMname _ =>
              (if #qual qualid <> "" orelse 
                  case #id qualid of [_] => false | _ => true
               then
                 errorMsg (#idLoc info)
                   "Variable names in patterns cannot be qualified"
               else ();
               pat)
          | CONname ci =>
              (if #conArity(!ci) <> 0 then
                 errorMsg (#idLoc info)
                   "Unary constructor in the pattern needs an argument"
               else ();
               #idKind info := { qualid= #qualid cs, info=CONik ci };
               (loc, NILpat ii))
          | EXNname ei =>
              (if #exconArity(!ei) <> 0 then
                 errorMsg (#idLoc info)
                   "Unary exception constructor in the pattern needs an argument"
               else ();
               #idKind info := { qualid= #qualid cs, info=EXCONik ei };
               #idFields info := fields; 
               (loc, EXNILpat ii))
          | REFname   =>
              errorMsg (#idLoc info) "`ref` is used as a variable"
        end
    | WILDCARDpat => pat
    | NILpat ii => fatalError "resolvePatCon"
    | CONSpat(ii, p) =>
        let val (fields,cs) = lookup_VEForPat ME VE ii
            val {qualid, info} = ii
        in
          case #info cs of
            VARname _ => errorVarAsCon ii
          | PRIMname _ => errorPrimAsCon ii
          | CONname ci =>
              (if #conArity(!ci) = 0 then
                 errorMsg (#idLoc info)
                   "Nullary constructor in a pattern cannot be applied"
               else ();
               #idKind info := { qualid= #qualid cs, info=CONik ci };
               (loc, CONSpat(ii, resolvePatCon ME VE p)))
          | EXNname ei =>
              (#idKind info := { qualid= #qualid cs, info=EXCONik ei };
               #idFields info := fields; 
               (loc, EXCONSpat(ii, resolvePatCon ME VE p)))
          | REFname   => (loc, REFpat (resolvePatCon ME VE p))
        end
    | EXNILpat _ => fatalError "resolvePatCon"
    | EXCONSpat _ => fatalError "resolvePatCon"
    | EXNAMEpat _ => fatalError "resolvePatCon"
    | REFpat _ => fatalError "resolvePatCon"
    | RECpat(ref (RECrp(fs, dots))) =>
        (loc, RECpat(ref (RECrp(map_fields (resolvePatCon ME VE) fs, dots))))
    | RECpat(ref (TUPLErp _)) => fatalError "resolvePatCon"
    | VECpat ps =>
        (loc, VECpat (map (resolvePatCon ME VE) ps))
    | PARpat p =>
        (loc, PARpat (resolvePatCon ME VE p))
    | INFIXpat (ref (UNRESinfixpat _)) => fatalError "resolvePatCon"
    | INFIXpat (ref (RESinfixpat p)) => 
        resolvePatCon ME VE p
    | TYPEDpat(p,t) =>
        (loc, TYPEDpat(resolvePatCon ME VE p, t))
    | LAYEREDpat(pat1, pat2) =>
        (loc, LAYEREDpat(resolvePatCon ME VE pat1, resolvePatCon ME VE pat2))
;


fun resolvePatConRec ME VE (pat as (loc, pat')) =
    case pat' of
	VARpat ii =>
	    let val {qualid, info} = ii
	    in
		if #qual qualid <> "" orelse 
                  case #id qualid of [_] => false | _ => true
		then
                 errorMsg (#idLoc info)
                   "Variable names in patterns cannot be qualified"
		else
		    (checkRebinding illegalVal ii;
 		     pat)
	    end
      |  WILDCARDpat => 
	    pat
      | PARpat p               => 
	    (loc, PARpat (resolvePatConRec ME VE p))
      | TYPEDpat(p,t)          => 
	    (loc, TYPEDpat(resolvePatConRec ME VE p, t))
      | LAYEREDpat(pat1, pat2) => 
	    (loc, LAYEREDpat(resolvePatConRec ME VE pat1, resolvePatConRec ME VE pat2))
      | INFIXpat (ref (RESinfixpat p)) => 
	    resolvePatConRec ME VE p
      (* Other errors will be caught later by Synchk.checkRecFnPat *)
      | _ => errorMsg loc "Ill-formed left hand side in recursive binding";


local (* to implement the derived form for structure sharing, 
         adapted from the MLKit 
      *)
    (* cvr: TODO the error messages could be further 
       improved by highlighting the
       location of the longmodid that causes the error *)
    fun update(a,b,m) = (let val bs = lookupEnv m a 
			 in
			     bindInEnv m a (b::bs)
                         end)
			handle Subscript  => bindInEnv m a [b];

    (* We first collect a list of tyname lists which must be identified. *)

    fun collect_TE (loc,T0 : TyName list, path, TEs, acc) : TyName list list =
      let val tcmap = foldL (fn TE => fn acc => 
			     foldEnv (fn tycon => fn tystr => fn acc =>
				     update(tycon,tystr,acc)) 
			     acc 
			     TE) 
	              NILenv 
                      TEs

	  (* Eliminate entries with less than two component, check
	   * arities and flexibility of involved tynames. Further,
	   * extract tynames from type structures. *)

      in
	    foldEnv  (fn tycon => fn tystrs => fn acc =>
                     case tystrs of 
			 [] => acc
		       | [tystr] => acc
                       | tystrs =>
			let fun tystr_to_tyname (tyfun,_) =
			       (choose (equalsTyFunTyName tyfun) T0)
			       handle Subscript => 
				   errorMsg loc 
				       ("Illegal sharing abbreviation: \
					\the type constructor "^
                                        (showQualId {qual="",id=tycon::path})^ 
					" does not denote an opaque type in each equated structure")
			    val tynames = map tystr_to_tyname tystrs   
			    val kind = case tynames of 
				          tn :: _ => kindTyName tn
					| _ => fatalError "collect_TE:2"
			                       (* we know that there are more than zero *)
			    val _ = app (fn tn => if kindTyName tn = kind 
						      then
							  ()
						  else errorMsg loc 
						          ("Illegal sharing abbreviation: \
							   \the type constructor "^ 
							   (showQualId {qual="",id=tycon::path})^ 
							   " does not have the same arity in \
							   \each equated structure"))
                                         tynames
			in tynames::acc
			end) acc tcmap
      end

    fun collect_S (loc,T0, path, Ss, acc) : TyName list list =
      let val (MEs, TEs) = foldL(fn RS => fn (MEs, TEs) =>
				      let val S = SofRecStr RS
					  val ME = MEofStr S
                                          val TE = TEofStr S
				      in (ME::MEs,TE::TEs)
				      end) ([],[]) Ss
	  val acc = collect_ME(loc,T0,path, MEs, acc)
      in collect_TE(loc,T0, path, TEs, acc)
      end

    and collect_ME (loc, T0, path, MEs, acc) : TyName list list =
      let val smap = foldL (fn ME => 
			    fn acc => 
			    foldEnv (fn modid =>
				     fn {qualid,info=S} =>
				     fn acc => 
				     update(modid,S,acc)
				     )
			            acc 
				    ME) 
	                    NILenv 
			    MEs
      in
	    foldEnv (fn strid => fn Ss => fn acc => 
                        case Ss of
			   [] => acc  	  (* Eliminate entries with *)
	                 | [S] => acc     (* less than two components. *)
			 | Ss => collect_S(loc,T0, strid::path,Ss,acc))
	    acc smap
      end


    (* Collapse tynames set if any candidates identify two such *)

    fun emptyIntersection T = fn [] => true 
                              | (tn::T') => not(exists (isEqTN tn) T)
                                              andalso emptyIntersection T T'
    fun union T = fn [] => T
	          |  tn::T' => if exists (isEqTN tn) T 
				   then union T T' 
			       else union (tn::T) T'

    fun collapse ([], Ts) : TyName list list = Ts
      | collapse (T::Ts, Ts') =
      let fun split ([], no, yes) = (no, yes)
	    | split (T'::Ts'', no, yes) =
	      if emptyIntersection T' T then split(Ts'',T'::no, yes)
	      else split(Ts'',no, T'::yes)
      in case split(Ts,[],[])
	   of (no, []) => collapse(no, T::Ts')
	    | (no, yes) => 
	     let val Tnew = foldL union T yes (*cvr: ?*)
	     in collapse(Tnew::no, Ts')
	     end
      end

    (* build the realisation *)

    fun build T0 Ts : TyName list = 
	  case Ts of 
	      [] => []
	    | (T::Ts) => let val T = (* cvr: re-order T as T0 *)
		                   foldR (fn tn => fn acc =>
					  (((choose (isEqTN tn) T)::acc)
					   handle Subscript => acc))
				   [] 
				   T0 
                             val (tn,T') = case T of 
				             [] => fatalError "build" 
					   | tn::T' => (tn,T')
			     val equ = foldR (fn tn => fn equ => 
					      if (#tnEqu (!(#info tn))) <> FALSEequ 
						  then TRUEequ (* cvr: TODO should we worry about REFequ? *)
					      else equ)
                		       FALSEequ 
				       T
			     val _ = setTnEqu (#info tn) equ
			     val () = app (fn {qualid,info} => setTnSort info (REAts (APPtyfun (NAMEtyapp(tn))))) T'
                                       (* cvr: TODO revise - 
					can't we identify type names
					just by identifying their info 
					fields? *)
			     val T'' = build T0 Ts
			 in T' @ T''
			 end
in
    fun share (loc,T0:TyName list, Ss : RecStr list) : TyName list =
	let val Ts = collect_S (loc,T0,[],Ss,[])
	    val Ts : TyName list list = collapse(Ts,[])
            val T = build T0 Ts
	in 
           drop (fn t => exists (isEqTN t) T) T0 
	end
end;

val elabModExpRef = 
    let fun dummyElabModExp (e:Expectation) (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE:TyEnv) (modexp:ModExp) : Globals.ExMod = fatalError "dummyElabModExp"
    in ref dummyElabModExp
    end;

fun elabTyConPath ME FE GE UE VE TE (loc,tyconpath') = 
  case tyconpath' of
      LONGtyconpath longtycon =>
	  findLongTyCon ME TE loc (#qualid longtycon)
    | WHEREtyconpath (longtycon,(_,modid),modexp) =>
	  let
	      val EXISTSexmod(T,M) = 
		  (!elabModExpRef) STRexpected ME FE GE UE VE TE modexp
	      val S = case M of 
		  FUNmod _ => errorMsg loc "Illegal projection: this module expression should be\
		   \ a structure but is actually a functor"
		| STRmod S => S
              val modidinfo = {qualid =  mkLocalName  modid, info = S} 	                   val tyStr = findLongTyCon (bindInEnv ME modid modidinfo) 
		                         TE loc (#qualid longtycon)
              val (fns,_,_) = freeVarsTyStr [] [] ([],[],[]) tyStr 
	  in
	      app (fn tn => 
		   if exists (isEqTN tn) T 
		       then   errorMsg loc "Illegal projection: this projection\
		              \ causes an existential type constructor\
              	              \ to escape its scope"			     		      	   else ()) fns;
              tyStr
	  end;
;

fun applyTyConPath ME FE GE UE VE TE ((tyconpath as (loc,_)) : TyConPath) ts =
  let val (tyfun,_) = elabTyConPath ME FE GE UE VE TE tyconpath
      val arity = List.length ts
  in
    if kindTyFun tyfun <> (ARITYkind arity) then
      errorMsg loc ("Arity mismatch! ")
    else ();
    case tyfun of
        APPtyfun tyapp =>
          CONt(ts, tyapp)
      | TYPEtyfun(pars, body) =>
          type_subst (zip2 pars ts) body 
(* cvr: TODO would this improve sharing? *) 
(*      | TYPEtyfun(pars, body) =>
	  let val tyname = 
	     {qualid = {qual="",id = [""]}, 
	      info = ref {tnKind = ARITYkind arity,
			 tnEqu = TRUEequ, (* cvr: TODO revise *)
			 tnSort = REAts tyfun, 
			 tnStamp = newTyNameStamp(),
			 tnLevel = currentBindingLevel(),
			 tnConEnv = ref NONE}}
	  in
	      CONt(ts, NAMEtyapp tyname)
	  end
*)
(* cvr: end *)
      | LAMtyfun _ => fatalError "applyTyConpath"
  end;

val elabSigExpRef = 
    let fun dummyElabSigExp (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE:TyEnv) (sigexp:SigExp) : Globals.Sig = fatalError "dummyElabSigExp"
    in ref dummyElabSigExp
    end;


fun elabTy (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE:VarEnv) (TE : TyEnv) (loc, ty') =
  case ty' of
    TYVARty ii =>
      lookup_UE UE loc ii
  | RECty fs =>
      (checkRecTy (loc,fs);
       type_rigid_record (map_fields (elabTy ME FE GE UE VE TE) fs))
  | CONty(ty_list,tyconpath) =>
     applyTyConPath ME FE GE UE VE TE tyconpath 
             (map (elabTy ME FE GE UE VE TE) ty_list)
  | FNty(ty,ty') =>
      type_arrow (elabTy ME FE GE UE VE TE ty) 
                 (elabTy ME FE GE UE VE TE ty') 
  | PACKty(sigexp) =>
      let val LAMBDAsig (T,M) = (!elabSigExpRef) ME FE GE UE VE TE sigexp 
      in
	  PACKt (EXISTSexmod(T,M))
      end
  | PARty(ty) =>
      elabTy ME FE GE UE VE TE ty
;

fun elabSCon (INTscon i,    _     ) = type_int
  | elabSCon (CHARscon c,   _     ) = type_char
  | elabSCon (WORDscon c, tyOptRef) =
    let val ty = VARt (newTypeVar false  false  true)
	                       (* nonequ nonimp overloaded *)
    in tyOptRef := SOME ty; ty end
  | elabSCon (REALscon r,   _     ) = type_real
  | elabSCon (STRINGscon s, _     ) = type_string
;

fun elabPat (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv) (TE : TyEnv)   (pat as (loc, pat')) (pat_t : Type) (PE : VarEnv) =
  case pat' of
    SCONpat scon =>
      (unifyPat pat (elabSCon scon) pat_t; PE)
  | VARpat ii =>
     (case ii of 
        {qualid = {id = [id],...}, info={idLoc,...}} =>
          let val q = (* mkName onTop *) mkLocalName  id
              val vi = { qualid=q, info=REGULARo }
          in bindOnceInEnv PE (idLoc,id) 
	      {qualid=q, info= (trivial_scheme pat_t,VARname REGULARo)}
	      "the same value identifier is bound twice in a pattern"
          end
      | {qualid = {id = _,...},...} => fatalError "elabPat: VARpat") 
                                       (* longvid variables are illegal *)
  | WILDCARDpat => PE
  | NILpat ii => (unifyPat pat (lookup_VE ME  VE ii) pat_t; PE)
  | CONSpat(ii, p) =>
      let val id_t = lookup_VE ME VE ii
          val p_t = newUnknown()
          val res_t = newUnknown()
      in
        unifyId ii id_t (type_arrow p_t res_t);
        if (looksLikeInfixId ii) andalso (isPairPat p) then
          (unify p_t (newUnknownPair())
           handle Unify reason =>
             typeClashId ii id_t (type_arrow (newUnknownPair()) res_t) reason)
        else ();
        unifyPat pat res_t pat_t;
        elabPat ME FE GE UE VE TE  p p_t PE
      end
  | EXNILpat ii =>
      let val id_t =  lookup_VE ME VE ii
      in
        unifyId ii id_t type_exn;
        unifyPat pat type_exn pat_t;
        PE
      end
  | EXCONSpat(ii, p) =>
      let val id_t = lookup_VE ME VE ii
          val p_t = newUnknown()
      in
        unifyId ii id_t (type_arrow p_t type_exn);
        if looksLikeInfixId ii andalso isPairPat p then
          (unify p_t (newUnknownPair())
           handle Unify reason =>
             typeClashId ii id_t (type_arrow (newUnknownPair()) type_exn) reason)
        else ();
        unifyPat pat type_exn pat_t;
        elabPat ME FE GE UE VE TE  p p_t PE
      end
  | EXNAMEpat _ => fatalError "elabPat:1"
  | REFpat p =>
      let val p_t = newUnknown() in
        unifyPat pat (type_ref p_t) pat_t;
         elabPat ME FE GE UE VE TE  p p_t PE
      end
  | RECpat(ref (RECrp(fs, dots))) =>
      let val _ = checkRecPat (loc,fs)
	  val ls = map fst fs
          val ps = map snd fs
          val ts = map (fn _ => newUnknown()) ps
          val fs_t = zip2 ls ts
          fun reportClash isRigid reason =
            let val ts' = map (fn _ => newUnknown()) ps
                val fs_t' = zip2 ls ts'
            in
              if isRigid then
                typeClashPat pat (type_rigid_record fs_t') pat_t reason
              else
                typeClashPat pat
                  (type_flexible_record fs_t' (fresh3DotType())) pat_t reason
            end
      in
        (case dots of
            NONE =>     (unify (type_rigid_record fs_t) pat_t
                         handle Unify reason => reportClash true reason)
          | SOME rho => (unify (type_flexible_record fs_t rho) pat_t
                         handle Unify reason => reportClash false reason));
        foldL_zip (elabPat ME FE GE UE VE TE ) PE ps ts
      end
  | RECpat(ref (TUPLErp _)) => fatalError "elabPat:2"
  | VECpat ps =>
      let val p_t = newUnknown() in
        unifyPat pat (type_vector p_t) pat_t;
        foldL (fn p => fn PE => elabPat ME FE GE UE VE TE  p p_t PE) PE ps
      end
  | PARpat p =>
      elabPat ME FE GE UE VE TE  p pat_t PE
  | INFIXpat _ => fatalError "elabPat:3"
  | TYPEDpat(p,ty) =>
      let val ty_t = elabTy ME FE GE UE VE TE ty
          val PE' = elabPat ME FE GE UE VE TE  p pat_t PE
      in
        unifyPat p pat_t ty_t;
        PE'
      end
  | LAYEREDpat(p1,p2) =>
      (checkAsPatSource p1;
       elabPat ME FE GE UE VE TE  p2 pat_t
         (elabPat ME FE GE UE VE TE  p1 pat_t PE))
;


fun freshTyName tycon kind =
  ({qualid = mkLocalName tycon, 
    info = ref {tnKind = kind,
                tnStamp = newTyNameStamp(), 
                tnEqu = TRUEequ,
                tnSort = PARAMETERts,
                tnLevel = currentBindingLevel(),
                tnConEnv = ref NONE 
           }
   } 
  : TyName)
;

fun makeTyName tyvar_list tycon =
  let val arity = List.length tyvar_list
  in freshTyName tycon (ARITYkind arity) end
;

fun initialDatBindTE (dbs : DatBind list)=
  foldL
    (fn (datbind as (tyvar_list, loctycon as (loc,tycon), _)) => fn (LAMBDA(T,env)) =>
       let val _ = checkDatBind datbind
	   val tyname = makeTyName tyvar_list tycon 
       in
        LAMBDA(tyname::T,
              bindOnceInEnv env loctycon 
	       (APPtyfun (NAMEtyapp tyname),ConEnv [])
	       "the same type constructor is bound twice\
               \ in a datatype declaration or specification")
       end)
    (LAMBDA([],NILenv)) dbs
;

fun absTE (TE : TyEnv) = 
  mapEnv
    (fn id => 
      (fn (APPtyfun (NAMEtyapp tyname),ConEnv CE) =>
         let val {info, ...} = tyname in
	     case !(#tnConEnv(!info)) of
		 SOME (ConEnv CE) =>
		     (setTnEqu info FALSEequ;
		      #tnConEnv(!info):= NONE;
		      (APPtyfun (NAMEtyapp tyname),ConEnv []))
	       | _ => fatalError "absTE:1"
	 end
       | _ => fatalError "absTE:2"))
    TE
;

fun elabTypBind (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv)
  (TE : TyEnv) (tb as (tyvars, loctycon, ty) : TypBind) =
  let val _ = checkTypBind tb 
      val (_,id) = loctycon
      val pars = map (fn tyvar => hd(#id(#qualid tyvar))) tyvars
      val _ = incrBindingLevel();
      val vs = map (fn tv => newExplicitTypeVar tv) pars
      val us = map TypeOfTypeVar vs
      val UE' = (zip2 pars us) @ UE
      val ty = elabTy ME FE GE UE' VE TE ty 
      val _ = decrBindingLevel();
      val tyname = makeTyName tyvars id
      val tyfun = APPtyfun (NAMEtyapp(tyname))
  in
    setTnSort (#info tyname) (REAts (TYPEtyfun(vs, ty))); 
    (loctycon, (tyfun,ConEnv []))
  end
;

fun elabTypBindList (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv)
   (TE : TyEnv) (tbs : TypBind list) =
  foldL_map (fn (locid, tyname) => fn env => 
	        bindOnceInEnv env locid tyname
		   "the same type constructor is bound twice in a type declaration")
            (elabTypBind ME FE GE UE VE TE) NILenv tbs
;

fun elabTypBindList_opt (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE : TyEnv) = fn
    SOME tbs => elabTypBindList ME FE GE UE VE TE tbs
  | NONE => NILenv
;

fun elabPrimTypBind equ (typdesc as (tyvars, loctycon) : TypDesc) =
  let val _ = checkTypDesc typdesc
      val (_,id) = loctycon
      val tyname = makeTyName tyvars id
      val tyfun = APPtyfun (NAMEtyapp(tyname))
  in
    setTnEqu (#info tyname) equ;
    LAMBDA([tyname],(loctycon, (tyfun, ConEnv [])))
  end;

fun elabPrimTypBindList equ (tbs : TypDesc list) =
  foldL_map (fn LAMBDA(T',(locid, tystr)) => fn (LAMBDA(T,env)) =>
	     LAMBDA(T@T',bindOnceInEnv env locid tystr
		           "The same tycon is bound twice\
                            \ in a prim_type declaration"))
            (elabPrimTypBind equ) (LAMBDA([],NILenv)) tbs
;

fun closeEE EE =
  mapEnv (fn excon => fn {qualid, info =(t,csd)} => 
         {qualid = qualid, info = (generalization true t,csd)}) EE
;

fun openVE VE =
  mapEnv (fn id => fn {qualid, info = (sch,csd)} => 
              {qualid=qualid, info = (TypeOfScheme sch,csd)}) VE
;

fun isRecTy (loc, ty') =
  case ty' of
    RECty [] => false
  | RECty _ => true
  | _ => false
;

fun arityOfRecTy (loc, ty') =
  case ty' of
      RECty fs => List.length fs
    | _ => fatalError "arityOfRecTy"
;


fun elabConBind (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv) (TE : TyEnv) tvs res_t = fn
    ConBind(ii, SOME ty) =>
      let val _ = checkRebinding illegalCon ii;
	  val {qualid, info} = ii
          val ci = getConInfo ii
          val _ =  setConArity ci 1   
          val arg_t = (elabTy ME FE GE UE VE TE ty)
      in
        setConType ci
          (mkScheme tvs (type_arrow arg_t res_t));
        if #conSpan(!ci) <> 1 andalso isRecTy ty then
          (setConArity ci (arityOfRecTy ty);
           setConIsGreedy ci true)
        else ();
        { qualid= #qualid(!(#idKind(#info ii))), info=ci }
      end
  | ConBind(ii, NONE) =>
      let val _ = checkRebinding illegalCon ii;
	  val {qualid, info} = ii
          val ci = getConInfo ii
          val _ =  setConArity ci 0   
      in
        setConType ci 
         (mkScheme tvs res_t);
        { qualid= #qualid(!(#idKind(#info ii))), info=ci }
      end
;


fun setEquality (TE :TyEnv) = 
  traverseEnv
    (fn _ => fn (tyfun,_) =>
       case tyfun of 
         APPtyfun (NAMEtyapp tyname) =>
          let val {info, ...} = tyname in
            case #tnSort (!info) of
               REAts tyfun => 
                  setTnEqu info (EqualityOfTyFun tyfun)
            | VARIABLEts => fatalError "setEquality"
            | PARAMETERts => fatalError "setEquality"
          end
       | _ => fatalError "setEquality")
    TE
;

val equAttrReset = ref false;

fun maximizeEquality (TE : TyEnv) = 
(
  equAttrReset := true;
  while !equAttrReset do
    (equAttrReset := false;
     traverseEnv
       (fn _ => fn tystr =>
         (case tystr of
	     (APPtyfun (NAMEtyapp tyname),ConEnv CE) =>
               let val {info, ...} = tyname in
                   case #tnEqu(!info) of
                        FALSEequ => ()
                      | TRUEequ  =>
                          if exists (fn ci => schemeViolatesEquality
                                       (#conType (!(#info ci))))
                                    CE
                          then
                            (setTnEqu info FALSEequ; equAttrReset := true)
                          else ()
                      | _ => fatalError "maximizeEquality:1"
               end
         | _ => fatalError "maximizeEquality:2")
        )
       TE)
);


fun setTags (cbs : ConBind list) =
  let prim_val string_of_int : int -> string = 1 "sml_string_of_int";
      val span = List.length cbs
      fun loop n = fn
          [] => ()
        | (ConBind(ii, _)) :: rest =>
            let val {qualid = {id = lid,...},info} = ii
		val id = longIdentAsIdent lid "setTags:1"
                val {idLoc,...} = info
                val _ = app (fn (ConBind ({qualid = {id = lid',...},info = {idLoc=idLoc',...}},_)) =>
			        if id = (longIdentAsIdent lid' "setTags:2") then
				    errorMsg idLoc' "Illegal constructor specification: \ 
				                     \the constructor cannot be specified twice \ 
						     \for the same datatype"
				else ())
                        rest (* cvr: should this check go elsewhere ? *)
                val () =
                  if n > maxBlockTag then
                    errorMsg idLoc ("Implementation restriction:\n \
                                    \A datatype cannot declare more than "^
				    string_of_int (maxBlockTag + 1) ^
				    " constructors.")
                  else ();
                val ci = mkConInfo() 
                val q = mkGlobalName id
                val _ =  #idKind info := { qualid=q, info=CONik ci }
            in
              setConTag ci n;
              setConSpan ci span;
              loop (n+1) rest
            end
  in loop 0 cbs end
;


fun cons x xs = x :: xs;

fun elabDatBind (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE:TyEnv) 
     (datbind as (tyvars, loctycon as (loc,tycon), conbind_list) : DatBind) =
  let val pars = map (fn ii => hd(#id(#qualid ii))) tyvars
      val conbind_list = 
	  Sort.sort (fn ConBind(ii,_) => fn ConBind(ii',_) =>
		     hd(#id(#qualid ii))<=hd(#id(#qualid ii'))) conbind_list
      val () = setTags conbind_list
      val () = incrBindingLevel()
      val vs = map (fn tv => newExplicitTypeVar tv) pars
      val us = map TypeOfTypeVar vs
      val UE' = (zip2 pars us) @ UE
      val (tyfun,_) = lookupEnv TE tycon
      val tyname = case tyfun of 
	              APPtyfun(NAMEtyapp tyname) => tyname
		   | _ => fatalError "elabDatBind"
      val t = type_con us tyname
      val CE = ConEnv (foldR_map cons (elabConBind ME FE GE UE' VE TE vs t) [] conbind_list)
  in
     decrBindingLevel();
     setTnConEnv (#info tyname) (ref (SOME CE)); 
     (VEofCE CE,(loctycon,(tyfun,CE)))
  end
;

fun elabDatBindList (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE : TyEnv) (dbs : DatBind list) =
  foldL_map (fn (VE',(loctycon,tystr)) => fn (VE,TE) => 
	     (plusEnv VE VE',
	      bindOnceInEnv TE loctycon tystr 
	       "The same type constructor is declared twice in a\
               \ datatype declaration"))
            (elabDatBind ME FE GE UE VE TE) (NILenv,NILenv) dbs
;

fun elabExBind  (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv) (TE : TyEnv) onTop = fn
    EXDECexbind(ii, SOME ty) =>
      let val _ = checkRebinding illegalCon ii
	  val {qualid, info = {idLoc,idKind,...}} = ii
          val id = longIdentAsIdent (#id qualid) "elabExBind"
          val ei = mkExConInfo()
          val q = (* mkName onTop *) mkLocalName  id
          val _ = idKind := { qualid=q, info=EXCONik ei };
          val _ = setExConArity ei 1
(* ps:    val _ = if onTop then
                    setExConTag ei (SOME (q, newExcStamp()))
                  else ()
*)
          val arg_t = (elabTy ME FE GE UE VE TE ty)
      in
        if typeIsImperative arg_t then ()
        else errorMsg (xLR ty) "Non-imperative exception type";
(* ps:        if isExConStatic ei andalso isRecTy ty then
          (setExConArity ei (arityOfRecTy ty);
           setExConIsGreedy ei true)
        else ();
*)
        ((idLoc,id), {qualid = q,info = (type_arrow arg_t type_exn, EXNname ei)})
      end
  | EXDECexbind(ii, NONE) =>
      let val _ = checkRebinding illegalCon ii
          val {qualid, info = {idLoc,idKind,...}} = ii
          val id = longIdentAsIdent (#id qualid) "elabDec:EXDECexbind"
          val ei = mkExConInfo()
          val q = (* mkName onTop *) mkLocalName id
          val _ = idKind := { qualid=q, info=EXCONik ei };
          val _ = setExConArity ei 0
(* ps:          val _ = if onTop then
                    setExConTag ei (SOME (q, newExcStamp()))
                  else ()
*)
      in 
            ((idLoc,id), {qualid = q, info = (type_exn, EXNname ei)})
      end
  | EXEQUALexbind(ii, ii') =>
      let val _ = checkRebinding illegalCon ii
	  val {qualid, info={idLoc,idKind,...}} = ii
          val id = longIdentAsIdent (#id qualid) "elabDec:EXEQUALexbind"
          val {qualid=qualid', info=info'} = ii'
          val {idLoc=loc', ...} = info'
          val (fields,{qualid = csqualid, info = (sigma,cs)}) = findLongVId ME VE loc' qualid'
      in
        case cs of
            VARname _ => errorMsg loc'
              ("Variable "^showQualId qualid' ^" is used as an exception name")
          | PRIMname _ => errorMsg loc'
              ("Primitive "^showQualId qualid' ^" is used as an exception name")
          | CONname _ => errorMsg loc'
              ("Constructor "^showQualId qualid' ^" is used as an exception name")
          | REFname   => errorMsg loc'
              "`ref' is used as an exception name"
          | EXNname ei' => (* cvr: TODO review *)
              let val q = (* mkName onTop *) mkLocalName  id in
                #idKind info' := { qualid= csqualid, info=EXCONik ei' };
                #idFields info' := fields; 
                idKind := { qualid= q, info=EXCONik ei' };
                ((idLoc,id), {qualid = q, info = (specialization(sigma), EXNname ei')})
              end
 
      end
; 

fun elabExBindList (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv) (TE : TyEnv) onTop ebs =
  closeEE (foldL_map (fn (locid, tau) => fn env =>
		      bindOnceInEnv env locid tau
		       "The same exception constructor is declared\
                       \ twice in an exception declaration" )
                     (elabExBind ME FE GE UE VE TE onTop) NILenv ebs)
;

(* OVL1TXXo is not a true overloaded type, *)
(* because it needn't be resolved to `int', `real', or `string'. *)
(* This is only a hack to catch the type inferred by the *)
(* type-checker... Thus the attribute `overloaded' mustn't be *)
(* turned on in the type variable. *)
(* The same is true of OVL1TPUo (installPP) and OVL2EEBo (=, <>). *)

fun elabOvlExp t ovltype =
  case ovltype of
      REGULARo =>
        fatalError "elabOvlExp"
    | OVL1NNo =>
        (setCurrentBindingLevel true t;
         type_arrow t t)
    | OVL1NSo =>
        (setCurrentBindingLevel true t;
         type_arrow t type_string)
    | OVL2NNBo =>
        (setCurrentBindingLevel true t;
         type_arrow (type_pair t t) type_bool)
    | OVL2NNNo =>
        (setCurrentBindingLevel true t;
         type_arrow (type_pair t t) t)
    | OVL1TXXo =>
        (setCurrentBindingLevel false t;
         type_arrow t t)
    | OVL1TPUo =>
        (setCurrentBindingLevel false t;
         type_arrow
           (type_arrow type_ppstream (type_arrow t type_unit))
           type_unit)
    | OVL2EEBo =>
        (setCurrentBindingLevel false t;
	 makeEquality t;
         type_arrow (type_pair t t) type_bool)
;

fun elabExp (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv) (TE : TyEnv)
                 (exp as (loc, exp')) exp_t =
  case exp' of
    SCONexp scon =>
      unifyExp exp (elabSCon scon) exp_t
(* cvr: TODO
  | VARexp(ref (RESve ii)) =>
      unifyExp exp (lookup_VE VE ii) exp_t
  | VARexp(ref (OVLve(_, ovltype, tau))) =>
      unifyExp exp (elabOvlExp tau ovltype) exp_t
*)
  | VIDPATHexp (r as (ref (RESvidpath ii))) =>
      let 
	  val {qualid, info} = ii
	  val {idKind, idFields,... } = info
          val (fields,{qualid = csqualid, info = (scheme,cs)}) = 
	      findLongVId ME VE loc qualid
          val tau = specialization(scheme)
      in
        case cs of
          VARname REGULARo =>
            (idKind := { qualid=csqualid, info=VARik };
             idFields := fields;
             unifyExp exp tau exp_t)
        | VARname ovltype =>
            let val tau = newUnknown() in
               r := OVLvidpath (ii, ovltype, tau);
               unifyExp exp (elabOvlExp tau ovltype) exp_t
            end 
        | PRIMname pi =>
            (idKind := { qualid=csqualid, info=PRIMik pi };
             idFields := fields;
             unifyExp exp tau exp_t)
        | CONname ci =>
            (idKind := { qualid=csqualid, info=CONik ci };
             idFields := fields;
             unifyExp exp tau exp_t)
        | EXNname ei =>
            (idKind := { qualid=csqualid, info=EXCONik ei };
             idFields := fields;
             unifyExp exp tau exp_t)
        | REFname   =>
            (idKind := { qualid=csqualid, info=PRIMik piRef };
             idFields := fields;
             unifyExp exp tau exp_t)
      end
  | VIDPATHexp (ref (OVLvidpath _)) =>
       fatalError "elabExp"
  | FNexp mrules =>
      elabMatch ME FE GE UE VE TE mrules exp_t
  | APPexp(func, arg) =>
      let val func_t = newUnknown()
          val () = elabExp ME FE GE UE VE TE func func_t
          val arg_t = newUnknown()
          val res_t = newUnknown()
      in
        unifyExp func func_t (type_arrow arg_t res_t);
        if looksLikeInfixExp func andalso isPairExp arg then
          (unify arg_t (newUnknownPair())
           handle Unify reason =>
             typeClashExp func func_t (type_arrow (newUnknownPair()) res_t) 
	                  reason)
        else ();
        unifyExp exp res_t exp_t;
        elabExp ME FE GE UE VE TE arg arg_t
      end
  | LETexp(dec, body) => 
      let val EXISTS(T,(ME',FE',GE', VE', TE')) =
	        elabDec ME FE GE UE VE TE false  dec 
	  val () = incrBindingLevel();
          val () = refreshTyNameSet PARAMETERts T;
          val tau = 
	      elabExp (plusEnv ME ME') (plusEnv FE FE') (plusEnv GE GE') UE 
	          (plusEnv VE VE') (plusEnv TE TE') body exp_t
      in  decrBindingLevel()
      end
  | RECexp(ref (RECre fs)) =>
      let val ls = map fst fs
	  val _ = if duplicates ls then
	             errorMsg loc "The same label is bound twice in a record expression"
		  else ()
          val es = map snd fs
          val ts = map (fn _ => newUnknown()) es
          val fs_t = zip2 ls ts
      in
        (unify (type_rigid_record fs_t) exp_t
         handle Unify reason =>
           let val ts' = map (fn _ => newUnknown()) es
               val fs_t' = zip2 ls ts'
           in typeClashExp exp (type_rigid_record fs_t') exp_t reason end);
        app2 (elabExp ME FE GE UE VE TE) es ts
      end
  | RECexp(ref (TUPLEre _)) => fatalError "elabExp"
  | VECexp es =>
      let val e_t = newUnknown() in
        app (fn e => elabExp ME FE GE UE VE TE e e_t) es;
        unifyExp exp (type_vector e_t) exp_t
      end
  | PARexp e =>
      elabExp ME FE GE UE VE TE e exp_t
  | INFIXexp (ref (RESinfixexp e)) => 
      elabExp ME FE GE  UE VE TE e exp_t
  | INFIXexp (ref (UNRESinfixexp _)) => fatalError "elabExp: unresolved infix exp"
  | TYPEDexp(e,ty) =>
      let val ty_t = elabTy ME FE GE UE VE TE ty in
        elabExp ME FE GE UE VE TE e exp_t;
        unifyExp e exp_t ty_t
      end
  | ANDALSOexp(e1, e2) =>
      (elabExp ME FE GE UE VE TE e1 type_bool;
       elabExp ME FE GE UE VE TE e2 type_bool;
       unifyExp exp type_bool exp_t)
  | ORELSEexp(e1, e2) =>
      (elabExp ME FE GE UE VE TE e1 type_bool;
       elabExp ME FE GE UE VE TE e2 type_bool;
       unifyExp exp type_bool exp_t)
  | HANDLEexp(e, mrules) =>
      (elabExp ME FE GE UE VE TE e exp_t;
       elabMatch ME FE GE UE VE TE mrules (type_arrow type_exn exp_t))
  | RAISEexp e =>
      elabExp ME FE GE  UE VE TE e type_exn
  | IFexp(e0, e1, e2) =>
      (elabExp ME FE GE UE VE TE e0 type_bool;
       elabExp ME FE GE UE VE TE e1 exp_t;
       elabExp ME FE GE UE VE TE e2 exp_t)
  | WHILEexp(e1, e2) =>
      let val e2_t = newUnknown() in
        elabExp ME FE GE UE VE TE e1 type_bool;
        elabExp ME FE GE UE VE TE e2 e2_t;
        unitResultExpected e2 e2_t;
        unifyExp exp type_unit exp_t
      end
  | SEQexp(e1, e2) =>
      let val e1_t = newUnknown() in
        elabExp ME FE GE UE VE TE e1 e1_t;
        unitResultExpected e1 e1_t;
        elabExp ME FE GE UE VE TE e2 exp_t
      end
  | STRUCTUREexp(modexp,sigexp,r) =>
       let val EXISTSexmod(T,M) = elabModExp STRexpected ME FE GE UE VE TE modexp 
           val _ = case M of STRmod S => ()
	           |  _ => errorMsg loc 
	                    "The encapsulated module expression should be\
                             \ a structure but is actually a functor"
           val LAMBDAsig(T',M') = elabSigExp ME FE GE UE VE TE sigexp
           val _ = case M' of STRmod _ => ()
	           |  _ => errorMsg loc 
	                    "The signature expression should specify\
                             \ a structure but actually specifies a functor" 
       in
           incrBindingLevel();
	   refreshTyNameSet PARAMETERts T;
           refreshTyNameSet VARIABLEts T';
           (matchMod M M'
	      handle MatchError matchReason => 
		  (msgIBlock 0;
		   errLocation loc;
		   errPrompt "Signature mismatch: \
		    \the structure does not match the signature ...";
		   msgEOL();
		   msgEBlock();
		   errMatchReason "structure" "signature" matchReason;
		   raise Toplevel));
           refreshTyNameSet PARAMETERts T'; (* forget the realisation *)
           decrBindingLevel();
           let val X' = EXISTSexmod(T',normMod M') (* re-introduce the quantifier *)
           in
           r := SOME X'; 
           unifyExp exp (PACKt (EXISTSexmod(T',M'))) exp_t 
                  (*    ^^^^^^^^^^^^^^^^^^^^^^^^^^^
                         this type should *not* be normed 
                         because it will be used to match against
                  *)
           end
       end
  | FUNCTORexp(modexp,sigexp,r) =>
       let val EXISTSexmod(T,M) = elabModExp FUNexpected ME FE GE UE VE TE modexp
           val _ = case M of FUNmod _ => ()
	           |  _ => errorMsg loc 
	                    "The encapsulated module expression should be\
                             \ a functor but is actually a structure"
           val LAMBDAsig(T',M') = elabSigExp ME FE GE UE VE TE sigexp
           val _ = case M' of FUNmod _ => ()
	           |  _ => errorMsg loc 
	                    "The signature expression should specify\
                             \ a functor but actually specifies a structure" 
       in
           incrBindingLevel();
	   refreshTyNameSet PARAMETERts T;
           refreshTyNameSet VARIABLEts T';
           (matchMod M M'
	      handle MatchError matchReason => 
		  (msgIBlock 0;
		   errLocation loc;
		   errPrompt "Signature mismatch: \
		    \the functor does not match the signature ...";
		   msgEOL();
		   msgEBlock();
		   errMatchReason "functor" "signature" matchReason;
		   raise Toplevel));
           refreshTyNameSet PARAMETERts T'; (* forget the realisation *)
           decrBindingLevel();
           let val X' = EXISTSexmod(T',normMod M') (* re-introduce the quantifier *)
           in
           r := SOME X';
           unifyExp exp (PACKt (EXISTSexmod(T',M'))) exp_t 
                  (*     ^^^^^^^^^^^^^^^^^^^^^^^^^^^
                         this type should *not* be normed 
                         because it will be used to match against
                  *)
           end
       end

and elabExpSeq  (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv)
  (TE : TyEnv) es ts =
  let fun loop [] [] = ()
        | loop (e :: es) (t :: ts) =
            (elabExp ME FE GE UE VE TE e t; loop es ts)
        | loop _ _ = fatalError "elabExpSeq"
  in loop es ts end

and elabMatch  (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv)
 (TE : TyEnv) mrules match_t =
  let val _ = app (fn MRule(r as ref pats, _) => 
                   r := map (resolvePatCon ME VE) pats)
              mrules
      val MRule(ref pats1,_) = hd mrules 
      val arg_ts = map (fn pat => newUnknown()) pats1
      val res_t = newUnknown()
  in
    unifyMatch mrules (foldR type_arrow res_t arg_ts) match_t;
    app (fn MRule(ref pats, exp) => elabMRule ME FE GE UE VE TE exp res_t pats arg_ts)
            mrules
  end

and elabMRule (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv)
 (TE : TyEnv) exp res_t pats arg_ts =
  case (pats, arg_ts) of
      ([], []) => elabExp ME FE GE UE VE TE exp res_t
    | (pat :: pats', arg_t :: arg_ts') =>
        let val VE' = elabPat ME FE GE UE VE TE (* false *) pat arg_t NILenv
        in elabMRule ME FE GE UE (plusEnv VE VE') TE exp res_t pats' arg_ts' end
    | (_, _) => fatalError "elabMRule"

and elabDatatypeReplication (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) 
        (UE:UEnv) (VE:VarEnv) (TE : TyEnv) 
       (loc,((_,tycon),tyconpath)) =
    let val tyStr as (tyfun,CE) =
	elabTyConPath ME FE GE UE VE TE tyconpath
    in (VEofCE CE,mk1Env tycon tyStr)
    end
and elabDec (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv)
  (TE : TyEnv) (onTop : bool)  (loc, dec') =
  case dec' of
    VALdec (tvs, (pvbs, rvbs)) =>
      let val _ = checkDuplIds tvs "Duplicate explicit type variable"
	  val pars = map (fn ii => hd(#id(#qualid ii))) tvs
	  val tyvars = scopedTyVars loc UE pars (unguardedValDec (pvbs, rvbs))
          val ()   = incrBindingLevel()
          val UE'  = incrUE tyvars @ UE
          val VE'  = elabValBind ME FE GE UE' VE TE  pvbs
          val VE'' = elabRecValBind ME FE GE UE' VE TE  rvbs
      in
        decrBindingLevel();
        EXISTS([],(NILenv,NILenv,NILenv,closeValBindVE loc pvbs (plusEnv VE' VE''), NILenv))
      end
  | PRIM_VALdec (tyvarseq,pbs) => 
       let val _ = checkDuplIds tyvarseq "Duplicate explicit type variable"
	   val pars = map (fn ii => hd(#id(#qualid ii))) tyvarseq
	   val tyvars = scopedTyVars loc UE pars (unguardedPrimValBindList pbs)
	   val () = incrBindingLevel()
           val tvs = map (fn tv => newExplicitTypeVar tv) tyvars
	   val UE' = (zip2 tyvars (map TypeOfTypeVar tvs)) @ UE
	   val VE' = 
	      foldL_map (fn(locid, info) => fn acc => 
  		           bindOnceInEnv acc locid info
			     "the same primitive is declared twice\ 
			      \ in a prim_val declaration")
			 (elabPrimValBind ME FE GE UE' VE TE tvs)
                         NILenv pbs
       in decrBindingLevel();
 	  EXISTS([],(NILenv, NILenv, NILenv, VE',NILenv))
       end
  | FUNdec (ref (UNRESfundec _)) => fatalError "elabDec"
  | FUNdec (ref (RESfundec dec)) => elabDec ME FE GE UE VE TE onTop dec
  | TYPEdec tbs =>
      let val tbsTE = elabTypBindList ME FE GE UE VE TE tbs 
      in
        setEquality tbsTE;
        EXISTS([],(NILenv,NILenv,NILenv,NILenv, tbsTE))
      end
  | PRIM_TYPEdec(equ, tbs) =>
      let val LAMBDA(T',TE') = elabPrimTypBindList equ tbs
      in
      EXISTS(T',(NILenv,NILenv,NILenv,NILenv,TE'))
      end
  | DATATYPEdec(dbs, tbs_opt) =>
      let val LAMBDA(T,dbsTE) = initialDatBindTE dbs
          val _ = incrBindingLevel();
          val _ = refreshTyNameSet PARAMETERts T;
          val tbsTE = elabTypBindList_opt ME FE GE UE VE (plusEnv TE dbsTE) tbs_opt
          (* Here dbsTE will get destructively updated too. *)
          val _ = checkNoRebindingsTyEnv loc (plusEnv dbsTE tbsTE)
	             "the same type constructur is defined twice in this datatype declaration"
          val (VE',dbsTE') = elabDatBindList ME FE GE UE VE (plusEnv (plusEnv TE dbsTE) tbsTE) dbs
          val _ = checkNoRebindingsVarEnv loc VE'
	             "the same constructor is defined twice in this datatype declaration"
      in
        maximizeEquality dbsTE';
        setEquality tbsTE;
        decrBindingLevel();
        EXISTS(T,(NILenv,NILenv,NILenv,VE', plusEnv dbsTE' tbsTE)) 
      end
  | DATATYPErepdec rep => 
    let val (VE,TE) = elabDatatypeReplication ME FE GE UE VE TE (loc,rep)
    in
       EXISTS([],(NILenv,NILenv,NILenv,VE,TE))
    end    
  | ABSTYPEdec(dbs, tbs_opt, dec2) =>
    let   val LAMBDA(T1,dbsTE) = initialDatBindTE dbs
          val _ = incrBindingLevel();
          val _ = refreshTyNameSet PARAMETERts T1;
          val tbsTE = elabTypBindList_opt ME FE GE UE VE (plusEnv TE dbsTE) tbs_opt
          (* Here dbsTE will get destructively updated too. *)
          val _ = checkNoRebindingsTyEnv loc (plusEnv dbsTE tbsTE)
	            "the same type constructur is defined twice in this abstype declaration"	      
          val (VE',dbsTE') = elabDatBindList ME FE GE UE VE (plusEnv (plusEnv TE dbsTE) tbsTE) dbs
          val _ = checkNoRebindingsVarEnv loc VE'
	             "the same constructor is bound twice in this abstype declaration"
          val () = maximizeEquality dbsTE'
          val () = setEquality tbsTE;
          val EXISTS(T2,(ME2,FE2,GE2,VE2, TE2)) =
            elabDec ME FE GE UE (plusEnv VE VE')
                    (plusEnv (plusEnv TE dbsTE') tbsTE) onTop dec2
      in
        (* Now let's destructively update the equality attributes *)
        (* and the lists of constructors! *)
        (* Here VE2 and TE2 will be implicitly influenced too. *)
        let val dbsTE2 = absTE dbsTE'; 
        in
        setEquality tbsTE;
        decrBindingLevel();
        EXISTS(T1@T2,(ME2,FE2,GE2,VE2, plusEnv(plusEnv dbsTE2 tbsTE) TE2))
        end
        (* cvr: *)
      end
  | EXCEPTIONdec ebs =>
     EXISTS([],(NILenv,NILenv,NILenv,(elabExBindList ME FE GE UE VE TE onTop ebs), NILenv))
  | LOCALdec (dec1, dec2) =>
      let val EXISTS(T',(ME',FE',GE',VE', TE')) =
	      elabDec ME FE GE UE VE TE onTop dec1;
          val _ = incrBindingLevel();
          val _ = refreshTyNameSet PARAMETERts T';
          val EXISTS(T'',(ME'', FE'', GE'', VE'',TE'')) =
            elabDec (plusEnv ME ME') (plusEnv FE FE') (plusEnv GE GE') UE (plusEnv VE VE') (plusEnv TE TE') onTop dec2
      in  decrBindingLevel();
	  EXISTS(T'@T'',(ME'', FE'', GE'', VE'', TE''))
      end
  | OPENdec longmodidinfos =>
  EXISTS([],
	 foldL (fn (longmodid,envoptref) => fn (ME',FE',GE',VE',TE') =>
		let val {qualid,info} = longmodid
		    val {idKind, idFields,idLoc,... } = info
		    val (fields,{qualid = csqualid, 
				 info = Env as (ME'',FE'',GE'',VE'',TE'')}) = 
			findLongModIdForOpen ME idLoc qualid
		in  
		    idKind := { qualid=csqualid, info=STRik }; 
		    idFields := fields;
		    envoptref := SOME Env;
		    (plusEnv ME' ME'',
		     plusEnv FE' FE'',
		     plusEnv GE' GE'',
		     plusEnv VE' VE'',
		     plusEnv TE' TE'')
		end)
	 (NILenv,NILenv,NILenv,NILenv,NILenv)
	 longmodidinfos)
  | EMPTYdec => EXISTS([],(NILenv, NILenv,NILenv,NILenv, NILenv))
  | SEQdec (dec1, dec2) =>
      let val EXISTS(T',(ME',FE',GE',VE', TE')) =
            elabDec ME FE GE UE VE TE onTop dec1
          val _ = incrBindingLevel();
          val _ = refreshTyNameSet PARAMETERts T';
          val EXISTS(T'',(ME'', FE'', GE'', VE'',TE'')) =
            elabDec (plusEnv ME ME') (plusEnv FE FE') (plusEnv GE GE')  UE (plusEnv VE VE') (plusEnv TE TE') onTop dec2
      in  (decrBindingLevel();
	   EXISTS(T'@T'',(plusEnv ME' ME'', plusEnv FE' FE'',plusEnv GE' GE'',plusEnv VE' VE'', plusEnv TE' TE'')))
      end
  | FIXITYdec _ => EXISTS([],(NILenv,NILenv,NILenv,NILenv,NILenv))
  | STRUCTUREdec mbs => 
      let val EXISTS(T,ME') = elabModBindList ME FE GE UE VE TE  mbs 
      in    EXISTS(T,(ME',NILenv,NILenv,NILenv, NILenv))
      end
  | FUNCTORdec fbs => 
      let val EXISTS(T,FE') = elabFunBindList ME FE GE UE VE TE  fbs 
      in    EXISTS(T,(NILenv,FE',NILenv,NILenv, NILenv))
      end
  | SIGNATUREdec sbs => 
      let val GE' = elabSigBindList ME FE GE UE VE TE  sbs 
      in    EXISTS([],(NILenv,NILenv,GE',NILenv, NILenv))
      end
and elabModBindList (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv)
   (VE : VarEnv) (TE : TyEnv)  mbs =
  foldL_map 
      (fn EXISTS(T',(locmodid,M')) => fn (EXISTS(T,ME)) => 
       EXISTS(T@T', bindOnceInEnv ME locmodid M' 
	               "the same structure identifier is declared twice\ 
                       \ in a structure declaration"))
     (elabModBind ME FE GE UE VE TE ) (EXISTS([],NILenv)) mbs
and elabModBind (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv)
   (VE : VarEnv) (TE : TyEnv)  
	      (MODBINDmodbind (locmodid as (loc,modid),modexp as (loc',_))) = 
    let val EXISTSexmod(T,M) = elabModExp STRexpected ME FE GE UE VE TE modexp
	val S = case M of 
	           STRmod S => S
		 | FUNmod _ => 
		       errorMsg loc' 
		       "This module expression is actually a functor \
			\but should be a structure"
    in
	  EXISTS(T,(locmodid,{qualid = (* mkName onTop *) mkLocalName  modid, info = S}))
    end
  | elabModBind (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv)
   (VE : VarEnv) (TE : TyEnv)  (ASmodbind (locmodid as (loc,modid),sigexp as (loc',_),exp)) = 
  let val LAMBDAsig(T,M) = elabSigExp ME FE GE UE VE TE sigexp
      val S = case M of FUNmod _ => errorMsg loc' "This signature should specify a structure but actually specifies a functor"
	      |  STRmod S => normRecStr S 
      val tau = elabExp ME FE GE UE VE TE exp (PACKt(EXISTSexmod(T,M)))
  in
        EXISTS(T,(locmodid,{qualid = (* mkName onTop *) mkLocalName  modid, info = S})) 
  end
and elabFunBindList (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv)
   (VE : VarEnv) (TE : TyEnv)  mbs =
  foldL_map 
      (fn EXISTS(T',(locfunid,F')) => fn (EXISTS(T,FE)) => 
       EXISTS(T@T', bindOnceInEnv FE locfunid F'
	               "the same functor identifier is declared twice\ 
                       \ in a functor declaration"))
     (elabFunBind ME FE GE UE VE TE ) (EXISTS([],NILenv)) mbs
and elabFunBind (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv) (TE : TyEnv)  
  (FUNBINDfunbind (locfunid as (loc,funid),modexp as (loc',_))) = 
    let val EXISTSexmod(T,M) = elabModExp FUNexpected ME FE GE UE VE TE modexp
	val F = case M of 
		    FUNmod F => F
	         |  STRmod _ => 
		       errorMsg loc' 
		       "This module expression is actually a structure \
			\but should be a functor"
    in
	  EXISTS(T,(locfunid,{qualid = (* mkName onTop *) mkLocalName  funid, info = F}))
    end
| elabFunBind (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv) (TE : TyEnv)  
   (ASfunbind (locfunid as (loc,funid),sigexp as (loc',_),exp)) =  
      let val LAMBDAsig(T,M) = elabSigExp ME FE GE UE VE TE sigexp
	  val F = case M of
	      STRmod _ => errorMsg loc' "This signature should specify a functor but actually specifies a structure"
	    |  FUNmod F => F
	  val tau = elabExp ME FE GE UE VE TE exp (PACKt(EXISTSexmod(T,M)))
      in
	  EXISTS(T,(locfunid,{qualid = (* mkName onTop *) mkLocalName  funid, info = F}))
      end
and elabSigBindList (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv)
   (VE : VarEnv) (TE : TyEnv)  sbs =
   foldL_map (fn (locsigid,G) => fn GE => 
	      bindOnceInEnv GE locsigid G
	       "the same signature identifier is declared twice\ 
                \ in a signature declaration")
             (elabSigBind ME FE GE UE VE TE ) NILenv sbs
and elabSigBind (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv)
   (VE : VarEnv) (TE : TyEnv)  (SIGBINDsigbind (locsigid as (loc,sigid),sigexp)) = 
  let val G = elabSigExp ME FE GE UE VE TE sigexp
  in
       (locsigid, {qualid = mkLocalName sigid, info = G})
  end
and elabModExp expectation (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv)
   (VE : VarEnv) (TE : TyEnv) (loc,(modexp',r)) = 
  case modexp' of
      DECmodexp dec => 
       let 
	   val EXISTS(T',(ME',FE',GE',VE',TE')) = elabDec ME FE GE UE VE TE false dec
           val exmod = EXISTSexmod(T',(STRmod (NONrec (STRstr (sortEnv ME',
							       sortEnv FE',
							       NILenv,
							       sortEnv TE',
							       sortEnv VE')))))
       in
           r :=  SOME exmod;
           exmod
       end
   |  LONGmodexp (ii as {info={withOp,...},qualid}) => 
       ((case resolveExpectation(expectation,withOp) of
	     STRexpected =>
	       let val {qualid, info} = ii
		   val {idKind, idFields,... } = info
		   val (fields,{qualid=resqualid,info=S}) = 
		       findLongModId ME loc qualid
		   val X = EXISTSexmod([],STRmod ((* cvr: avoid*) copyRecStr  [] [] S))
	       in
		     idKind := {qualid = resqualid, info = STRik};
		     idFields := fields;
		     r :=  SOME X;
		     X
	       end 
	   | FUNexpected =>
	       let val {qualid, info} = ii
		   val {idKind, idFields,... } = info
		   val (fields,{qualid=resqualid,info=F}) = 
		       findLongFunId ME FE loc qualid
		   val X = EXISTSexmod([],FUNmod ((* cvr: avoid*) copyGenFun  [] [] F))
	       in
		   idKind := {qualid = resqualid, info = FUNik};
		   idFields := fields;
		   r :=  SOME X;
		   X
	       end
	   | _ => fatalError "elabModExp:resolveExpectation")
      handle Toplevel => reportExpectation expectation ii)
    | CONmodexp (modexp,sigexp) =>
       let 
           val LAMBDAsig(T',M') = elabSigExp ME FE GE UE VE TE sigexp 
           val EXISTSexmod(T,M) = elabModExp (expectMod M') ME FE GE UE VE TE modexp
       in
           incrBindingLevel();
	   refreshTyNameSet PARAMETERts T;
           refreshTyNameSet VARIABLEts T';
           ((matchMod M M')
	      handle MatchError matchReason => 
		  (msgIBlock 0;
		   errLocation loc;
		   errPrompt "Signature mismatch: \
		    \the module does not match the signature ...";
		   msgEOL();
		   msgEBlock();
		   errMatchReason "module" "signature" matchReason; 
		   raise Toplevel));
           decrBindingLevel();
           let val X' = EXISTSexmod(T,normMod M')
           in
	       r := SOME X'; 
	       X' 
           end
       end
    | ABSmodexp (modexp,sigexp) =>
       let val LAMBDAsig(T',M') = elabSigExp ME FE GE UE VE TE sigexp 
	   val EXISTSexmod(T,M) = elabModExp (expectMod M') ME FE GE UE VE TE modexp
       in
           incrBindingLevel();
	   refreshTyNameSet PARAMETERts T;
           refreshTyNameSet VARIABLEts T';
           ((matchMod M M')
	      handle MatchError matchReason => 
		  (msgIBlock 0;
		   errLocation loc;
		   errPrompt "Signature mismatch: \
		    \the module does not match the signature ..."; 
		   msgEOL();
		   msgEBlock();
		   errMatchReason "module" "signature" matchReason; (* cvr: TODO improve descs*)
		   raise Toplevel));
           refreshTyNameSet PARAMETERts T'; (* forget the realisation *)
           (* cvr: REVIEW forgetting the realisation *only* works if
	      we haven't done path compression on any realised type names in M that
	      pointed to T' (usually because of sharing constraints), otherwise
	      those names won't have their realisations forgotten *)
           decrBindingLevel();
           let val X' = EXISTSexmod(T',normMod M') (* re-introduce the quantifier *)
           in
	       r := SOME X'; 
	       X' 
           end
       end
   | LETmodexp (dec, modexp) =>
      let 
	  val EXISTS(T',(ME',FE',GE',VE', TE')) =
	      elabDec ME FE GE UE VE TE false dec;
          val _ = incrBindingLevel();
          val _ = refreshTyNameSet PARAMETERts T';
          val EXISTSexmod(T'',M) =
            elabModExp expectation (plusEnv ME ME') (plusEnv FE FE') (plusEnv GE GE') UE (plusEnv VE VE') (plusEnv TE TE') modexp
      in  decrBindingLevel();
          let val X' = EXISTSexmod(T'@T'',M) (* re-introduce the quantifier *)
          in
	      r := SOME X'; 
	      X' 
          end
      end
  | FUNCTORmodexp (Generative standard,(loc,modid),idKindDescRef,sigexp,modexp) =>
      let val _ = checkApplicativeModExp modexp
	  val LAMBDAsig (T,M) = elabSigExp ME FE GE UE VE TE sigexp
	  val (ME',FE') = case M of 
	      STRmod S =>
                  (idKindDescRef := STRik;
		   (bindInEnv ME modid {qualid=mkLocalName modid,
					info = normRecStr S},
		    FE))
	    | FUNmod F => 
                  (idKindDescRef := FUNik;
		   (ME,
		    bindInEnv FE modid {qualid=mkLocalName modid,
					info = F}))
	  val _ = incrBindingLevel();
	  val _ = refreshTyNameSet PARAMETERts T;
	  val X = elabModExp MODexpected ME' FE' GE UE VE TE modexp
      in 
	  decrBindingLevel();
	  let val X' = EXISTSexmod([],FUNmod(T,M,X))
	  in
	      r := SOME X';
	      X'
	  end
      end
  | FUNCTORmodexp (Applicative,(loc,modid),idKindDescRef,sigexp,modexp) =>
      let val LAMBDAsig (T,M) = elabSigExp ME FE GE UE VE TE sigexp
	  val (ME',FE') = case M of 
	      STRmod S =>
                  (idKindDescRef := STRik;
		   (bindInEnv ME modid {qualid=mkLocalName modid,
					info = normRecStr S},
		    FE))
	    | FUNmod F => 
                  (idKindDescRef := FUNik;
		   (ME,
		    bindInEnv FE modid {qualid=mkLocalName modid,
					info = F}))
	  val _ = incrBindingLevel(); (* cvr: TODO review *)
	  val _ = incrBindingLevel();
	  val _ = refreshTyNameSet PARAMETERts T;
	  val EXISTSexmod(T',M') = elabModExp MODexpected ME' FE' GE UE VE TE modexp
      in 
	  decrBindingLevel();
	  let 
              val (T'',T'toT'') = parameteriseTyNameSet T' T
	      val X' = EXISTSexmod(T'',FUNmod(T,M,EXISTSexmod([],copyMod T'toT'' [] M')))
	  in
	      decrBindingLevel();
	      r := SOME X';
	      X'
	  end
      end
   | APPmodexp (funmodexp as (loc',_),modexp) =>
       let
           val EXISTSexmod(T,M) = elabModExp FUNexpected ME FE GE UE VE TE funmodexp
           val (T',M',X) = case M of 
	       FUNmod F => copyGenFun [] [] F
	     | STRmod _ => errorMsg loc' "Illegal application: this module expression \
                                            \is a structure but should be a functor"
           val EXISTSexmod(T'',M'') = elabModExp (expectMod M') ME FE GE UE VE TE modexp

       in
           incrBindingLevel();
	   refreshTyNameSet PARAMETERts T;
           refreshTyNameSet VARIABLEts T';
	   refreshTyNameSet PARAMETERts T'';
           (matchMod M'' M'
	      handle MatchError matchReason => 
		  (msgIBlock 0;
		   errLocation loc;
		   errPrompt "Signature mismatch: \
		    \the argument module does not match the functor's domain ...";
		   msgEOL();
		   msgEBlock();
		   errMatchReason "actual argument" "formal argument" matchReason;
		   raise Toplevel));
           decrBindingLevel();
           let val X' = let val EXISTSexmod(T''',M''') = X
			in
			   EXISTSexmod(T@T''@T''',normMod M''')
			end
           in
	       r := SOME X'; 
	       X' 
           end
       end
   | PARmodexp modexp =>
      let val X = elabModExp expectation ME FE GE UE VE TE modexp
      in r:= SOME X;
         X
      end
  | RECmodexp ((loc,modid),inforef,
	       sigexp as (locsigexp,_),
	       modexp as (locmodexp,_)) =>
      let val LAMBDAsig (T,M) = elabSigExp ME FE GE UE VE TE sigexp
	  val (ME',RS) = case M of 
	      STRmod RS =>
                  (let val normRS = normRecStr RS
		   in
		       inforef := SOME normRS;
		       (bindInEnv ME modid {qualid=mkLocalName modid,
					    info = normRS},
			RS)
		   end)
	    | FUNmod F => 
		  errorMsg locsigexp "Illegal recursive structure: \
		   \the forward specification should specify \
		   \a structure but actually specifies a functor"
	  val _ = incrBindingLevel();
          val _ = refreshTyNameSet PARAMETERts T;
	  val EXISTSexmod(T',M') = elabModExp STRexpected ME' FE GE UE VE TE modexp
	  val RS' = case M' of 
	      STRmod RS' =>
                  RS'
	    | FUNmod F => 
		  errorMsg locmodexp "Illegal recursive structure: \
		                     \the body should be \
				     \a structure but is actually a functor"
          val _ = refreshTyNameSet PARAMETERts T';
          val _ = matchMod (STRmod RS') (STRmod RS)
		      handle MatchError matchReason => 
			  (msgIBlock 0;
			   errLocation loc;
			   errPrompt "Illegal recursive structure: \
			    \the body does not enrich the forward specification...";
			   msgEOL();
			   msgEBlock();
			   errMatchReason "body" "forward specification" matchReason;
			   raise Toplevel)
      in 
	  decrBindingLevel();
	  let val X' = EXISTSexmod(T@T',STRmod (normRecStr RS'))
	  in
	      r := SOME X';
	      X'
	  end
      end

and elabValBind (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv)
 (TE : TyEnv)  (vbs : ValBind list) =
  let val _ = app (fn ValBind(r as ref p, e) => 
                     r := resolvePatCon ME  VE p) 
                  vbs 
      val ps = map (fn ValBind(ref p,e) => p) vbs
      val es = map (fn ValBind(ref p,e) => e) vbs
      val pts = map (fn _ => newUnknown()) ps
      val VE' = foldL_zip (elabPat ME FE GE UE VE TE ) NILenv ps pts
      val VE'' = mkHashEnv (length ps) VE'
  in
    app2 (elabExp ME FE GE UE VE TE) es pts;
    openVE VE''
  end

and elabRecValBind (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv) (TE : TyEnv)  (vbs : ValBind list) =
  let val _ = app (fn ValBind(r as ref p, e) => 
                     r := resolvePatConRec ME VE p) 
                  vbs 
      val ps = map (fn ValBind(ref p,e) => p) vbs
      val es = map (fn ValBind(ref p,e) => e) vbs
      val _  = app checkFnExp es
      val pts = map (fn _ => newUnknown()) ps
      val VE' = foldL_zip (elabPat ME FE GE UE VE TE ) NILenv ps pts
      val VE'' = mkHashEnv (length ps) VE'
      val rec_VE = plusEnv VE VE''
  in
    app2 (elabExp ME FE GE UE rec_VE TE) es pts;
    openVE VE''
  end
and elabPrimValBind ME FE GE UE VE TE tvs (ii, ty, arity, n) =
  let (* cvr: REVIEW  val _ = checkRebinding illegalVal ii 
       *)
      val ty_t = elabTy ME FE GE UE VE TE ty
      val {qualid, info = {idLoc,...}} = ii
      val pid = longIdentAsIdent (#id qualid) "elabPrimValBind"
      val q = (* mkName onTop *) mkLocalName pid 
  in ((idLoc,pid),
      {qualid =q,
       info=(mkScheme tvs ty_t,mkPrimStatus arity n)})
  end




and elabValDesc (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE : TyEnv) tvs ((ii, ty) : ValDesc) =
  let val _ = checkRebinding illegalVal ii
      val ty_t = elabTy ME FE GE UE VE TE ty
      val {qualid, info = {idLoc,...}} = ii
      val vid = longIdentAsIdent (#id qualid) "elabValDesc"
      val q = (* mkGlobalName *)  mkLocalName vid 
  in ((idLoc,vid), {qualid = q, info = (mkScheme tvs ty_t,VARname (REGULARo))}) end

and elabExDesc (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv) (TE : TyEnv) onTop
               ((ii, ty_opt) : ExDesc) =
  let val _ = checkRebinding illegalCon ii
      val {qualid, info = {idLoc,idKind,...}} = ii
      val eid = longIdentAsIdent (#id qualid) "elabExDesc"
      val ei = mkExConInfo()
      val q = (* mkGlobalName *) mkLocalName eid  
  in
    idKind := { qualid=q, info=EXCONik ei };
    (case ty_opt of
          SOME _ => setExConArity ei 1
        | NONE   => setExConArity ei 0);
    case ty_opt of
      SOME ty =>
        let 
            val arg_t = (elabTy ME FE GE UE VE TE ty)
        in
          if typeIsImperative arg_t then ()
          else errorMsg (xLR ty) "Non-imperative exception type";
          ((idLoc,eid), {qualid = q, info =(type_arrow arg_t type_exn, EXNname ei)})
        end
    | NONE =>
        ((idLoc,eid), {qualid = q, info = (type_exn,EXNname ei)})
  end

and elabExDescList (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv)
  (VE : VarEnv) (TE : TyEnv) onTop eds =
  closeEE (foldL_map (fn (locid, tau) => fn env =>
		      bindOnceInEnv env locid tau
		      	       "the same exception constructor is specified twice\ 
				\ in an exception specification")
                     (elabExDesc ME FE GE UE VE TE onTop) NILenv eds)


and elabSigExp (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE:TyEnv)
  (loc, sigexp) = 
  (case sigexp of
      SPECsigexp spec => 
	  let val LAMBDA(T,S) = elabSpec ME FE GE UE VE TE false spec
              val _ = checkNoRebindingsStr loc S
     	                "the same identifier is specified twice in the body of this signature"
	  in LAMBDAsig (T,STRmod (NONrec (removeGEofStr S)))
	  end
   |  SIGIDsigexp (loc,sigid) => 
          let val {qualid,info = G} = findSigId GE loc sigid
          in copySig [] [] G
          end
   | FUNSIGsigexp (Generative standard,(loc,modid),sigexp,sigexp') =>
          let val LAMBDAsig(T,M) = elabSigExp ME FE GE UE VE TE  sigexp
	      val (ME',FE') = case M of 
				    STRmod S =>
					(bindInEnv ME modid {qualid=mkLocalName modid,
							     info = normRecStr S},
					 FE)
				  | FUNmod F => 
					(ME,
					 bindInEnv FE modid {qualid=mkLocalName modid,
							     info = F})
	      val _ = incrBindingLevel();
	      val _ = refreshTyNameSet PARAMETERts T;
	      val LAMBDAsig(T',M') = elabSigExp ME' FE' GE UE VE TE sigexp'
	  in
	    (decrBindingLevel();
	     LAMBDAsig([],FUNmod (T,M,(EXISTSexmod(T',M')))))
	  end
   | FUNSIGsigexp (Applicative,(loc,modid),sigexp,sigexp') =>
          let val LAMBDAsig(T,M) = elabSigExp ME FE GE UE VE TE  sigexp
	      val (ME',FE') = case M of 
				    STRmod S =>
					(bindInEnv ME modid {qualid=mkLocalName modid,
							     info = normRecStr S},
					 FE)
				  | FUNmod F => 
					(ME,
					 bindInEnv FE modid {qualid=mkLocalName modid,
							     info = F})
	      val _ = incrBindingLevel();
	      val _ = incrBindingLevel();
	      val _ = refreshTyNameSet PARAMETERts T;
	      val LAMBDAsig(T',M') = elabSigExp ME' FE' GE UE VE TE sigexp'
              val _ = decrBindingLevel();
              val (T'',T'toT'') = parameteriseTyNameSet T' T
	  in  decrBindingLevel();
	      (LAMBDAsig(T'',FUNmod (T,M,(EXISTSexmod([],copyMod T'toT'' [] M')))))
	  end
   | WHEREsigexp (sigexp, tyvarseq, longtycon,ty) => (* cvr: TODO review *)
      (* Unlike SML, we reject where type constraints that construct inconsistent signatures 
         by equating a specified datatype with an non-equivalent type or datatype. 
	 In SML, an inconsitent signature can never be implemented, but in Mosml it
         can, by using a recursive structure, so we have to rule out inconsitent signatures from
	 the start.
       *)
      (case (elabSigExp ME FE GE UE VE TE  sigexp) of
	    (LAMBDAsig(_,FUNmod _)) =>
		errorMsg loc "Illegal where constraint: the constrained signature specifies a functor but should specify a structure"
	|   (LAMBDAsig(T,STRmod RS)) =>
	      let val S = SofRecStr RS
		  val _ = incrBindingLevel();
		  val _ = refreshTyNameSet PARAMETERts T;
		  val _ = checkDuplIds tyvarseq "Duplicate type parameter"
                  val pars = map (fn tyvar => hd(#id(#qualid tyvar))) tyvarseq
		  val vs = map (fn tv => newExplicitTypeVar tv) pars
		  val us = map TypeOfTypeVar vs
		  val UE' = zip2 pars us
                  val {qualid = qualid,info={idLoc,...}} = longtycon
		  val tycon = hd (#id qualid)
                  val tau = elabTy ME FE GE (UE' @ UE) VE TE ty
                  val infTyFun = TYPEtyfun(vs,tau)
		  val infTyStr = (infTyFun,ConEnv [])
                  val _ = decrBindingLevel();
                  val specTyStr = findLongTyConInStr S idLoc qualid
                  val specTyFun = normTyFun (#1 specTyStr)
                  val tn = (choose (equalsTyFunTyName specTyFun) T)
                           handle Subscript =>
			    (msgIBlock 0;
			     errLocation loc;
			     errPrompt "Illegal where constraint: the type constructor "; printQualId qualid;msgEOL();
                             errPrompt "refers to a transparent type specification";msgEOL();
                             errPrompt "but should refer to an opaque type or datatype specification";msgEOL();
			     msgEBlock();
			     raise Toplevel)
		  exception IllFormed
	      in  			
		 setTnSort (#info tn) (VARIABLEts);
		 ((realizeLongTyCon qualid infTyStr specTyStr)
		  handle MatchError matchReason => 
		      (msgIBlock 0;
		       errLocation loc;
		       errPrompt "Illegal where constraint: the type constructor "; printQualId qualid;msgEOL();
		       errPrompt "cannot be constrained in this way because ...";msgEOL();
		       msgEBlock();
		       errMatchReason "constraint" "signature" matchReason;
		       raise Toplevel));
		 ((case !(#tnConEnv(!(#info(tn)))) of (* check well-formedness *)
		       NONE => () (* nothing to check *)
		     | SOME specConEnv => 
			   (case normType tau of (* eta-equivalent to a type application? *)
				CONt(ts,tyapp) => 
				    let fun equal_args [] [] = ()
					|   equal_args ((VARt w)::ts) (v::vs) = 
					     if w = v 
					     then equal_args ts vs
					     else raise IllFormed
                                        |   equal_args _ _ = raise IllFormed
				    in
					equal_args ts vs;
					case conEnvOfTyApp tyapp of
					    NONE => raise IllFormed
					  | SOME infConEnv => (* equivalent conenvs ? *)
					        let 
   						    val infMod =
							STRmod (NONrec (STRstr
							  (NILenv,NILenv,NILenv,
							   mk1Env tycon (infTyFun,infConEnv),
							   (VEofCE infConEnv))))
						    val specMod = copyMod [(tn,tyapp)] [] 
							(STRmod (NONrec (STRstr 
 							  (NILenv,NILenv,NILenv,
							   mk1Env tycon (specTyFun,specConEnv),
							   (VEofCE specConEnv)))))
						in
						   ((matchMod infMod specMod)
						     handle MatchError matchReason => 
							 (msgIBlock 0;
							  errLocation loc;
							  errPrompt "Illegal where constraint: the datatype constructor ";
							   printQualId qualid;msgEOL();
							  errPrompt "cannot be constrained in this way because ";msgEOL();
							  errPrompt "the constraint's constructor environment \
 							   \does not match the specification's constructor environment";msgEOL();
							  msgEBlock();
							  errMatchReason "constraint" "specification" matchReason;
							  raise Toplevel);
						     (matchMod specMod infMod)
						     handle MatchError matchReason => 
							 (msgIBlock 0;
							  errLocation loc;
							  errPrompt "Illegal where constraint: the datatype constructor ";
							   printQualId qualid;msgEOL();
							  errPrompt "cannot be constrained in this way because ";msgEOL();
							  errPrompt "the specification's constructor environment \
 							   \does not match the constraint's constructor environment";msgEOL();
							  msgEBlock();
							  errMatchReason "specification" "constraint" matchReason;
							  raise Toplevel))
						end
				    end					
			      | _ => raise IllFormed))
		  handle IllFormed =>
		      (msgIBlock 0;
		       errLocation loc;
		       errPrompt "Illegal where constraint: the type constructor ";printQualId qualid;msgEOL();
		       errPrompt "is specified as a datatype"; msgEOL();
		       errPrompt "but its constraint is not a datatype";msgEOL();
		       msgEBlock();
		       raise Toplevel));
		 LAMBDAsig(remove tn T,STRmod RS) 
	      end)

   | RECsigexp ((_,modid),sigexp as (locforward,_),sigexp' as (locbody,_)) =>
          let val LAMBDAsig(T,M) = elabSigExp ME FE GE UE VE TE  sigexp
	      val (ME',RS) = 
		  case M of STRmod RS =>
		      (bindInEnv ME modid {qualid=mkLocalName modid,
					   info = normRecStr RS},
		       RS)
		  | FUNmod F => 
		      errorMsg locforward 
		               "Illegal recursive signature: \
				\the forward specification should specify \
				\a structure but actually specifies a functor"
	      val _ = incrBindingLevel();
	      val _ = refreshTyNameSet PARAMETERts T;
	      val LAMBDAsig(T',M') = elabSigExp ME' FE GE UE VE TE sigexp'
	      val RS' = 
		  case M' of 
		      STRmod RS' => RS'
		    | FUNmod F => 
			  errorMsg locbody 
			           "Illegal recursive signature: \
				    \the body should specify a structure \
				    \but actually specifies a functor"
	      val _ = refreshTyNameSet VARIABLEts T;
	      val _ = refreshTyNameSet PARAMETERts T';
	      val _ = matchMod (STRmod RS') (STRmod RS)
		      handle MatchError matchReason => 
			  (msgIBlock 0;
			   errLocation loc;
			   errPrompt "Illegal recursive signature: \
			              \the body does not match the \
				      \forward specification...";
			   msgEOL();
			   msgEBlock();
			   errMatchReason "body" "forward specification" matchReason;
			   raise Toplevel)
	      val T2T' = map (fn tn as {info = ref {tnSort = 
						    REAts (APPtyfun tyapp),
						    ...},
					...} => (tn,tyapp)
			      | _ => fatalError "elabRecSigExp")
		              T
	  in
            (decrBindingLevel();
	     LAMBDAsig(T',copyMod T2T' [] (STRmod (RECrec(RS,RS'))))) 
	  end)
and elabModDesc (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE : TyEnv) (MODDESCmoddesc (locmodid as (loc,modid), sigexp as (loc',_)) : ModDesc)=
  let val LAMBDAsig(T,M) = elabSigExp ME FE GE UE VE TE sigexp
      val S = case M of 
	           STRmod S => S
		 | FUNmod _ => 
		       errorMsg loc' 
		       "This signature actually specifies a functor \
			\but should specify a structure"
  in LAMBDA(T,(locmodid,{qualid = mkLocalName modid, info = S})) end

and elabModDescList (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE : TyEnv) mds =
  foldL_map 
      (fn (LAMBDA(T',(locmodid,M))) => fn LAMBDA(T,ME) => 
       LAMBDA(T@T', bindOnceInEnv ME locmodid M
	             "the same structure identifier is specified twice\ 
		      \ in a structure specification"))
     (elabModDesc ME FE GE UE VE TE ) (LAMBDA([],NILenv)) mds
and elabFunDesc (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE : TyEnv) 
	      (FUNDESCfundesc (locmodid as (loc,modid), sigexp as (loc',_)) : FunDesc)=
  let val LAMBDAsig(T,M) = elabSigExp ME FE GE UE VE TE sigexp
      val F = case M of 
	            FUNmod F => F
                 |  STRmod _ => 
		       errorMsg loc' 
		       "This signature actually specifies a structure \
			\but should specify a functor"
  in LAMBDA(T,(locmodid,{qualid = mkLocalName modid, info = F})) end

and elabFunDescList (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE:UEnv) (VE:VarEnv) (TE : TyEnv) mds =
  foldL_map 
      (fn (LAMBDA(T',(locfunid,F))) => fn LAMBDA(T,FE) => 
       LAMBDA(T@T', bindOnceInEnv FE locfunid F
	             "the same functor identifier is specified twice\ 
		      \ in a functor specification"))
     (elabFunDesc ME FE GE UE VE TE ) (LAMBDA([],NILenv)) mds
and elabSpec  (ME:ModEnv) (FE:FunEnv) (GE:SigEnv) (UE : UEnv) (VE : VarEnv) (TE : TyEnv) onTop (loc, spec') =
  case spec' of
    VALspec (tyvarseq,vds) => 
       let val _ = checkDuplIds tyvarseq "Duplicate explicit type variable"
	   val pars = map (fn ii => hd(#id(#qualid ii))) tyvarseq
	   val tyvars = scopedTyVars loc UE pars (unguardedValDescList vds)
	   val ()   = incrBindingLevel()
           val tvs = map (fn tv => newExplicitTypeVar tv) tyvars
	   val UE'  = (zip2 tyvars (map TypeOfTypeVar tvs)) @ UE
	   val VE' = 
	      foldL_map (fn(locid, info) => fn acc => 
  		           bindOnceInEnv acc locid info
			     "the same value identifier is specified twice\ 
			      \ in a value specification")
			 (elabValDesc ME FE GE UE' VE TE tvs)
                         NILenv vds
       in decrBindingLevel();
 	  LAMBDA([],STRstr (NILenv, NILenv,NILenv, NILenv, VE'))
       end
 | PRIM_VALspec (tyvarseq,pbs) => 
       let val _ = checkDuplIds tyvarseq "Duplicate explicit type variable"
	   val pars = map (fn ii => hd(#id(#qualid ii))) tyvarseq
	   val tyvars = scopedTyVars loc UE pars (unguardedPrimValBindList pbs)
	   val () = incrBindingLevel()
           val tvs = map (fn tv => newExplicitTypeVar tv) tyvars
	   val UE' = (zip2 tyvars (map TypeOfTypeVar tvs)) @ UE
	   val VE' = 
	      foldL_map (fn(locid, info) => fn acc => 
  		           bindOnceInEnv acc locid info
			     "the same value identifier is specified twice\ 
			      \ in a value specification")
			 (elabPrimValBind ME FE GE UE' VE TE tvs)
                         NILenv pbs
       in decrBindingLevel();
 	  LAMBDA([],STRstr (NILenv, NILenv,NILenv, NILenv, VE'))
       end
  | TYPEDESCspec(equ, tds) =>
      let val LAMBDA(T',TE') = elabPrimTypBindList equ tds
      in
         LAMBDA(T',STRstr(NILenv, NILenv, NILenv, TE',NILenv)) 
      end
  | TYPEspec tbs =>
      let val tbsTE = elabTypBindList ME FE GE UE VE TE tbs 
      in
        setEquality tbsTE;
        LAMBDA([],STRstr(NILenv, NILenv, NILenv, tbsTE, NILenv))
      end
  | DATATYPEspec(dbs, tbs_opt) =>
      let val LAMBDA(T,dbsTE) = initialDatBindTE dbs 
          val _ = incrBindingLevel();
          val _ = refreshTyNameSet PARAMETERts T;
          val tbsTE = elabTypBindList_opt ME FE GE UE VE (plusEnv TE dbsTE) tbs_opt
          (* Here dbsTE will get destructively updated too. *)
          val _ = checkNoRebindingsTyEnv loc (plusEnv dbsTE tbsTE)
	             "the same type constructur is specified twice in this datatype specification"
          val (VE',dbsTE') = elabDatBindList ME FE GE UE VE (plusEnv (plusEnv TE dbsTE) tbsTE) dbs
          val _ = checkNoRebindingsVarEnv loc VE'
	             "the same constructor is specified twice in this datatype declaration"
      in
        maximizeEquality dbsTE';
        setEquality tbsTE;
        decrBindingLevel();
        LAMBDA(T,STRstr(NILenv,  NILenv, NILenv, plusEnv dbsTE' tbsTE,VE'))
      end
  | DATATYPErepspec rep => 
    let val (VE,TE) = elabDatatypeReplication ME FE GE UE VE TE (loc,rep)
    in
       LAMBDA([],STRstr(NILenv, NILenv, NILenv, TE, VE))
    end    
  | EXCEPTIONspec eds =>
      (if U_map unguardedExDesc eds <> [] then
         errorMsg loc "Type variables in an exception description"
       else (); (* cvr: TODO can be relaxed? *)
       LAMBDA([],STRstr(NILenv, NILenv, NILenv, NILenv,elabExDescList ME FE GE [] VE TE onTop eds)))
  | STRUCTUREspec mds =>
      let val LAMBDA(T,ME') = elabModDescList ME FE GE UE VE TE mds
      in LAMBDA(T,STRstr (ME', NILenv, NILenv, NILenv, NILenv)) end     
  | FUNCTORspec mds =>
      let val LAMBDA(T,FE') = elabFunDescList ME FE GE UE VE TE mds
      in LAMBDA(T,STRstr (NILenv, FE', NILenv, NILenv, NILenv)) end       
  | LOCALspec (spec1, spec2) =>
      let val (ME',FE',GE',VE', TE') = elabLocalSpec ME FE GE UE VE TE spec1
      in
          elabSpec (plusEnv ME ME') (plusEnv FE FE') (plusEnv GE GE') UE (plusEnv VE VE') (plusEnv TE TE') onTop spec2
      end
  | EMPTYspec => LAMBDA([],STRstr(NILenv, NILenv, NILenv, NILenv, NILenv))
  | INCLUDEspec sigexp =>
      let val LAMBDAsig(T,M) = elabSigExp ME FE GE UE VE TE sigexp
      in  case M of  (* cvr: TODO revise to deal properly with onTop since this may kill static exception status *)
	  FUNmod _ => 
	      errorMsg loc "Illegal include: the included \
                            \signature must specify a structure, not a functor"
        | STRmod (NONrec S) => LAMBDA(T,S)
	| _ => errorMsg loc "Illegal include: the included \
                             \signature may not be recursive"
      end
  | SHARINGTYPEspec (spec,longtyconlist) => 
      let val LAMBDA(T,S) = elabSpec ME FE GE UE VE TE onTop spec 
          val _ = incrBindingLevel();
          val _ = refreshTyNameSet PARAMETERts T;
          val LocTyFunOfLongTyCon = 
	      let val S' = STRstr(MEofStr S,FEofStr S,GEofStr S,TEofStr S,VEofStr S) 
		  (* this step flattens S' once, instead of once for each find *)
	      in
	          fn longtycon as {qualid,info = {idLoc,...},...} =>
		     (idLoc,#1 (findLongTyConInStr S' idLoc qualid))
	      end
          val LocTyFuns = map LocTyFunOfLongTyCon longtyconlist
          fun orderAsT LocT = foldR (fn tn => fn acc =>
				      ((choose (fn (loc,tn') => isEqTN tn tn') LocT)::acc)
				       handle Subscript => acc)
	                       [] T
	  val LocT' (* as ((loc,tn)::LocT'') *) =
	      orderAsT 
                 (map (fn (loc,tyfun) =>
			   ((loc,choose (equalsTyFunTyName tyfun) T)
			     handle Subscript => 
			       errorMsg loc "Illegal sharing spec: \
				\this type constructor does not denote \
                                \an opaque type of the specification"))
			 LocTyFuns)
          val ((loc,tn),LocT'') = case LocT' of 
		                     (loctn::LocT'') => (loctn,LocT'')
				  | _ => fatalError "elabSpec"
          val kind = kindTyName tn
          val _ = app (fn (loc,tn'') =>
		       if kindTyName tn'' = kind
			   then ()
		       else errorMsg loc "Illegal sharing spec: \
		            \the arity of this type constructor differs from the preceding ones")
		       (LocT'')
	  val TminusT'' = foldR (fn (loc,tn) => fn TminusT'' =>
 		                  remove tn TminusT'')
		                 T LocT''
          val equ = foldR (fn (_,tn) => fn equ => 
		       if (#tnEqu (!(#info tn))) <> FALSEequ 
			   then TRUEequ                  (* cvr: TODO should we worry about REFequ? *)
		       else equ)
		       FALSEequ LocT'
          val _ = setTnEqu (#info tn) equ
          val _ = app (fn (_,tn'') => setTnSort (#info tn'') 
		        (REAts (APPtyfun(NAMEtyapp tn))))
		      LocT''
      in
          decrBindingLevel();
	  LAMBDA(TminusT'', S)
      end
  | SEQspec (spec1, spec2) =>
      let val LAMBDA(T',S)  = elabSpec ME FE GE UE VE TE onTop spec1
          val _ = incrBindingLevel();
          val _ = refreshTyNameSet PARAMETERts T';
          val LAMBDA(T'',S') =
            elabSpec (plusEnv ME (MEofStr S)) 
	             (plusEnv FE (FEofStr S)) 
		     (plusEnv GE (GEofStr S)) 
		     UE 
		     (plusEnv VE (VEofStr S)) 
		     (plusEnv TE (TEofStr S)) 
		     onTop 
		     spec2
      in  decrBindingLevel();
	  LAMBDA(T'@T'',SEQstr(S,S')) 
      end
  | SHARINGspec (spec1, (loc',longmodids)) =>
	  let val LAMBDA(T, S) = elabSpec ME FE GE UE VE TE onTop spec1
	      val _ = incrBindingLevel();
	      val _ = refreshTyNameSet PARAMETERts T;
	      val Ss =
		     foldR
		     (fn longmodid as {qualid,info={idLoc,...}} =>
		      (fn Ss =>
		       let val (_,{info = S_i,qualid}) = findLongModIdInStr S idLoc qualid  
		       in
			   S_i::Ss
		       end))
		        [] longmodids
	  in let val T' = share (loc',T,Ss)
	     in
		 decrBindingLevel();
		 LAMBDA(T', S)
	     end
          end
  | FIXITYspec _ => 
	  LAMBDA([],STRstr (NILenv,NILenv,NILenv,NILenv, NILenv))
  | SIGNATUREspec sigbinds => 
      let val GE' = elabSigBindList ME FE GE UE VE TE  sigbinds
      in    LAMBDA([],STRstr(NILenv,NILenv,GE',NILenv, NILenv))
      end
  | _ => errorMsg loc "Illegal specification: this form of specification can only \
                       \ appear within a local specification"
and elabLocalSpec ME FE GE UE VE TE (loc,spec) = 
  case spec of
    EMPTYspec => (NILenv,NILenv,NILenv,NILenv, NILenv)
  | SEQspec (spec1, spec2) =>
      let val (ME',FE',GE',VE',TE') =
	        elabLocalSpec ME FE GE UE VE TE spec1
          val (ME'',FE'',GE'',VE'',TE'') =
                elabLocalSpec (plusEnv ME ME') (plusEnv FE FE') 
                              (plusEnv GE GE') UE 
                              (plusEnv VE VE') (plusEnv TE TE') spec2
      in (plusEnv ME' ME'', plusEnv FE' FE'', plusEnv GE' GE'',
          plusEnv VE' VE'', plusEnv TE' TE'') end
  | OPENspec longmodidinfos => 
	  foldL (fn (longmodid,envoptref) => fn (ME',FE',GE',VE',TE') =>
		 let val {qualid,info} = longmodid
		     val {idKind, idFields,... } = info
		     val (fields,{qualid = csqualid, 
				  info = Env as (ME'',FE'',GE'',VE'',TE'')}) = 
  			 findLongModIdForOpen ME loc qualid
		 in  
		     idKind := { qualid=csqualid, info=VARik };
		     idFields := fields;
                     (* this should be unnecessary 
		         envoptref := SOME Env; *) 
		     (plusEnv ME' ME'',
		      plusEnv FE' FE'',
		      plusEnv GE' GE'',
		      plusEnv VE' VE'',
		      plusEnv TE' TE'')
		 end)
	  (NILenv,NILenv,NILenv,NILenv,NILenv)
          longmodidinfos
  | TYPEspec tbs =>
      let val tbsTE = elabTypBindList ME FE GE UE VE TE tbs in
        setEquality tbsTE;
            (NILenv,NILenv,NILenv,NILenv, tbsTE)
      end
  | LOCALspec (spec1, spec2) =>
      let val (ME',FE',GE',VE', TE') = elabLocalSpec ME FE GE UE VE TE spec1
      in
         elabLocalSpec (plusEnv ME ME') (plusEnv FE FE') (plusEnv GE GE') UE (plusEnv VE VE') (plusEnv TE TE') spec2
      end
  | _ => errorMsg loc "Illegal local specification: this form of specification cannot \
                       \ appear as a local specification"
;


fun elabToplevelDec (dec : Dec) =
(
  if unguardedDec dec <> [] then
    errorMsg (xLR dec) "Unguarded type variables at the top-level"
  else ();
  resetBindingLevel();
  let val EXISTS(T',(ME',FE',GE',VE',TE')) =
      elabDec (mkGlobalME()) (mkGlobalFE()) (mkGlobalGE()) [] 
              (mkGlobalVE()) (mkGlobalTE()) (* ps: true *) false dec  
      val _ = if (!currentCompliance) <> Liberal 
		   then Synchk.compliantTopDec dec 
	      else ()
  in EXISTS(T',(cleanEnv ME', 
		 cleanEnv FE', 
		 cleanEnv GE', 
		 cleanEnv VE', 
		 cleanEnv TE'))
  end
);

fun elabStrDec (dec : Dec) =
(
  if unguardedDec dec <> [] then
    errorMsg (xLR dec) "Unguarded type variables at the top-level"
  else ();
  resetBindingLevel();
  let val EXISTS(T',(ME',FE',GE',VE',TE')) =
     elabDec (mkGlobalME()) (mkGlobalFE()) (mkGlobalGE()) [] 
             (mkGlobalVE()) (mkGlobalTE()) (* ps: true *) false dec  
      val _ = if (!currentCompliance) <> Liberal 
		   then Synchk.compliantStrDec dec 
	      else ()
  in 
      EXISTS(T',(cleanEnv ME', 
		 cleanEnv FE', 
		 cleanEnv GE', 
		 cleanEnv VE', 
		 cleanEnv TE'))
  end
);

fun elabToplevelSigExp (sigexp as (loc,_) : SigExp) =
    (resetBindingLevel();
     let val LAMBDAsig(T,M) = 
	  elabSigExp (mkGlobalME()) 
	             (mkGlobalFE()) 
		     (mkGlobalGE()) 
		     [] 
		     (mkGlobalVE()) 
		     (mkGlobalTE()) 
		     sigexp
      in  case M of  
	  FUNmod _ => 
	      errorMsg loc "Illegal unit signature: the signature \
                            \must specify a structure, not a functor"
        | STRmod RS => 
              (if (!currentCompliance) <> Liberal 
		   then Synchk.compliantSigExp sigexp 
	           else (); 
	       LAMBDA(T,RS))
      end);

fun elabToplevelSpec (spec : Spec) =
    (resetBindingLevel();
     let val StrSig = 
	 elabSpec (mkGlobalME()) (mkGlobalFE()) 
	          (mkGlobalGE()) [] 
		  (mkGlobalVE()) (mkGlobalTE()) 
                   (* ps: true *) false spec
     in  
	 (*  we could, but don't, check compliance since toplevel-mode .sig files don't need to be ported 
	  if (!currentCompliance) <> Liberal 
	       then Synchk.compliantTopSpec spec
	 else (); *) 
	 StrSig
     end )
;

fun elabSigSpec (spec : Spec) =
    (resetBindingLevel();
     let val StrSig = 
	 elabSpec (mkGlobalME()) (mkGlobalFE()) 
	          (mkGlobalGE()) [] 
		  (mkGlobalVE()) (mkGlobalTE()) 
                   (* ps: true *) false spec
     in  
	 if (!currentCompliance) <> Liberal 
	     then Synchk.compliantSpec spec
	 else ();
	 StrSig
     end )
;



(* tie the knot *)

(* cvr: TODO remove in favour of mutual recursion *)

val () = elabSigExpRef := elabSigExp;
val () = elabModExpRef := elabModExp;








