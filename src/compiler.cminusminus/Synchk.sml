open List Fnlib Mixture Const Globals Location Units Asynt Asyntfn

fun complianceMsg loc msg =
    case (!currentCompliance) of
	Orthodox => 
	    (msgIBlock 0;
	     errLocation loc;
	     errPrompt "Compliance Error: ";msgEOL();
	     errPrompt "The phrase is an instance of the Moscow ML extension:";msgEOL();
	     errPrompt "  ";msgString msg; msgEOL();
	     errPrompt "which is not supported by the Definition of Standard ML."; msgEOL();
	     msgEBlock();
	     raise Toplevel)
      |  Conservative => 
	    (msgIBlock 0;
	     errLocation loc;
	     errPrompt "Compliance Warning: ";msgEOL();
	     errPrompt "The phrase is an instance of the Moscow ML extension:";msgEOL();
	     errPrompt "  ";msgString msg; msgEOL();
	     errPrompt "which is not supported by the Definition of Standard ML."; msgEOL();
	     msgEBlock())
      |  Liberal =>()
;

fun atmodexps args (loc,(APPmodexp(func,arg),_)) = 
	        atmodexps (arg::args) func
	    |   atmodexps args head = (head,args)

fun compliantExp (loc, exp') =
  case exp' of
    SCONexp _ => ()
  | VIDPATHexp _ => ()
  | RECexp(ref (RECre fields)) =>
      app (fn(_, e) => compliantExp e) fields
  | RECexp(ref (TUPLEre es)) => 
      app compliantExp es
  | VECexp es =>
      app compliantExp es
  | LETexp(dec, exp) =>
      (compliantDec dec;compliantExp exp)
  | PARexp exp => compliantExp exp
  | APPexp(exp1, exp2) =>
      (compliantExp exp1;compliantExp exp2)
  | INFIXexp (ref (UNRESinfixexp es)) => 
      app compliantExp es
  | INFIXexp (ref (RESinfixexp e)) => compliantExp e
  | TYPEDexp(exp, ty) =>
      (compliantExp exp;compliantTy ty)
  | ANDALSOexp(exp1, exp2) =>
      (compliantExp exp1;compliantExp exp2)
  | ORELSEexp(exp1, exp2) =>
      (compliantExp exp1;compliantExp exp2)
  | HANDLEexp(exp, mrules) =>
      (compliantExp exp;app compliantMRule mrules)
  | RAISEexp exp =>
      compliantExp exp
  | IFexp(e0, e1, e2) =>
      (compliantExp e0;compliantExp e1;compliantExp e2)
  | FNexp mrules =>
      app compliantMRule mrules
  | WHILEexp(exp1, exp2) =>
      (compliantExp exp1;compliantExp exp2)
  | SEQexp(exp1, exp2) =>
      (compliantExp exp1;compliantExp exp2)
  | STRUCTUREexp(modexp,sigexp,_) => 
      (complianceMsg loc "<atexp> ::= [structure <modexp> as <sigexp>]"; 
       compliantModExp modexp;
       compliantSigExp sigexp)
  | FUNCTORexp(modexp,sigexp,_) => 
      (complianceMsg loc "<atexp> ::= [functor <modexp> as <sigexp>]"; 
       compliantModExp modexp;
       compliantSigExp sigexp)
and compliantMRule (MRule(ref pats, exp)) =
    (app compliantPat pats;compliantExp exp)
and compliantPat (_, pat') =
  case pat' of
    SCONpat _ => ()
  | VARpat _ => ()
  | WILDCARDpat => ()
  | NILpat _ => ()
  | CONSpat(_, p) => compliantPat p
  | EXNILpat _ => ()
  | EXCONSpat(_,p) => compliantPat p
  | EXNAMEpat _ => fatalError "compliantPat"
(* cvr: TODO review *)
  | REFpat p => compliantPat p
  | RECpat(ref (RECrp(fs, _))) =>
      app (fn(_, p) => compliantPat p) fs
  | RECpat(ref (TUPLErp _)) => fatalError "compliantPat"
(* cvr: TODO review *)
  | VECpat ps =>
      app compliantPat ps
  | INFIXpat (ref (RESinfixpat p))  => compliantPat p
  | INFIXpat (ref (UNRESinfixpat _)) => fatalError "compliantPat"
(* cvr: TODO review *)
  | PARpat pat => compliantPat pat
  | TYPEDpat(pat, ty) =>
      (compliantPat pat;compliantTy ty)
  | LAYEREDpat(pat1, pat2) =>
      (compliantPat pat1;compliantPat pat2)
and compliantDec (loc,dec') =
  case dec' of
    VALdec (tyvarseq,(valbind,valbind')) => 
	(app compliantValBind valbind;
	 app compliantValBind valbind')
  | PRIM_VALdec _ => ()
  | FUNdec (ref (UNRESfundec (tyvarseq, fvbds))) => fatalError "compliantDec"
(* cvr: TODO review *)
  | FUNdec (ref (RESfundec dec)) => compliantDec dec
  | TYPEdec tbds => 
	app compliantTypBind tbds
  | PRIM_TYPEdec _ => ()
  | DATATYPEdec (dbds,SOME tbds) => 
	((app compliantDatBind dbds) ;
	 (app compliantTypBind tbds))
  | DATATYPEdec (dbds,NONE) => 
 	 app compliantDatBind dbds
  | DATATYPErepdec (_,tyconpath) =>       
	 compliantTyConPath tyconpath
  | ABSTYPEdec(dbds,SOME tbds,dec) =>
      ((app compliantDatBind dbds);
       (app compliantTypBind tbds);
       compliantDec dec)
  | ABSTYPEdec(dbds,NONE,dec) =>
      ((app compliantDatBind dbds);
       compliantDec dec)
  | EXCEPTIONdec ebs =>
      app compliantExBind ebs
  | LOCALdec (dec1, dec2) =>
      (compliantDec dec1;compliantDec dec2)
  | OPENdec longmodidinfos => 
        compliantLongModIdInfoList longmodidinfos
  | EMPTYdec => ()
  | SEQdec (dec1, dec2) =>
      (compliantDec dec1;compliantDec dec2)
  | FIXITYdec _ => ()
  | STRUCTUREdec mbds => 
      (complianceMsg loc "<dec> ::= structure <strbind>"; 
       app compliantModBind mbds)
  | FUNCTORdec fbds => 
      (complianceMsg loc "<dec> ::= functor <funbind>"; 
       app compliantFunBind fbds)
  | SIGNATUREdec sbds => 
      (complianceMsg loc "<dec> ::= signature <sigbind>"; 
       app compliantSigBind sbds)
and compliantStrDec (loc,dec') =
  case dec' of
    VALdec (tyvarseq,(valbind,valbind')) => 
	(app compliantValBind valbind;
	 app compliantValBind valbind')
  | PRIM_VALdec _ => ()
  | FUNdec (ref (UNRESfundec (tyvarseq, fvbds))) => fatalError "compliantStrDec"
(* cvr: TODO review *)
  | FUNdec (ref (RESfundec dec)) => compliantStrDec dec
  | TYPEdec tbds => 
	app compliantTypBind tbds
  | PRIM_TYPEdec _ => ()
  | DATATYPEdec (dbds,SOME tbds) => 
	((app compliantDatBind dbds) ;
	 (app compliantTypBind tbds))
  | DATATYPEdec (dbds,NONE) => 
 	 app compliantDatBind dbds
  | DATATYPErepdec (_,tyconpath) =>       
	 compliantTyConPath tyconpath
  | ABSTYPEdec(dbds,SOME tbds,dec) =>
      ((app compliantDatBind dbds);
       (app compliantTypBind tbds);
       compliantDec dec)
  | ABSTYPEdec(dbds,NONE,dec) =>
      ((app compliantDatBind dbds);
       compliantDec dec)
  | EXCEPTIONdec ebs =>
      app compliantExBind ebs
  | LOCALdec (dec1, dec2) =>
      (compliantStrDec dec1;compliantStrDec dec2)
  | OPENdec longmodidinfos => 
        compliantLongModIdInfoList longmodidinfos
  | EMPTYdec => ()
  | SEQdec (dec1, dec2) =>
      (compliantStrDec dec1;compliantStrDec dec2)
  | FIXITYdec _ => ()
  | STRUCTUREdec mbds => 
      (app compliantModBind mbds)
  | FUNCTORdec fbds => 
      (complianceMsg loc "<strdec> ::= functor <funbind>"; 
       app compliantFunBind fbds)
  | SIGNATUREdec sbds => 
      (complianceMsg loc "<strdec> ::= signature <sigbind>"; 
       app compliantSigBind sbds)
and compliantTopDec (loc,dec') =
  case dec' of
    VALdec (tyvarseq,(valbind,valbind')) => 
	(app compliantValBind valbind;
	 app compliantValBind valbind')
  | PRIM_VALdec _ => ()
  | FUNdec (ref (UNRESfundec (tyvarseq, fvbds))) => fatalError "compliantTopDec"
(* cvr: TODO review *)
  | FUNdec (ref (RESfundec dec)) => compliantTopDec dec
  | TYPEdec tbds => 
	app compliantTypBind tbds
  | PRIM_TYPEdec _ => ()
  | DATATYPEdec (dbds,SOME tbds) => 
	((app compliantDatBind dbds) ;
	 (app compliantTypBind tbds))
  | DATATYPEdec (dbds,NONE) => 
 	 app compliantDatBind dbds
  | DATATYPErepdec (_,tyconpath) =>       
	 compliantTyConPath tyconpath
  | ABSTYPEdec(dbds,SOME tbds,dec) =>
      ((app compliantDatBind dbds);
       (app compliantTypBind tbds);
       compliantDec dec)
  | ABSTYPEdec(dbds,NONE,dec) =>
      ((app compliantDatBind dbds);
       compliantDec dec)
  | EXCEPTIONdec ebs =>
      app compliantExBind ebs
  | LOCALdec (dec1, dec2) =>
      (compliantStrDec dec1;compliantStrDec dec2)
  | OPENdec longmodidinfos => 
        compliantLongModIdInfoList longmodidinfos
  | EMPTYdec => ()
  | SEQdec (dec1, dec2) =>
      (compliantTopDec dec1;compliantTopDec dec2)
  | FIXITYdec _ => ()
  | STRUCTUREdec mbds => 
      (app compliantModBind mbds)
  | FUNCTORdec fbds => 
      (app compliantFunBind fbds)
  | SIGNATUREdec sbds => 
      (app compliantSigBind sbds)
and compliantExBind (EXDECexbind(_, SOME ty)) = compliantTy ty
  | compliantExBind (EXDECexbind(_, NONE)) = ()
  | compliantExBind (EXEQUALexbind(_,_)) = ()
and compliantValBind (ValBind(ref pat, exp)) =
  (compliantPat pat;compliantExp exp)
and compliantPrimValBindList (pbs) =
  (app (fn (ii,ty,arity,n) => compliantTy ty) pbs)
and compliantValDec (pvbs, rvbs) =
  ((app compliantValBind pvbs) ;
   (app compliantValBind rvbs))
and compliantTy (loc, ty') =
  case ty' of
    TYVARty ii => ()
  | RECty fs =>
      app (fn(_, ty) => compliantTy ty) fs
  | CONty(tys, tyconpath) => 
      ((app compliantTy tys);compliantTyConPath tyconpath)
  | FNty(ty1, ty2) =>
      (compliantTy ty1;compliantTy ty2)
  | PACKty(sigexp) =>
      (complianceMsg loc "<ty> ::= [<sigexp>]"; 
       compliantSigExp sigexp)
  | PARty(ty) =>
       compliantTy ty
and compliantModBind (MODBINDmodbind(modid,modexp)) =
      compliantModExp modexp
  | compliantModBind (ASmodbind(modid,sigexp,exp)) =
      (complianceMsg (xxLR modid exp) "<strbind> ::= <strid> as <sigexp> = <exp>"; 
       compliantSigExp sigexp;
       compliantExp exp)
and compliantSigBind (SIGBINDsigbind(sigid,sigexp)) =
      compliantSigExp sigexp
and compliantFunBind (FUNBINDfunbind(funid,
				     modexp as (loc,(FUNCTORmodexp _,_)))) =
     compliantModExp modexp
  | compliantFunBind (FUNBINDfunbind(funid,modexp)) =
     (complianceMsg (xxLR funid modexp) "<funbind> ::= <funid> = <modexp>"; 
      compliantModExp modexp)
  | compliantFunBind (ASfunbind(funid,sigexp,exp)) =
     (complianceMsg (xxLR funid exp) "<funbind> ::= <funid> as <sigexp> = <exp>"; 
       compliantSigExp sigexp;
       compliantExp exp)
and compliantLongModIdInfoList longmodidinfos =
    app (fn ({info = {withOp,idLoc,...},...},_) =>
	 if withOp then	
	     complianceMsg idLoc 
	      "<longstrid> ::= op <strid_1>. ... .<strid_n>.<strid>"	
	 else ())
        longmodidinfos
and compliantModExp (loc,(modexp,_)) = 
   case modexp of
     DECmodexp dec => 
	 compliantStrDec dec
   | LONGmodexp {info = {idKind = ref {info = FUNik,...},
			 withOp = true,
			 ...},
		 ...} =>
	 complianceMsg loc "<longfunid> ::= op <strid_1>. ... .<strid_n>.<funid>"	
   | LONGmodexp {info = {idKind = ref {info = STRik,...},
			 withOp = true,
			 ...},
		 ...} =>
	 complianceMsg loc "<longstrid> ::= op <strid_1>. ... .<strid_n>.<strid>"	
   | LONGmodexp {info = {idKind = ref {info = FUNik,...},
			 withOp = false,
			 ...},
		 qualid = {id = (funid::strid::_),...}} =>
	 complianceMsg loc "<longfunid> ::= <strid_1>. ... .<strid_n>.<funid>"	  | LONGmodexp _ => ()
   | LETmodexp (dec,modexp) =>
	  (compliantStrDec dec;compliantModExp modexp)
   | PARmodexp modexp => 
	  (complianceMsg loc 
	      "<atmodexp> ::= ( <modexp> )";  
           compliantModExp modexp)
   | CONmodexp (modexp,sigexp) =>
	  (compliantModExp modexp;compliantSigExp sigexp)
   | ABSmodexp (modexp,sigexp) =>
	  (compliantModExp modexp;compliantSigExp sigexp)
   | FUNCTORmodexp (Generative isDerived,modid,_, sigexp, modexp) =>
	  (if isDerived then ()
	   else complianceMsg loc 
	      "<modexp> ::= functor (<modid> : <sigexp>) => <modexp>"; 
	   compliantSigExp sigexp;compliantModExp modexp)
   | FUNCTORmodexp (Applicative,modid,_, sigexp, modexp) =>
	  (complianceMsg loc 
	      "<modexp> ::= functor <modid> : <sigexp> => <modexp>"; 
	   compliantSigExp sigexp;compliantModExp modexp)
   | APPmodexp (func,arg) =>
	(case (atmodexps [arg] func) of
	     (head as (_,(LONGmodexp _,_)),
	      [arg as (_,(PARmodexp modexp,_))]) =>
	         (compliantModExp head;
		  compliantModExp modexp)
	    | (head,args) =>
		 (complianceMsg loc 
		    "<modexp> ::= <atmodexp_1> ... <atmodexp_n>"; 
		  compliantModExp head;
		  app compliantModExp args))
	     
   | RECmodexp (modid,_,sigexp, modexp) =>
	  (complianceMsg loc 
	      "<modexp> ::= rec (<strid> : <sigexp>) <modexp>"; 
	   compliantSigExp sigexp;compliantModExp modexp)
and compliantSigExp (loc,sigexp) =
  case sigexp of
    SPECsigexp spec => compliantSpec spec
  | SIGIDsigexp _ => ()
  | WHEREsigexp (sigexp, tyvarseq, longtycon, ty) =>
           (compliantSigExp sigexp;compliantTy ty)
  | FUNSIGsigexp (Generative _,modid,sigexp,sigexp') =>
	   (complianceMsg loc 
	      "<sigexp> ::= functor (<modid> : <sigexp>) -> <sigexp>"; 
            compliantSigExp sigexp;
	    compliantSigExp sigexp')
  | FUNSIGsigexp (Applicative,modid,sigexp,sigexp') =>
	   (complianceMsg loc 
	      "<sigexp> ::= functor <modid> : <sigexp> -> <sigexp>"; 
            compliantSigExp sigexp;
	    compliantSigExp sigexp')
  | RECsigexp (modid, sigexp,sigexp') =>
           (complianceMsg loc 
	      "<sigexp> ::= rec (<strid> : <sigexp>) <sigexp>"; 
	    compliantSigExp sigexp;
	    compliantSigExp sigexp')
and compliantSpec (loc, spec') = 
  case spec' of
    VALspec ([],vds) => 
       compliantValDescList vds
  | VALspec ((tyvar::_), vds)=> 
      (complianceMsg loc "<spec> ::= val <tyvarseq> <valdesc>"; 
       compliantValDescList vds)
  | PRIM_VALspec _ => ()
  | TYPEDESCspec _ => ()
  | TYPEspec tbds => app compliantTypBind tbds
  | DATATYPEspec (dbds,SOME tbds) => 
       (complianceMsg loc "<spec> ::= datatype <datdesc> withtype <typbind>"; 
	(app compliantDatBind dbds);
        (app compliantTypBind tbds))
  | DATATYPEspec (dbds,NONE) => 
       app compliantDatBind dbds
  | DATATYPErepspec (_,tyconpath) =>       
       compliantTyConPath tyconpath
  | EXCEPTIONspec eds => app compliantExDesc eds
  | LOCALspec(spec1, spec2) =>
       (complianceMsg loc "<spec> ::= local <spec> in <spec> end"; 
        compliantSpec spec1;compliantSpec spec2)
  | OPENspec longmodidinfos => 
       (complianceMsg loc "<spec> ::= open <longstrid_1> ... <longstrid_n>";
        compliantLongModIdInfoList longmodidinfos)
  | EMPTYspec => ()
  | SEQspec(spec1, spec2) =>
       (compliantSpec spec1;compliantSpec spec2)
  | INCLUDEspec sigexp => 
       compliantSigExp sigexp
  | STRUCTUREspec moddescs => 
       app compliantModDesc moddescs
  | FUNCTORspec fundescs => 
       (complianceMsg loc "<spec> ::= functor <fundesc>"; 
	app compliantFunDesc fundescs)
  | SHARINGTYPEspec (spec, longtycons) => 
       compliantSpec spec
  | SHARINGspec (spec, longmodids) => 
       compliantSpec spec
  | FIXITYspec (NONFIXst,_) => 
       complianceMsg loc "<spec> ::= nonfix <vid_1> ... <vid_n>"
  | FIXITYspec (INFIXst _,_) => 
       complianceMsg loc "<spec> ::= infix <d> <vid_1> ... <vid_n>"
  | FIXITYspec (INFIXRst _,_) => 
       complianceMsg loc "<spec> ::= infixr <d> <vid_1> ... <vid_n>"
  | SIGNATUREspec sigdescs =>
      (complianceMsg loc "<spec> ::= signature <sigbind>"; 
       app compliantSigBind sigdescs)
and compliantTopSpec spec = compliantSpec spec
and compliantModDesc (MODDESCmoddesc(modid,sigexp)) =
    compliantSigExp sigexp
and compliantFunDesc (FUNDESCfundesc(funid,sigexp)) =
    compliantSigExp sigexp
and compliantTyConPath (_,LONGtyconpath _) = ()
  | compliantTyConPath (loc,WHEREtyconpath (_,_,modexp)) = 
    (complianceMsg loc "<tyconpath> ::= <longtycon> where <strid> = <modexp>"; 
     compliantModExp modexp)
and compliantTypBind (tyvarseq,tycon,ty) =
       compliantTy ty
and compliantExDesc (_,SOME ty) =
       compliantTy ty
  | compliantExDesc (_,NONE) = ()
and compliantDatBind (tyvarseq, tycon, cbds) =
    app compliantConBind cbds
and compliantConBind (ConBind (ii, NONE)) = ()
  | compliantConBind (ConBind (ii, SOME ty)) = compliantTy ty
and compliantValDescList (vds) =
    (app (fn (ii,ty) => compliantTy ty) vds)
;






