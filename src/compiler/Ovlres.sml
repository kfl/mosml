open List;
open Fnlib Mixture Const Prim Smlprim Globals Location;
open Units Types Asynt;

fun errorOverloadingType loc id tau =
(
  msgIBlock 0;
  errLocation loc;
  errPrompt "Overloaded "; msgString id;
  msgString " cannot be applied to argument(s) of type ";
  printType tau; msgEOL();
  msgEBlock();
  raise Toplevel
);

fun errorOverloadingScon loc msg tau =
(
  msgIBlock 0;
  errLocation loc;
  errPrompt ("Overloaded " ^ msg ^ " constant cannot have type ");
  printType tau; msgEOL();
  msgEBlock();
  raise Toplevel
);

fun errorConstTooLarge loc msg =
(
  msgIBlock 0;
  errLocation loc;
  errPrompt msg; msgString " constant is too large"; msgEOL();
  msgEBlock();
  raise Toplevel
);

val negInt = mkPrimInfo 1 (MLPprim(1, Psmlnegint))
and absInt = mkPrimInfo 1 (MLPccall(1, "sml_abs_int"))
and makestringInt = mkPrimInfo 1 (MLPccall(1, "sml_string_of_int"))
and addInt = mkPrimInfo 1 MLPadd_int
and subInt = mkPrimInfo 1 MLPsub_int
and mulInt = mkPrimInfo 1 MLPmul_int
and divInt = mkPrimInfo 1 MLPdiv_int
and modInt = mkPrimInfo 1 MLPmod_int
and ltInt = mkPrimInfo 1 MLPlt_int
and gtInt = mkPrimInfo 1 MLPgt_int
and leInt = mkPrimInfo 1 MLPle_int
and geInt = mkPrimInfo 1 MLPge_int
;

fun resolveIntOvlId loc "~"    OVL1NNo       = negInt
  | resolveIntOvlId loc "abs"  OVL1NNo       = absInt
  | resolveIntOvlId loc "makestring" OVL1NSo = makestringInt
  | resolveIntOvlId loc "+"    OVL2NNNo      = addInt
  | resolveIntOvlId loc "-"    OVL2NNNo      = subInt
  | resolveIntOvlId loc "*"    OVL2NNNo      = mulInt
  | resolveIntOvlId loc "div"  OVL2NNNo      = divInt
  | resolveIntOvlId loc "mod"  OVL2NNNo      = modInt
  | resolveIntOvlId loc "<"    OVL2NNBo      = ltInt
  | resolveIntOvlId loc ">"    OVL2NNBo      = gtInt
  | resolveIntOvlId loc "<="   OVL2NNBo      = leInt
  | resolveIntOvlId loc ">="   OVL2NNBo      = geInt
  | resolveIntOvlId _   _      _             = fatalError "resolveIntOvlId"
;

val addWord = mkPrimInfo 1 MLPadd_word
and subWord = mkPrimInfo 1 MLPsub_word
and mulWord = mkPrimInfo 1 MLPmul_word
and divWord = mkPrimInfo 1 MLPdiv_word
and modWord = mkPrimInfo 1 MLPmod_word
and ltWord = mkPrimInfo 1 MLPlt_word
and gtWord = mkPrimInfo 1 MLPgt_word
and leWord = mkPrimInfo 1 MLPle_word
and geWord = mkPrimInfo 1 MLPge_word
;

val makestringWord = mkPrimInfo 1 (MLPccall(1, "sml_hexstring_of_word"));

fun resolveWordOvlId loc "+"    OVL2NNNo = addWord
  | resolveWordOvlId loc "-"    OVL2NNNo = subWord
  | resolveWordOvlId loc "*"    OVL2NNNo = mulWord
  | resolveWordOvlId loc "div"  OVL2NNNo = divWord
  | resolveWordOvlId loc "mod"  OVL2NNNo = modWord
  | resolveWordOvlId loc "<"    OVL2NNBo = ltWord
  | resolveWordOvlId loc ">"    OVL2NNBo = gtWord
  | resolveWordOvlId loc "<="   OVL2NNBo = leWord
  | resolveWordOvlId loc ">="   OVL2NNBo = geWord
  | resolveWordOvlId loc "makestring" OVL1NSo = makestringWord
  | resolveWordOvlId loc id     _        = 
      errorOverloadingType loc id type_word;

(* Temporary implementation of Word8.{+,-,*} operations: *)

val addWord8 = mkPrimInfo 1 (MLPgv {qual="Word8", id=["+"]})
val subWord8 = mkPrimInfo 1 (MLPgv {qual="Word8", id=["-"]})
val mulWord8 = mkPrimInfo 1 (MLPgv {qual="Word8", id=["*"]})

fun resolveWord8OvlId loc "+"    OVL2NNNo = addWord8
  | resolveWord8OvlId loc "-"    OVL2NNNo = subWord8
  | resolveWord8OvlId loc "*"    OVL2NNNo = mulWord8
  | resolveWord8OvlId loc "div"  OVL2NNNo = divWord
  | resolveWord8OvlId loc "mod"  OVL2NNNo = modWord
  | resolveWord8OvlId loc "<"    OVL2NNBo = ltWord
  | resolveWord8OvlId loc ">"    OVL2NNBo = gtWord
  | resolveWord8OvlId loc "<="   OVL2NNBo = leWord
  | resolveWord8OvlId loc ">="   OVL2NNBo = geWord
  | resolveWord8OvlId loc "makestring" OVL1NSo = makestringWord
  | resolveWord8OvlId loc id     _        = 
      errorOverloadingType loc id type_word8;

val makestringChar = mkPrimInfo 1 (MLPccall(1, "sml_makestring_of_char"));

fun resolveCharOvlId loc "makestring" OVL1NSo = makestringChar
  | resolveCharOvlId loc "<"    OVL2NNBo = ltInt
  | resolveCharOvlId loc ">"    OVL2NNBo = gtInt
  | resolveCharOvlId loc "<="   OVL2NNBo = leInt
  | resolveCharOvlId loc ">="   OVL2NNBo = geInt
  | resolveCharOvlId loc id     _ =
      errorOverloadingType loc id type_char
;

val negReal = mkPrimInfo 1 (MLPprim(1, Pfloatprim Psmlnegfloat))
and absReal = mkPrimInfo 1 (MLPccall(1, "sml_abs_real"))
and makestringReal = mkPrimInfo 1 (MLPccall(1, "sml_string_of_float"))
and addReal = mkPrimInfo 1 MLPadd_real
and subReal = mkPrimInfo 1 MLPsub_real
and mulReal = mkPrimInfo 1 MLPmul_real
and ltReal = mkPrimInfo 1 MLPlt_real
and gtReal = mkPrimInfo 1 MLPgt_real
and leReal = mkPrimInfo 1 MLPle_real
and geReal = mkPrimInfo 1 MLPge_real
;

fun resolveRealOvlId loc "~"    OVL1NNo  = negReal
  | resolveRealOvlId loc "abs"  OVL1NNo  = absReal
  | resolveRealOvlId loc "makestring" OVL1NSo = makestringReal
  | resolveRealOvlId loc "+"    OVL2NNNo = addReal
  | resolveRealOvlId loc "-"    OVL2NNNo = subReal
  | resolveRealOvlId loc "*"    OVL2NNNo = mulReal
  | resolveRealOvlId loc "<"    OVL2NNBo = ltReal
  | resolveRealOvlId loc ">"    OVL2NNBo = gtReal
  | resolveRealOvlId loc "<="   OVL2NNBo = leReal
  | resolveRealOvlId loc ">="   OVL2NNBo = geReal
  | resolveRealOvlId loc id _ = 
      errorOverloadingType loc id type_real
;

val makestringString = mkPrimInfo 1 (MLPccall(1, "sml_makestring_of_string"))
and ltString = mkPrimInfo 1 MLPlt_string
and gtString = mkPrimInfo 1 MLPgt_string
and leString = mkPrimInfo 1 MLPle_string
and geString = mkPrimInfo 1 MLPge_string
;

fun resolveStringOvlId loc "makestring" OVL1NSo = makestringString
  | resolveStringOvlId loc "<"    OVL2NNBo = ltString
  | resolveStringOvlId loc ">"    OVL2NNBo = gtString
  | resolveStringOvlId loc "<="   OVL2NNBo = leString
  | resolveStringOvlId loc ">="   OVL2NNBo = geString
  | resolveStringOvlId loc id     _ =
      errorOverloadingType loc id type_string
;

val eqInt = mkPrimInfo 1 MLPeq_int
and noteqInt = mkPrimInfo 1 MLPnoteq_int;

val eqWord = mkPrimInfo 1 MLPeq_word
and noteqWord = mkPrimInfo 1 MLPnoteq_word;

val eqPoly = mkPrimInfo 1 MLPeq
and noteqPoly = mkPrimInfo 1 MLPnoteq;

fun resolveOvlId loc id ovltype tau =
  case (ovltype, id) of
      (OVL1TXXo, "printVal") =>
        let val sc = freshSchemeOfType tau in
          mkPrimInfo 1 (MLPgvt({qual="Meta", id=["printVal"]}, ref (Obj.repr sc)))
        end
    | (OVL1TPUo, "installPP") =>
        let val sc = freshSchemeOfType tau in
          mkPrimInfo 1 (MLPgvt({qual="Meta", id=["installPP"]}, ref (Obj.repr sc)))
        end
    | (OVL2EEBo, "=") =>
        (case normType tau of
            CONt([], NAMEtyapp tyname) =>
              if isEqTN tyname tyname_int orelse isEqTN tyname tyname_char then
		  eqInt
              else if (isEqTN tyname tyname_word 
		       orelse isEqTN tyname tyname_word8) then
		  eqWord
	      else
		  eqPoly
          | _ => 
		eqPoly)
    | (OVL2EEBo, "<>") =>
        (case normType tau of
            CONt([], NAMEtyapp tyname) =>
              if isEqTN tyname tyname_int 
		  orelse isEqTN tyname tyname_char then
		  noteqInt
              else if isEqTN tyname tyname_word 
		  orelse isEqTN tyname tyname_word8 then
		  noteqWord
	      else
		  noteqPoly
          | _ => 
		noteqPoly)
    | (_,_) =>
        (case normType tau of
            CONt([], NAMEtyapp tyname) =>
              if (isEqTN tyname tyname_int) then
                resolveIntOvlId loc id ovltype
              else if (isEqTN tyname tyname_char) then
                resolveCharOvlId loc id ovltype
              else if (isEqTN tyname tyname_real) then
                resolveRealOvlId loc id ovltype
              else if (isEqTN tyname tyname_string) then
                resolveStringOvlId loc id ovltype
              else if (isEqTN tyname tyname_word) then
                resolveWordOvlId loc id ovltype
              else if (isEqTN tyname tyname_word8) then
                resolveWord8OvlId loc id ovltype
              else
	        errorOverloadingType loc id tau
          | VARt _ => 
		  (* OK because "/" is not overloaded on `real' types: *)
		  (unify tau type_int;
		   resolveIntOvlId loc id ovltype)
	  | _ => errorOverloadingType loc id tau);

fun resolveWord8OvlScon loc w = 
    if w > 0w255 then errorConstTooLarge loc "Word8.word"
    else ();

fun resolveOvlScon loc (scon as WORDscon w, ref (SOME tau)) =
    (case normType tau of
	 CONt([], NAMEtyapp tyname) =>
	     if (isEqTN tyname tyname_word) then
		 ()
	     else if (isEqTN tyname tyname_word8) then
		 resolveWord8OvlScon loc w
	     else 
		 errorOverloadingScon loc "word" tau
       | VARt _ => unify tau type_word
       | _      => errorOverloadingScon loc "word" tau)
  | resolveOvlScon loc (WORDscon w, ref NONE) =
	 fatalError "resolveOvlScon"
  | resolveOvlScon _ _ = (); 

fun resolve3Dot (loc: Location) fs rho =
  let val (fields, unresolved) = contentsOfRowType rho
      val () =
        if unresolved then
          errorMsg loc "Unresolved record pattern"
        else ();
      val fs' = map (fn (lab,_) => (lab, (loc, WILDCARDpat))) fields
  in fs @ fs' end
;

fun resolveOvlPat firstpass (loc, pat') =
  case pat' of
    SCONpat sconInfo => resolveOvlScon loc sconInfo
  | VARpat _ => ()
  | WILDCARDpat => ()
  | NILpat _ => ()
  | CONSpat(_, p) => resolveOvlPat firstpass p
  | EXNILpat _ => ()
  | EXCONSpat(_, p) => resolveOvlPat firstpass p
  | EXNAMEpat _ => fatalError "resolveOvlPat:1"
  | REFpat p => resolveOvlPat firstpass p
  | RECpat rp =>
      (case !rp of
           RECrp(fs, NONE) =>
             (app_field (resolveOvlPat firstpass) fs;
	      if firstpass 
	      then rp := TUPLErp(map snd (sortRow fs))
	      else ())
         | RECrp(fs, SOME rho) =>
             (app_field (resolveOvlPat firstpass) fs;
              if firstpass 
	      then rp := TUPLErp(map snd (sortRow (resolve3Dot loc fs rho))) 
	      else ())
         | TUPLErp pats =>
	     if firstpass
	     then fatalError "resolveOvlPat:2" 
	     else app (resolveOvlPat firstpass) pats)
  | VECpat ps => app (resolveOvlPat firstpass) ps
  | PARpat p => resolveOvlPat firstpass p
  | INFIXpat _ => fatalError "resolveOvlPat:3"
  | TYPEDpat(p,t) => 
      (resolveOvlPat firstpass p;
       resolveOvlTy firstpass t)
  | LAYEREDpat(p1, p2) =>
      (resolveOvlPat firstpass p1; 
       resolveOvlPat firstpass p2)
and resolveOvlExp firstpass (loc, exp') =
  case exp' of
    SCONexp sconInfo => 
	if firstpass then resolveOvlScon loc sconInfo else ()
  | VIDPATHexp (ref (RESvidpath vidpath)) => ()
  | VIDPATHexp (r as ref (OVLvidpath vidpathinfo)) => 
    (case vidpathinfo of
       (ii,ovltype,tau) =>
        if firstpass then 
	   ()
        else
	  let val {qualid, info} = ii
	      val {qual, id} = qualid
	      val pi = resolveOvlId loc (hd id) ovltype tau
	  in
	      #idKind info :=
	      { qualid={qual="General", id=id}, info=PRIMik pi };
	      r := RESvidpath ii
	  end )
  | FNexp mrules =>
      app (resolveOvlMRule firstpass) mrules
  | APPexp(e1, e2) =>
      (resolveOvlExp firstpass e1; resolveOvlExp firstpass e2)
  | LETexp(dec, body) =>
      (resolveOvlDec firstpass dec; resolveOvlExp firstpass body)
  | RECexp(r as ref (RECre fs)) =>	(* firstpass only *)
      (app_field (resolveOvlExp firstpass) fs;
       if isTupleRow fs then
         r := TUPLEre(map snd fs)
       else ())
  | RECexp(ref (TUPLEre es)) => 
      if firstpass then fatalError "resolveOvlExp" 
      else app (resolveOvlExp firstpass) es
  | VECexp es =>
      app (resolveOvlExp firstpass) es
  | PARexp e =>
      resolveOvlExp firstpass e
  | INFIXexp (ref(UNRESinfixexp _))  => fatalError "resolveOvlExp"
  | INFIXexp (ref(RESinfixexp e)) => resolveOvlExp firstpass e
  | TYPEDexp(e,ty) =>
      (resolveOvlExp firstpass e;
       resolveOvlTy firstpass ty)
  | ANDALSOexp(e1, e2) =>
      (resolveOvlExp firstpass e1; resolveOvlExp firstpass e2)
  | ORELSEexp(e1, e2) =>
      (resolveOvlExp firstpass e1; resolveOvlExp firstpass e2)
  | HANDLEexp(e, mrules) =>
      (resolveOvlExp firstpass e; app (resolveOvlMRule firstpass) mrules)
  | RAISEexp e =>
      resolveOvlExp firstpass e
  | IFexp(e0, e1, e2) =>
      (resolveOvlExp firstpass e0; resolveOvlExp firstpass e1; resolveOvlExp firstpass e2)
  | WHILEexp(e1, e2) =>
      (resolveOvlExp firstpass e1; resolveOvlExp firstpass e2)
  | SEQexp(e1, e2) =>
      (resolveOvlExp firstpass e1; resolveOvlExp firstpass e2)
  | STRUCTUREexp(modexp,sigexp,_) =>
      (resolveOvlModExp firstpass modexp;resolveOvlSigExp firstpass sigexp)
  | FUNCTORexp(modexp,sigexp,_) =>
      (resolveOvlModExp firstpass modexp;resolveOvlSigExp firstpass sigexp)
and resolveOvlMRule firstpass (MRule(ref pats, exp)) =
    (app (resolveOvlPat firstpass) pats; 
     resolveOvlExp firstpass exp) 
and resolveOvlDec firstpass (_, dec') =
  case dec' of
    VALdec (_, (pvbs, rvbs)) =>
      (app (resolveOvlValBind firstpass) pvbs; 
       app (resolveOvlValBind firstpass) rvbs)
  | PRIM_VALdec (_,pbds) => 
      resolveOvlPrimValBindList firstpass pbds
  | FUNdec (ref (UNRESfundec _)) => fatalError "resolveOvlDec"
  | FUNdec (ref (RESfundec dec)) => resolveOvlDec firstpass dec
  | TYPEdec tbds => resolveOvlTypBindList firstpass tbds   
  | PRIM_TYPEdec _ => ()
  | DATATYPEdec (dbds,SOME tbds) => 
       (resolveOvlDatBindList firstpass dbds;
	resolveOvlTypBindList firstpass tbds)
  | DATATYPEdec (dbds,NONE) => 
       (resolveOvlDatBindList firstpass dbds)      
  | DATATYPErepdec(_,tyconpath) => 
      resolveOvlTyConPath firstpass tyconpath
  | ABSTYPEdec(dbds,NONE, dec2) =>
      (resolveOvlDatBindList firstpass dbds;      
       resolveOvlDec firstpass dec2)
  | ABSTYPEdec(dbds,SOME tbds , dec2) =>
      (resolveOvlDatBindList firstpass dbds;      
       resolveOvlTypBindList firstpass tbds;
       resolveOvlDec firstpass dec2)
  | EXCEPTIONdec ebs => 
      resolveOvlExBindList firstpass ebs
  | LOCALdec(dec1, dec2) =>
      (resolveOvlDec firstpass dec1; resolveOvlDec firstpass dec2)
  | OPENdec _ => ()
  | EMPTYdec => ()
  | SEQdec(dec1, dec2) =>
      (resolveOvlDec firstpass dec1; resolveOvlDec firstpass dec2)
  | FIXITYdec _ => ()
  | STRUCTUREdec mbs => 
      resolveOvlModBindList firstpass mbs
  | FUNCTORdec fbs => 
      resolveOvlFunBindList firstpass fbs
  | SIGNATUREdec sbs => 
      resolveOvlSigBindList firstpass sbs
and resolveOvlValBind firstpass (ValBind(ref pat, exp)) =
       (resolveOvlPat firstpass pat;
	resolveOvlExp firstpass exp) 
and resolveOvlExBindList firstpass ebs  =
    app (fn EXDECexbind(_, SOME ty) => resolveOvlTy firstpass ty
	 | EXDECexbind(_, NONE) => ()
	 | EXEQUALexbind(_,_) => ()) ebs
and resolveOvlModBindList firstpass mbs = 
      app (resolveOvlModBind firstpass) mbs
and resolveOvlModBind firstpass (MODBINDmodbind (_,modexp)) =
      resolveOvlModExp firstpass modexp
  | resolveOvlModBind firstpass (ASmodbind (modid,sigexp,exp)) =
      (resolveOvlSigExp firstpass sigexp;
       resolveOvlExp firstpass exp)     
and resolveOvlFunBindList firstpass fbs = 
    app (resolveOvlFunBind firstpass) fbs
and resolveOvlFunBind firstpass (FUNBINDfunbind (funid,modexp)) =
      resolveOvlModExp firstpass modexp
  | resolveOvlFunBind firstpass (ASfunbind (funid,sigexp,exp)) =
      (resolveOvlSigExp firstpass sigexp;
       resolveOvlExp firstpass exp)     
and resolveOvlSigBindList firstpass sbs = 
    app (resolveOvlSigBind firstpass) sbs
and resolveOvlSigBind firstpass (SIGBINDsigbind (_,sigexp)) =
    resolveOvlSigExp firstpass sigexp
and resolveOvlModExp firstpass (loc,(modexp',_)) =
    case modexp' of
      DECmodexp dec => resolveOvlDec firstpass dec
    | LONGmodexp _ => ()
    | CONmodexp (modexp,sigexp) => 
       (resolveOvlModExp firstpass modexp;resolveOvlSigExp firstpass sigexp) 
    | ABSmodexp (modexp,sigexp) => 
       (resolveOvlModExp firstpass modexp;resolveOvlSigExp firstpass sigexp) 
    | LETmodexp (dec,modexp) => 
       (resolveOvlDec firstpass dec;resolveOvlModExp firstpass modexp)
    | PARmodexp modexp => 
       resolveOvlModExp firstpass modexp
    | FUNCTORmodexp (_,modid,_,sigexp,modexp) => 
       (resolveOvlSigExp firstpass sigexp;
	resolveOvlModExp firstpass modexp)
    | APPmodexp (funmodexp,modexp) => 
       (resolveOvlModExp firstpass funmodexp;
	resolveOvlModExp firstpass modexp)
    | RECmodexp (modid,_,sigexp,modexp) => 
       (resolveOvlSigExp firstpass sigexp;
	resolveOvlModExp firstpass modexp)
and resolveOvlTyConPath firstpass (_, LONGtyconpath _) =  ()
  | resolveOvlTyConPath firstpass (_, WHEREtyconpath (_,_,modexp)) = 
      resolveOvlModExp firstpass modexp
and resolveOvlTy firstpass (_, ty') =
  case ty' of
    TYVARty ii => ()
  | RECty fs =>
      app (fn(_, ty) => resolveOvlTy firstpass ty) fs
  | CONty(tys, _) =>
      app (resolveOvlTy firstpass) tys
  | FNty(ty1, ty2) =>
      (resolveOvlTy firstpass ty1; 
       resolveOvlTy firstpass ty2)
  | PACKty(sigexp) =>
       resolveOvlSigExp firstpass sigexp
  | PARty(ty) =>
       resolveOvlTy firstpass ty
and resolveOvlSigExp firstpass (_,sigexp) =
  case sigexp of
    SPECsigexp spec => resolveOvlSpec firstpass spec
  | SIGIDsigexp _ => ()
  | WHEREsigexp (sigexp, tyvarseq, longtycon, ty) =>
     (resolveOvlSigExp firstpass sigexp; 
      resolveOvlTy firstpass ty)
  | FUNSIGsigexp (_,modid, sigexp,sigexp') =>
     (resolveOvlSigExp firstpass sigexp;
      resolveOvlSigExp firstpass sigexp')
  | RECsigexp (modid, sigexp,sigexp') =>
     (resolveOvlSigExp firstpass sigexp;
      resolveOvlSigExp firstpass sigexp')
and resolveOvlSpec firstpass (_, spec') = 
  case spec' of
    VALspec (_,vds) => resolveOvlValDescList firstpass vds
  | PRIM_VALspec (_,pbs) => resolveOvlPrimValBindList firstpass pbs
  | TYPEDESCspec _ => ()
  | TYPEspec tbds => resolveOvlTypBindList firstpass tbds   
  | DATATYPEspec (dbds,SOME tbds) => 
       (resolveOvlDatBindList firstpass dbds;
	resolveOvlTypBindList firstpass tbds)
  | DATATYPEspec (dbds,NONE) => 
       (resolveOvlDatBindList firstpass dbds)      
  | DATATYPErepspec (_,tyconpath) =>       
        resolveOvlTyConPath firstpass tyconpath
  | EXCEPTIONspec eds => 
      resolveOvlExDescList firstpass eds
  | LOCALspec(spec1, spec2) =>
      (resolveOvlSpec firstpass spec1;
       resolveOvlSpec firstpass spec2)
  | OPENspec _ => ()
  | EMPTYspec => ()
  | SEQspec(spec1, spec2) =>
       (resolveOvlSpec firstpass spec1;
	resolveOvlSpec firstpass spec2)
  | INCLUDEspec sigexp => 
       resolveOvlSigExp firstpass sigexp
  | STRUCTUREspec moddescs => 
       resolveOvlModDescList firstpass moddescs
  | FUNCTORspec fundescs => 
       resolveOvlFunDescList firstpass fundescs
  | SHARINGTYPEspec (spec, longtycons) => 
       resolveOvlSpec firstpass spec
  | SHARINGspec (spec, longmodids) => 
       resolveOvlSpec firstpass spec
  | FIXITYspec _ => ()
  | SIGNATUREspec sigdescs =>
       resolveOvlSigBindList firstpass sigdescs
and resolveOvlModDescList firstpass mds = 
    app (fn MODDESCmoddesc(modid,sigexp) =>
	 resolveOvlSigExp firstpass sigexp) 
        mds
and resolveOvlFunDescList firstpass fds =
    app (fn FUNDESCfundesc(modid,sigexp) => 
	 resolveOvlSigExp firstpass sigexp) 
        fds
and resolveOvlTypBindList firstpass tbds =
    app (fn (tyvarseq,tycon,ty) => resolveOvlTy firstpass ty) tbds
and resolveOvlExDescList firstpass eds = 
    app (fn (_,SOME ty) => resolveOvlTy firstpass ty
	 | (_,NONE) => ())
         eds
and resolveOvlDatBindList firstpass dbds = 
    app (fn (tyvarseq, tycon, cbds) => resolveOvlConBindList firstpass cbds) dbds
and resolveOvlConBindList firstpass cbds = 
    app (fn ConBind (ii, NONE) => ()
         |  ConBind (ii, SOME ty) =>  resolveOvlTy firstpass ty) 
        cbds
and resolveOvlPrimValBindList firstpass (pbs) =
  (app (fn (ii,ty,arity,n) => resolveOvlTy firstpass ty) pbs)
and resolveOvlValDescList firstpass vds =
   app (fn (ii,ty) => resolveOvlTy firstpass ty) vds
;

(* We perform two passes over the declaration to resolve overloading:
 * Pass 1 resolves overloaded constants (and their default types),
 * Pass 2 resolves overloaded operators (and their default types).
 *)

val resolveOvlDec = 
    fn dec => (resolveOvlDec true dec; resolveOvlDec false dec);



 







