(* front.ml : translation abstract syntax -> extended lambda-calculus. *)

open Misc List Obj Fnlib Config Mixture Const Smlexc Prim Lambda Smlprim;
open Globals Location Units Types Asynt Asyntfn Tr_env Match;


datatype SMLPrimImpl =
    GVprim of QualifiedIdent
  | VMprim of int * primitive
  | VMPprim of int * primitive
  | GVTprim of QualifiedIdent * obj
;

val getPrimImpl = fn
    MLPeq =>         VMPprim(1, Pccall("sml_equal", 2))
  | MLPnoteq =>      VMPprim(1, Pccall("sml_not_equal", 2))
  | MLPeq_c =>       VMprim (2, Pccall("sml_equal", 2))
  | MLPnoteq_c =>    VMprim (2, Pccall("sml_not_equal", 2))
  | MLPref =>        VMprim (1, Pmakeblock (CONtag(refTag, 1)))
  | MLPsetref =>     VMPprim(1, Psetfield 0)
  | MLPsetref_c  =>  VMprim (2, Psetfield 0)
  | MLPadd_int   =>  VMPprim(1, Psmladdint)
  | MLPsub_int   =>  VMPprim(1, Psmlsubint)
  | MLPmul_int   =>  VMPprim(1, Psmlmulint)
  | MLPdiv_int   =>  VMPprim(1, Psmldivint)
  | MLPmod_int   =>  VMPprim(1, Psmlmodint)
  | MLPquot_int  =>  VMPprim(1, Psmlquotint)
  | MLPrem_int   =>  VMPprim(1, Psmlremint)
  | MLPeq_int    =>  VMPprim(1, Ptest(Pint_test PTeq))
  | MLPnoteq_int =>  VMPprim(1, Ptest(Pint_test PTnoteq))
  | MLPlt_int    =>  VMPprim(1, Ptest(Pint_test PTlt))
  | MLPgt_int    =>  VMPprim(1, Ptest(Pint_test PTgt))
  | MLPle_int    =>  VMPprim(1, Ptest(Pint_test PTle))
  | MLPge_int    =>  VMPprim(1, Ptest(Pint_test PTge))
  | MLPadd_int_c =>  VMprim (2, Psmladdint)
  | MLPsub_int_c =>  VMprim (2, Psmlsubint)
  | MLPmul_int_c =>  VMprim (2, Psmlmulint)
  | MLPdiv_int_c =>  VMprim (2, Psmldivint)
  | MLPmod_int_c =>  VMprim (2, Psmlmodint)
  | MLPquot_int_c => VMprim (2, Psmlquotint)
  | MLPrem_int_c =>  VMprim (2, Psmlremint)
  | MLPeq_int_c =>   VMprim (2, Ptest(Pint_test PTeq))
  | MLPnoteq_int_c => VMprim (2, Ptest(Pint_test PTnoteq))
  | MLPlt_int_c =>   VMprim (2, Ptest(Pint_test PTlt))
  | MLPgt_int_c =>   VMprim (2, Ptest(Pint_test PTgt))
  | MLPle_int_c =>   VMprim (2, Ptest(Pint_test PTle))
  | MLPge_int_c =>   VMprim (2, Ptest(Pint_test PTge))
  | MLPadd_real =>   VMPprim(1, Pfloatprim Psmladdfloat)
  | MLPsub_real =>   VMPprim(1, Pfloatprim Psmlsubfloat)
  | MLPmul_real =>   VMPprim(1, Pfloatprim Psmlmulfloat)
  | MLPdiv_real =>   VMPprim(1, Pfloatprim Psmldivfloat)
  | MLPlt_real =>    VMPprim(1, Ptest(Pfloat_test PTlt))
  | MLPgt_real =>    VMPprim(1, Ptest(Pfloat_test PTgt))
  | MLPle_real =>    VMPprim(1, Ptest(Pfloat_test PTle))
  | MLPge_real =>    VMPprim(1, Ptest(Pfloat_test PTge))
  | MLPadd_real_c => VMprim (2, Pfloatprim Psmladdfloat)
  | MLPsub_real_c => VMprim (2, Pfloatprim Psmlsubfloat)
  | MLPmul_real_c => VMprim (2, Pfloatprim Psmlmulfloat)
  | MLPdiv_real_c => VMprim (2, Pfloatprim Psmldivfloat)
  | MLPlt_real_c =>  VMprim (2, Ptest(Pfloat_test PTlt))
  | MLPgt_real_c =>  VMprim (2, Ptest(Pfloat_test PTgt))
  | MLPle_real_c =>  VMprim (2, Ptest(Pfloat_test PTle))
  | MLPge_real_c =>  VMprim (2, Ptest(Pfloat_test PTge))
  | MLPlt_string =>  VMPprim(1, Ptest(Pstring_test PTlt))
  | MLPgt_string =>  VMPprim(1, Ptest(Pstring_test PTgt))
  | MLPle_string =>  VMPprim(1, Ptest(Pstring_test PTle))
  | MLPge_string =>  VMPprim(1, Ptest(Pstring_test PTge))
  | MLPconcat =>     VMPprim(1, Pccall("sml_concat", 2))
  | MLPlt_string_c => VMprim (2, Ptest(Pstring_test PTlt))
  | MLPgt_string_c => VMprim (2, Ptest(Pstring_test PTgt))
  | MLPle_string_c => VMprim (2, Ptest(Pstring_test PTle))
  | MLPge_string_c => VMprim (2, Ptest(Pstring_test PTge))
  | MLPconcat_c    => VMprim(2, Pccall("sml_concat", 2))
  | MLPadd_word   =>  VMPprim(1, Paddint)
  | MLPsub_word   =>  VMPprim(1, Psubint)
  | MLPmul_word   =>  VMPprim(1, Pmulint)
  | MLPdiv_word   =>  VMPprim(1, Pdivint)
  | MLPmod_word   =>  VMPprim(1, Pmodint)
  | MLPadd_word_c =>  VMprim (2, Paddint)
  | MLPsub_word_c =>  VMprim (2, Psubint)
  | MLPmul_word_c =>  VMprim (2, Pmulint)
  | MLPdiv_word_c =>  VMprim (2, Pdivint)
  | MLPmod_word_c =>  VMprim (2, Pmodint)
  | MLPeq_word     => VMPprim(1, Ptest(Pword_test PTeq))
  | MLPnoteq_word  => VMPprim(1, Ptest(Pword_test PTnoteq))
  | MLPlt_word     => VMPprim(1, Ptest(Pword_test PTlt))
  | MLPgt_word     => VMPprim(1, Ptest(Pword_test PTgt))
  | MLPle_word     => VMPprim(1, Ptest(Pword_test PTle))
  | MLPge_word     => VMPprim(1, Ptest(Pword_test PTge))
  | MLPeq_word_c   => VMprim (2, Ptest(Pword_test PTeq))
  | MLPnoteq_word_c => VMprim (2, Ptest(Pword_test PTnoteq))
  | MLPlt_word_c   => VMprim (2, Ptest(Pword_test PTlt))
  | MLPgt_word_c   => VMprim (2, Ptest(Pword_test PTgt))
  | MLPle_word_c   => VMprim (2, Ptest(Pword_test PTle))
  | MLPge_word_c   => VMprim (2, Ptest(Pword_test PTge))
  | MLPprim(arity, prim)  => VMprim(arity, prim)
  | MLPccall(arity, name) => VMprim(arity, Pccall(name, arity))
  | MLPgv qualid         => GVprim qualid
  | MLPgvt(qualid, ref sc) =>   GVTprim(qualid, sc)
;

val curriedPrimVersion = fn
    MLPeq       =>    SOME MLPeq_c
  | MLPnoteq    =>    SOME MLPnoteq_c
  | MLPsetref   =>    SOME MLPsetref_c
  | MLPadd_int  =>    SOME MLPadd_int_c
  | MLPsub_int  =>    SOME MLPsub_int_c
  | MLPmul_int  =>    SOME MLPmul_int_c
  | MLPdiv_int  =>    SOME MLPdiv_int_c
  | MLPmod_int  =>    SOME MLPmod_int_c
  | MLPquot_int =>    SOME MLPquot_int_c
  | MLPrem_int  =>    SOME MLPrem_int_c
  | MLPeq_int   =>    SOME MLPeq_int_c
  | MLPnoteq_int =>   SOME MLPnoteq_int_c
  | MLPlt_int   =>    SOME MLPlt_int_c
  | MLPgt_int   =>    SOME MLPgt_int_c
  | MLPle_int   =>    SOME MLPle_int_c
  | MLPge_int   =>    SOME MLPge_int_c
  | MLPadd_real =>    SOME MLPadd_real_c
  | MLPsub_real =>    SOME MLPsub_real_c
  | MLPmul_real =>    SOME MLPmul_real_c
  | MLPdiv_real =>    SOME MLPdiv_real_c
  | MLPlt_real  =>    SOME MLPlt_real_c
  | MLPgt_real  =>    SOME MLPgt_real_c
  | MLPle_real  =>    SOME MLPle_real_c
  | MLPge_real  =>    SOME MLPge_real_c
  | MLPlt_string =>   SOME MLPlt_string_c
  | MLPgt_string =>   SOME MLPgt_string_c
  | MLPle_string =>   SOME MLPle_string_c
  | MLPge_string =>   SOME MLPge_string_c
  | MLPconcat    =>   SOME MLPconcat_c
  | MLPadd_word  =>   SOME MLPadd_word_c
  | MLPsub_word  =>   SOME MLPsub_word_c
  | MLPmul_word  =>   SOME MLPmul_word_c
  | MLPdiv_word  =>   SOME MLPdiv_word_c
  | MLPmod_word  =>   SOME MLPmod_word_c
  | MLPeq_word   =>   SOME MLPeq_word_c
  | MLPnoteq_word =>   SOME MLPnoteq_word_c
  | MLPlt_word   =>   SOME MLPlt_word_c
  | MLPgt_word   =>   SOME MLPgt_word_c
  | MLPle_word   =>   SOME MLPle_word_c
  | MLPge_word   =>   SOME MLPge_word_c
  | _            =>   NONE
;

(* Translation of expressions *)

exception Not_constant;

fun extractConstant (Lconst cst) = cst
  | extractConstant _ = raise Not_constant;

fun mkDynexn1 exnname arg = Lprim(Pmakeblock (CONtag(0,1)), [exnname, arg])
fun mkDynexn0 exnname     = mkDynexn1 exnname (Lconst constUnit)

(* ps: TODO perhaps share the code for raising Bind and Match *)

val bindExn  = 
    mkDynexn0 (Lprim(Pget_global ({qual="General", id=["exn_bind"]}, 0), []));
val matchExn = 
    mkDynexn0 (Lprim(Pget_global ({qual="General", id=["exn_match"]}, 0), []))
val bindRaiser  = Lprim(Praise, [bindExn]);
val matchRaiser = Lprim(Praise, [matchExn]);

fun partial_fun (loc as Loc(start,stop)) () =
    (msgIBlock 0;
     errLocation loc;
     errPrompt "Warning: pattern matching is not exhaustive";
     msgEOL(); msgEOL();
     msgEBlock();
     matchRaiser);

fun partial_let (onTop : bool) (loc as Loc(start,stop)) () =
    (if not onTop then
	 (msgIBlock 0;
	  errLocation loc;
	  errPrompt "Warning: pattern matching is not exhaustive";
	  msgEOL(); msgEOL();
	  msgEBlock())
     else ();
     bindRaiser);

fun partial_try () = Lprim(Praise, [Lvar 0]);

fun extract_fields arity =
  let fun loop i =
    if i >= arity then []
    else
      Lprim(Pfield i, [Lvar 0]) :: loop (i+1)
  in loop 0 end
;

fun normApp (func as (_, func')) args =
  case func' of
      PARexp e        => normApp e args
    | TYPEDexp(e,_)   => normApp e args
    | APPexp(e1,e2)   => normApp e1 (e2 :: args)
    | _               => (func, args)
;

fun extractPairArg (_, exp') =
  case exp' of
      PARexp e                       => extractPairArg e
    | TYPEDexp(e,_)                  => extractPairArg e
    | RECexp(ref (TUPLEre [e1,e2]))  => SOME (e1, e2)
    | _                              => NONE
;

fun canSplitFirstArg (Lvar n :: args) = true
  | canSplitFirstArg (Lprim(Pget_global _, []) :: args) = true
  | canSplitFirstArg _ = false
;

fun splitFirstArg (arg :: args) =
      Lprim(Pfield 0, [arg]) :: Lprim(Pfield 1, [arg]) :: args
  | splitFirstArg _ = fatalError "splitFirstArg"
;

(* An expression is "safe", if evaluating it can't produce *)
(* side-effects, i.e. I/O, exceptions, etc. *)
(* The following is a crude approximation... *)

fun isSafe (_, exp') =
  case exp' of
    SCONexp _ => true
  | VIDPATHexp (ref (RESvidpath _))=> true
  | VIDPATHexp (ref (OVLvidpath _)) => fatalError "isSafe:1"
  | FNexp _ => true
  | APPexp(e1,e2) => false
  | RECexp(ref (RECre fs)) =>
      all (fn (_, e) => isSafe e) fs
  | RECexp(ref (TUPLEre es)) =>
      all isSafe es
  | VECexp es =>
      all isSafe es
  | PARexp e => isSafe e
  | LETexp (dec,exp) => false
  | INFIXexp (ref (UNRESinfixexp e)) => fatalError "isSafe:2"
  | INFIXexp (ref (RESinfixexp e)) => isSafe e
  | TYPEDexp(e,ty) => isSafe e
  | ANDALSOexp(e1,e2) =>
      isSafe e1 andalso isSafe e2
  | ORELSEexp(e1,e2) =>
      isSafe e1 andalso isSafe e2
  | HANDLEexp(e, mrules) => false
  | RAISEexp e => false
  | IFexp(e0,e1,e2) =>
      isSafe e0 andalso isSafe e1 andalso isSafe e2
  | WHILEexp(e1,e2) =>
      isSafe e1 andalso isSafe e2
  | SEQexp(e1,e2) =>
      isSafe e1 andalso isSafe e2
  | STRUCTUREexp (modexp,_,_) => isSafeModExp modexp
  | FUNCTORexp (modexp,_,_) => isSafeModExp modexp
and isSafeModExp (_, (modexp',_)) = 
    case modexp' of
      DECmodexp _ => false
    | LONGmodexp _ => true
    | LETmodexp (dec,modexp) => false
    | PARmodexp modexp => isSafeModExp modexp
    | CONmodexp (modexp,sigexp) => isSafeModExp modexp
    | ABSmodexp  (modexp,sigexp) => isSafeModExp modexp
    | FUNCTORmodexp (_,modid,_, sigexp, modexp) => true
    | APPmodexp (modexp,modexp') => false
    | RECmodexp (modid,_,sigexp, modexp) => false
;

(* All unsafe arguments must be lifted, except the rightmost one, *)
(* in order to preserve the evaluation order. *)

datatype AppArgs =
    SAFEarg of Exp
  | CONSTarg of Lambda
  | UNSAFEarg
;

fun trConVar (ci : ConInfo) =
  let val {conArity, conIsGreedy, conTag, conSpan, ...} = !ci in
    case (conIsGreedy, conArity, conSpan) of
        (true,  _, _) =>
          Lfn(Lprim(
            Pmakeblock(CONtag(conTag,conSpan)), extract_fields conArity))
      | (false, 0, _) =>
          Lconst(BLOCKsc(CONtag(conTag,conSpan), []))
      | (false, _, 1) =>
          Lfn(Lvar 0)
      | (false, _, _) =>
          Lfn(Lprim(Pmakeblock(CONtag(conTag,conSpan)), [(Lvar 0)]))
  end;

fun trExConVar (env as (rho, depth))  (ii : IdInfo) (ei:ExConInfo) =
  let val {exconArity, ...} = !ei
      val en = translateLongAccess ValId env ii
  in 
      if exconArity = 0 then mkDynexn0 en
      else Llet([en], Lfn(mkDynexn1 (Lvar 1) (Lvar 0)))
  end 

fun trTopDynExConVar (ei : ExConInfo) (en:Lambda) =
  let val {exconArity,...} = !ei
  in if exconArity = 0 then 
         mkDynexn0 en
     else
	 Llet([en], Lfn(mkDynexn1 (Lvar 1) (Lvar 0)))
  end;

fun trPrimVar prim =
  case getPrimImpl prim of
      GVprim globalName =>
        Lprim(Pget_global (globalName, 0), [])
    | VMprim(arity, p) =>
        let fun make_fn n args =
          if n >= arity
          then Lprim(p, args)
          else Lfn(make_fn (n+1) (Lvar n :: args))
        in make_fn 0 [] end
    | VMPprim(arity, p) =>
        let fun make_fn n args =
          if n >= arity
          then Lprim(p, splitFirstArg args)
          else Lfn(make_fn (n+1) (Lvar n :: args))
        in make_fn 0 [] end
    | GVTprim(globalName, sc) =>
        Lfn(Lapply(
              Lprim(Pget_global (globalName, 0), []),
              [Lconst(QUOTEsc (ref sc)), Lvar 0]))
;

fun trVar (env as (rho, depth)) (ii : IdInfo) =
  let val {info={idKind,...}, ...} = ii
      val {info,qualid} = !idKind
  in
    (case info of
         VARik =>
           translateLongAccess ValId env ii
       | STRik =>
           translateLongAccess ModId env ii
       | FUNik =>
           translateLongAccess FunId env ii
       | PRIMik pi =>
           trPrimVar (#primOp pi)
       | CONik ci =>
          trConVar ci
       | EXCONik ei =>
          trExConVar env ii ei) 
  end;


(* coercion *)

fun getGlobal asId {qual,id} =
  Lprim(Pget_global ({qual=qual,id = [mangle (asId (longIdentAsIdent id "getGlobal"))]},0),[])
;

fun coerceVarEnv S VE' =
 let val lookupVEofS = lookupVEofStr S 
 in foldEnv 
     (fn id => fn {info = (_,cs'),...} => fn tr_VE => 
      case cs' of  
	  VARname _ => 
	    (let val (field,{qualid,info=(_,cs)}) = lookupVEofS id 
	     in case cs of
		 VARname _ => 
		     if isGlobalName qualid 
			 then (Lprim(Pget_global(qualid,0),[]))::tr_VE
		     else
			 (Lprim(Pfield field,[Lvar 0]))::tr_VE
	       |  PRIMname pi => (trPrimVar (#primOp pi))::tr_VE
	       |  CONname ci => (trConVar ci)::tr_VE
	       |  EXNname ei => 
		     (let val {exconArity, ...} = !ei
			  val en = 
			      if isGlobalName qualid then
				  getGlobal ValId qualid
			      else Lprim(Pfield field,[Lvar 0])
		      in
			  if exconArity = 0 then
			      mkDynexn0 en 
			  else
			      Llet([en], Lfn(mkDynexn1 (Lvar 1) (Lvar 0)))
		      end 
		      :: tr_VE)
	       | REFname => fatalError "coerceVarEnv:1"
	     end)
	| EXNname ei' => 
	     let val (field,{qualid,info = (_,cs)}) = lookupVEofS id 
	     in case cs of
		 EXNname ei => 
		     (let val {exconArity, ...} = !ei
		      in
			  if isGlobalName qualid then getGlobal ValId qualid
			  else Lprim(Pfield field, [Lvar 0])
		      end 
		     :: tr_VE)
	       | _  => fatalError "coerceVarEnv:3"
	     end
	| _ (* PRIMname pi' | CONname ci' | REFname *) => 
	     tr_VE)
     [] 
     VE'
 end
and coerceFunEnv S FE' tr_VE =
    let val lookupFEofS = lookupFEofStr S
    in foldEnv 
	(fn id => fn {info = F',...} => fn tr_FE_VE =>
	 let val (field,{qualid,info = F}) = lookupFEofS id 
	     val trF = if isGlobalName qualid 
			   then  getGlobal FunId qualid
		       else Lprim(Pfield field, [Lvar 0])
			   
	 in 
	     (coerceFun trF F F')::tr_FE_VE
	 end) 
	tr_VE 
	FE'
    end
and coerceModEnv S ME' tr_FE_VE =
    let val lookupMEofS = lookupMEofStr S   
    in foldEnv 
	(fn id => fn {info = RS',...} => fn tr_ME_FE_VE =>
	 let val (field,{qualid,info = RS}) = lookupMEofS id 
	     val trM = if isGlobalName qualid 
			   then  getGlobal ModId qualid
		       else Lprim(Pfield field, [Lvar 0])
			   
	 in 
	     (coerceRecStr trM RS RS')::tr_ME_FE_VE
	 end) 
	tr_FE_VE 
	ME'
    end
and coerceStr lam S S' =
    case S' of 
     STRstr(ME',FE',GE',TE',VE') =>
	 let val tr_VE = coerceVarEnv S VE' 
	     val tr_FE_VE = coerceFunEnv S FE' tr_VE
	     val tr_ME_FE_VE = coerceModEnv S ME' tr_FE_VE 
	 in
	     if isIdentityCoercion S 0 tr_ME_FE_VE 
		 then lam 
	     else 
		 Llet([lam],Lstruct tr_ME_FE_VE)
	 end
  | _ => fatalError "coerceStr"
and isIdentityCoercion S pos (Lprim(Pfield field, [Lvar 0])::lams) = 
      (pos = field) andalso isIdentityCoercion S (pos+1) lams
  | isIdentityCoercion S pos (_::lams) = false
  | isIdentityCoercion S pos [] = (sizeOfStr S) = pos
and coerceRecStr lam (NONrec S) (NONrec S') = coerceStr lam S S' 
  | coerceRecStr lam RS (RECrec (_,RS')) = coerceRecStr lam RS RS'    
  | coerceRecStr lam (RECrec (_,RS)) RS' = coerceRecStr lam RS RS'    
and coerceMod lam (STRmod RS) (STRmod RS') = coerceRecStr lam RS RS' 
  | coerceMod lam (FUNmod F) (FUNmod F') = coerceFun lam F F'
  | coerceMod _ _ _ = fatalError "coerceMod"
and coerceFun lam (_,M1,EXISTSexmod(_,M1')) (_,M2,EXISTSexmod(_,M2')) =
    let val domCoercion = coerceMod (Lvar 0) (normMod M2) (normMod M1)
	val rngCoercion = coerceMod (Lvar 0) (normMod M1') (normMod M2')
    in case (domCoercion,rngCoercion) of
	  (Lvar 0, Lvar 0) => lam
	| (Lvar 0, _) => 
	      Llet([lam], 
		   Lfn(Llet([Lapply(Lvar 1,[Lvar 0])],rngCoercion)))
	| (_, Lvar 0) => 
	      Llet([lam], 
		   Lfn(Llet([domCoercion],
			    Lapply(Lvar 2,[Lvar 0]))))
	 | (_,_) =>
	      Llet([lam], 
		   Lfn(Llet([domCoercion],
			    Llet([Lapply(Lvar 2,[Lvar 0])],
				 rngCoercion))))
    end;


fun coerceDecVarEnv (env as (rho,depth)) VE VE' =
    foldEnv 
     (fn id => fn {info = (_,cs'),...} => fn tr_VE => 
      case cs' of  
	  VARname _ => 
	    (let val {qualid,info=(_,cs)} = lookupEnv VE id 
	     in case cs of
		 VARname _ => 
		     if isGlobalName qualid 
			 then (Lprim(Pget_global(qualid,0),[]))::tr_VE
		     else
			 (translateLocalAccess ValId env id)::tr_VE
	       |  PRIMname pi => (trPrimVar (#primOp pi))::tr_VE
	       |  CONname ci => (trConVar ci)::tr_VE
	       |  EXNname ei => 
		     (let val {exconArity, ...} = !ei
			  val en = 
			      if isGlobalName qualid then 
				  getGlobal ValId qualid
			      else
				  translateLocalAccess ValId env id
		      in
			  if exconArity = 0 then
			      mkDynexn0 en 
			  else
			      Llet([en],Lfn(mkDynexn1 (Lvar 1) (Lvar 0)))
		      end
		      :: tr_VE)
	       | REFname => fatalError "coerceDecVarEnv:1"
	     end)
	| EXNname ei' => 
	     let val {qualid,info = (_,cs)} = lookupEnv VE id 
	     in 
	        case cs of
		   EXNname ei => 
		     (let val {exconArity, ...} = !ei
		      in
			  if isGlobalName qualid then getGlobal ValId qualid
			  else translateLocalAccess ValId env id
		      end
		      :: tr_VE)
		 | _  => fatalError "coerceDecVarEnv:3"
	     end
	| _ (* PRIMname pi' | CONname ci' | REFname *) => 
	     tr_VE)
     [] 
     VE'
and coerceDecFunEnv env FE FE' tr_VE =
    foldEnv 
	(fn id => fn {info = F',...} => fn tr_FE_VE =>
	 let val {qualid,info = F} = lookupEnv FE id 
	     val trF = if isGlobalName qualid 
			   then  getGlobal FunId qualid
		       else translateLocalAccess FunId env id
			   
	 in 
	     (coerceFun trF F F')::tr_FE_VE
	 end) 
	tr_VE 
	FE'
and coerceDecModEnv env ME ME' tr_FE_VE =
    foldEnv 
	(fn id => fn {info = RS',...} => fn tr_ME_FE_VE =>
	 let val {qualid,info = RS} = lookupEnv ME id 
	     val trM = if isGlobalName qualid 
			   then  getGlobal ModId qualid
		       else translateLocalAccess ModId env id
	 in 
	     (coerceRecStr trM RS RS')::tr_ME_FE_VE
	 end) 
	tr_FE_VE 
	ME'
(*
and coerceDec env (STRstr(ME,FE,TE,VE)) (STRstr(ME',FE',TE',VE')) =
      let val tr_VE = coerceDecVarEnv env VE VE' 
          val tr_FE_VE = coerceDecFunEnv env FE FE' tr_VE
          val tr_ME_FE_VE = coerceDecModEnv env ME ME' tr_FE_VE 
      in
	      Lstruct tr_ME_FE_VE
      end;
*)
and coerceDec env RS RS' =
      let val (ME,FE,_,_,VE) = 
	       case SofRecStr RS of
		 STRstr ES => ES
	       | _ => fatalError "coerceDec:1"
	  val (ME',FE',_,_,VE') = 
	      case SofRecStr RS' of
		  STRstr ES' => ES'
	      | _ => fatalError "coerceDec:2"
          val tr_VE = coerceDecVarEnv env VE VE' 
          val tr_FE_VE = coerceDecFunEnv env FE FE' tr_VE
          val tr_ME_FE_VE = coerceDecModEnv env ME ME' tr_FE_VE 
      in
	      Lstruct tr_ME_FE_VE
      end;

fun trExp (env as (rho, depth)) (exp as (loc, exp')) =
  case exp' of
    SCONexp (scon, _) =>
      Lconst (ATOMsc scon)
  | VIDPATHexp (ref (RESvidpath ii))=> 
      trVar env ii 
  | VIDPATHexp (ref (OVLvidpath _)) => fatalError "trExp:1"
  | FNexp [] =>
      fatalError "trExp:2"
  | FNexp(mrules as MRule(ref pats ,_)::_) =>
      foldR (fn pat => fn lam => Lfn lam)
            (trMatch loc env (partial_fun loc) mrules)
            pats
  | APPexp(e1,e2) =>
      (case normApp e1 [e2] of
           (func as (loc, FNexp mrules), args) =>
             if curriedness mrules = List.length args then
               Llet(trLetArgs env args,
                    trMatch loc env (partial_fun loc) mrules)
             else
               let val (env', tr_args, envelope) = trArgs env args
               in envelope(Lapply(trExp env' func, tr_args)) end
          | (func as (_,VIDPATHexp(ref (RESvidpath ii))),args)=>
              trVarApp env ii args
          | (func, args) =>
              let val (env', tr_args, envelope) = trArgs env (func :: args)
              in envelope(Lapply(hd tr_args, tl tr_args)) end)
  | RECexp(ref (RECre fs)) =>
      trRec env (CONtag(0,1)) fs
  | RECexp(ref (TUPLEre es)) =>
      trTuple env (CONtag(0,1)) es
  | VECexp es =>
      trTuple env (CONtag(0,1)) es
  | PARexp e => trExp env e
  | LETexp (dec,exp) =>
      let val ((rho', depth'), envelope) = trDec env dec
          val env'' = (plusEnv rho rho', depth')
      in envelope(trExp env'' exp) end
  | INFIXexp (ref (UNRESinfixexp es)) => fatalError "trExp:3"
  | INFIXexp (ref (RESinfixexp e)) => trExp env e
  | TYPEDexp(e,ty) => trExp env e
  | ANDALSOexp(e1,e2) =>
      Landalso(trExp env e1, trExp env e2)
  | ORELSEexp(e1,e2) =>
      Lorelse(trExp env e1, trExp env e2)
  | HANDLEexp(e, mrules) =>
      Lhandle(trExp env e, trMatch loc env partial_try mrules)
  | RAISEexp e =>
      Lprim(Praise, [trExp env e])
  | IFexp(e0,e1,e2) =>
      Lif(trExp env e0, trExp env e1, trExp env e2)
  | WHILEexp(e1,e2) =>
      Lwhile(trExp env e1, trExp env e2)
  | SEQexp(e1,e2) =>
      Lseq(trExp env e1, trExp env e2)
  | STRUCTUREexp(modexp,sigexp, ref (SOME (EXISTSexmod(_,M')))) =>
        trConstrainedModExp env modexp M'
  | STRUCTUREexp(modexp,sigexp,_) =>
        fatalError "trExp:4"
  | FUNCTORexp(modexp,sigexp, ref (SOME (EXISTSexmod(_,M')))) =>
        trConstrainedModExp env modexp M'
  | FUNCTORexp (modexp,sigexp, _) =>
        fatalError "trExp:5"      
and trVarApp env (ii : IdInfo) args =
  let val {info={idKind, ...},...} = ii in 
    case #info(!idKind) of
        VARik =>
          let val (env', tr_args, envelope) = trArgs env args
          in envelope(Lapply(translateLongAccess ValId env' ii, tr_args)) end
      | PRIMik pi =>
          let val {primOp, ...} = pi in
            case curriedPrimVersion primOp of
                NONE => trPrimApp env primOp args
              | SOME prim_c =>
                  (case extractPairArg (hd args) of
                        NONE => trPrimApp env primOp args
                      | SOME(arg', arg'') =>
                          trPrimApp env prim_c (arg'::arg''::(tl args)))
          end
      | CONik ci =>
          let val {conArity, conIsGreedy, conTag, conSpan, ...} = !ci in
            if List.length args <> 1 then
              fatalError "trVarApp: unary con requires 1 arg"
            else ();
            case (conIsGreedy, conArity, conSpan) of
                (true,  _, _) =>
                  (case (hd args) of
                      (_, RECexp(ref (RECre fs))) =>
                        trRec env (CONtag(conTag,conSpan)) fs
                    | (_, RECexp(ref (TUPLEre es))) =>
                        trTuple env (CONtag(conTag,conSpan)) es
                    | _ =>
                        Llet([trExp env (hd args)],
                              Lprim(Pmakeblock(CONtag(conTag,conSpan)),
                                    extract_fields conArity)))
              | (false, 0, _) =>
                  fatalError "trVarApp: nullary con in app"
              | (false, _, 1) =>
                  trExp env (hd args)
              | (false, _, _) =>
                  (* Normal unary con, in the end... *)
                  let val tr_arg = trExp env (hd args) in
                    Lconst(BLOCKsc(CONtag(conTag,conSpan),
                                    [extractConstant tr_arg]))
                    handle Not_constant =>
                        Lprim(Pmakeblock(CONtag(conTag,conSpan)), [tr_arg])
                  end
          end
      | EXCONik ei =>
          let val {exconArity, ...} = !ei 
	  in
	      if List.length args <> 1 then
		  fatalError "trVarApp: unary excon requires 1 arg"
	      else ();
	      if exconArity = 0 then
		  fatalError "trVarApp: nullary excon in app"
	      else ();
	      let val en = translateLongAccess ValId env ii
		  val tr_arg = trExp env (hd args)
	      in mkDynexn1 en tr_arg end
          end
      | STRik => fatalError "trVarApp: STRik"
      | FUNik => fatalError "trVarApp: FUNik"
  end

and trPrimApp env prim args =
    case getPrimImpl prim of
        GVprim globalName =>
	    let val (env', tr_args, envelope) = trArgs env args 
	    in envelope(Lapply(trPrimVar prim, tr_args)) end
      | VMprim(arity, p) =>
	    if arity <> List.length args then 
		let val (env', tr_args, envelope) = trArgs env args 
		in envelope(Lapply(trPrimVar prim, tr_args)) end
	    else 
		Lprim(p, map (trExp env) args)
      | VMPprim(arity, p) =>
	    let val (env', tr_args, envelope) = trArgs env args 
	    in
		if (arity <> List.length tr_args) then
		    envelope(Lapply(trPrimVar prim, tr_args))
		else if canSplitFirstArg tr_args then
		    envelope(Lprim(p, splitFirstArg tr_args))
		else if arity = 1 then
		    Llet(tr_args, Lprim(p, splitFirstArg [Lvar 0]))
		else
		    envelope(Lapply(trPrimVar prim, tr_args))
	    end
      | GVTprim(globalName, sc) =>
	    let val (env', tr_args, envelope) = trArgs env args 
	    in
		envelope(Lapply(Lprim(Pget_global (globalName, 0), []),
				Lconst(QUOTEsc (ref sc))::tr_args))
	    end

and trRec env tag fs =
  let val labs = map fst fs and es = map snd fs
      val (env', tr_es, envelope) = trArgs env es
      val tr_es' = map snd (sortRow (zip2 labs tr_es))
  in
      (envelope(Lconst(BLOCKsc(tag, map extractConstant tr_es'))))
      handle Not_constant => envelope(Lprim(Pmakeblock tag, tr_es'))
  end

and trTuple env tag es =
  let val tr_es = map (trExp env) es in
      (Lconst(BLOCKsc(tag, map extractConstant tr_es)))
      handle Not_constant => Lprim(Pmakeblock tag, tr_es)
  end

(* We recognize constant arguments only upon translating them, *)
(* to avoid repeated traversals of the abstract syntax tree. *)

and classifyArgs (env as (rho, depth)) unsafe safe = fn
    [] => (unsafe, safe)
  | arg :: args =>
      if isSafe arg then
        classifyArgs env unsafe ((SAFEarg arg) :: safe) args
      else
        let val lam = trExp env arg in
          case lam of
              Lconst _ =>
                classifyArgs env unsafe ((CONSTarg lam) :: safe) args
            | _ =>
                classifyArgs (rho, depth+1) (lam :: unsafe)
                             (UNSAFEarg :: safe) args
        end

and adjustHeadArgs env pos acc = fn
    [] => acc
  | SAFEarg exp :: rest =>
      adjustHeadArgs env pos (trExp env exp :: acc) rest
  | CONSTarg lam :: rest =>
      adjustHeadArgs env pos (lam :: acc) rest
  | UNSAFEarg :: rest =>
      adjustHeadArgs env (pos+1) (Lvar pos :: acc) rest

(* The rightmost unsafe expression needn't be lifted, *)
(* as it can't do any harm. *)

and adjustArgs env quasisafe acc = fn
    [] => fatalError "adjustArgs"
  | SAFEarg exp :: rest =>
      adjustArgs env quasisafe (trExp env exp :: acc) rest
  | CONSTarg lam :: rest =>
      adjustArgs env quasisafe (lam :: acc) rest
  | UNSAFEarg :: rest =>
      adjustHeadArgs env 0 (quasisafe :: acc) rest

and trArgs (env as (rho, depth)) args =
  case classifyArgs env [] [] args of
      ([], safe) => (env, adjustHeadArgs env 0 [] safe, fn lam => lam)
    | (quasisafe :: unsafe, safe) =>
        let val num = List.length unsafe
            val env' = (rho, depth + num)
        in
          (env',
           adjustArgs env' quasisafe [] safe,
           if num = 0 then fn lam => lam
                      else fn lam => Llet(rev unsafe, lam))
        end
and trOpenLongStrIdInfo depth ((_,ref NONE):LongModIdInfo) =
      fatalError "trOpenLongStrIdInfo:1"
  | trOpenLongStrIdInfo depth ((longstrid,ref(SOME (ME,FE,_,VE,_))):LongModIdInfo) =
      let val {info={idKind,...}, ...} = longstrid
	  val {qualid, ...} = !idKind
	  val _ = if isUnitName qualid then fatalError "trOpenLongStrIdInfo:2" else ()
      in 
	  let val (rhoME,posME) = 
	      foldEnv (fn id => fn {qualid,...} => fn cont => fn pos =>
		       if isGlobalName qualid
			   then cont pos 
		       else let val (rhoME,posME) = cont (pos + 1) 
				in
				    (bindInEnv rhoME  (ModId id) 
				     (Path_son (pos, Path_local depth)),
				     posME)
			    end)
	      (fn pos => (NILenv,pos))
	      ME
	      0
	      val (rhoMEFE,posMEFE) = 
		  foldEnv (fn id => fn {qualid,...} => fn cont => fn pos => 		
			   if isGlobalName qualid
			       then cont pos 
			   else let val (rhoMEFE,posMEFE) = cont (pos + 1) 
				in   (bindInEnv rhoMEFE  (FunId id) 
				      (Path_son (pos, Path_local depth)),
				      posMEFE)
				end)
		  (fn pos => (rhoME,pos))
		  FE
		  posME
	      val (rhoMEFEVE,posMEFEVE) = 
		  foldEnv (fn id => fn {qualid,info = (_,cs)} => fn cont => fn pos =>
			   case cs of  
			       VARname _ => (* cvr: review *)
				   if isGlobalName qualid 
				       then cont pos
				   else 
				       let val (rhoMEFEVE,posMEFEVE) = cont (pos + 1) 
				       in
					   (bindInEnv rhoMEFEVE  (ValId id) 
					    (Path_son (pos, Path_local depth)),
					    posMEFEVE)
				       end
			     | EXNname ei => 
				       if isGlobalName qualid then cont pos
				       else 
					   let val (rhoMEFEVE,posMEFEVE) = cont (pos + 1) 
					   in
					       (bindInEnv rhoMEFEVE  (ValId id) 
						(Path_son (pos, Path_local depth)),
						posMEFEVE)
					   end
			     | _ => cont pos)
		  (fn pos => (rhoMEFE,pos))
		  VE
		  posMEFE
	  in rhoMEFEVE
	  end
      end
and trValDec onTop (env as (rho, depth)) pvbs rvbs =
  let val ((rho',  depth'),  envelope' ) =
        trValBind onTop env pvbs
      val ((rho'', depth''), envelope'') =
        trRecValBind (rho, depth') rvbs
  in
    ((plusEnv rho' rho'', depth''), envelope' o envelope'')
  end
and trDec (env as (rho, depth)) (loc, dec') =
  case dec' of
    VALdec (_, (pvbs, rvbs)) =>
      trValDec false env pvbs rvbs
  | PRIM_VALdec _ => ((NILenv, depth), fn lam => lam)
  | FUNdec (ref (UNRESfundec _)) => fatalError "trDec"
  | FUNdec (ref (RESfundec dec)) => trDec env dec
  | TYPEdec _ => ((NILenv, depth), fn lam => lam)
  | PRIM_TYPEdec _ => ((NILenv, depth), fn lam => lam)
  | DATATYPEdec(dbs, _) => ((NILenv, depth), fn lam => lam)
  | DATATYPErepdec _ => ((NILenv, depth), fn lam => lam)
  | ABSTYPEdec(dbs, _, dec2) =>
      trDec env dec2
  | EXCEPTIONdec ebs =>
      trExBindList env ebs
  | LOCALdec(dec1,dec2) =>
      let val ((rho', depth'), envelope') =
                              trDec env dec1
          val ((rho'', depth''), envelope'') =
                              trDec ((plusEnv rho rho'), depth') dec2
      in ((rho'', depth''), envelope' o envelope'') end
  | OPENdec longmodidinfos =>
      trOpenLongModIdInfos env longmodidinfos
  | EMPTYdec => ((NILenv, depth), fn lam => lam)
  | SEQdec(dec1,dec2) =>
      let val ((rho', depth'), envelope') =
                              trDec env dec1
          val ((rho'', depth''), envelope'') =
                              trDec ((plusEnv rho rho'), depth') dec2
      in ((plusEnv rho' rho'', depth''), envelope' o envelope'') end
  | FIXITYdec  _ =>
	  ((NILenv, depth), fn lam => lam)
  | STRUCTUREdec mbs => 
       trModBindList env mbs
  | FUNCTORdec fbs => 
       trFunBindList env fbs
  | SIGNATUREdec _ => 
        ((NILenv, depth), fn lam => lam)
and trModBindList (env as (rho, depth)) mbs =
  let val id_path_list =
        mapFrom (fn depth =>
                   fn (MODBINDmodbind((loc,mid), _))   =>
                              (mid, Path_local depth)
		    | (ASmodbind((loc,mid), _,_))   =>
                              (mid, Path_local depth))
                depth mbs
      and len = List.length mbs
      and args = mapFrom (fn i => fn mb => trModBind (rho, i) mb) depth mbs
      val rho' = foldR (fn (id, path) => fn rho => bindInEnv rho (ModId id) path)
                       NILenv id_path_list
  in ((rho', depth+len), fn lam => Llet(args, lam)) 
  end
and trOpenLongModIdInfos (env as (rho, depth)) longmodidinfos =
  let val longstridinfos = 
      drop (fn ({info={idKind,...}, ...},_) =>
	    let val {qualid, ...} = !idKind
	    in  isUnitName qualid
	    end) longmodidinfos
      val rhos = mapFrom trOpenLongStrIdInfo depth longstridinfos
      and len = List.length longstridinfos
      and args = 
	  mapFrom (fn depth => fn (longstrid,_) => 
		   translateLongAccess ModId (rho,depth) longstrid)
	          depth
		  longstridinfos
      val rho' = foldR (fn rho => fn rho' => plusEnv rho rho') NILenv rhos
  in ((rho', depth+len), fn lam => Llet(args, lam)) 
  end
and trModBind env = fn
    MODBINDmodbind(_, modexp) =>
      trModExp env modexp
  | ASmodbind(_,_,exp) =>
      trExp env exp
and trFunBind env = fn
    FUNBINDfunbind(funid, modexp) =>
      trModExp env modexp
  | ASfunbind(_,_,exp) =>
      trExp env exp
and trFunBindList (env as (rho, depth)) fbs =
  let val id_path_list =
        mapFrom (fn depth =>
		 fn (FUNBINDfunbind((loc,funid), _)) =>
		       (funid, Path_local depth)
		  | (ASfunbind((loc,funid), _,_)) =>
		       (funid, Path_local depth))
                depth fbs
      and len = List.length fbs
      and args = 
	  mapFrom (fn depth => fn mb => trFunBind (rho, depth) mb) 
	          depth 
		  fbs
      val rho' = foldR (fn (id, path) => fn rho => bindInEnv rho (FunId id) path)
                       NILenv id_path_list
  in ((rho', depth+len), fn lam => Llet(args, lam)) end
and trModExp (env as (rho,depth)) (_, (modexp,r)) = 
  (* cvr: consider setting r to NONE to free up space *)
  case (modexp,!r) of
    (DECmodexp dec, SOME (EXISTSexmod (_, STRmod RS))) =>
      let val (ME,FE,_,_,VE) = 
	       case SofRecStr RS of
		   STRstr ES => ES
	       | _ => fatalError "trModExp:1"
	  val (env as (rho', depth'), envelope') = trDec env dec
          val tr_VE = 
	      foldEnv  (fn id => fn {qualid,info = (_,cs)} => fn tr_VE => 
			case cs of  
			    VARname _ => 
				if isGlobalName qualid 
				    then tr_VE
				else (translateLocalAccess ValId env id) :: tr_VE 
			  | EXNname ei => 
				    if isGlobalName qualid then tr_VE 
				    else (translateLocalAccess ValId env id) :: tr_VE
			  | _  => tr_VE (* PRIMname,CONname & REFname cases *) )
	               [] 
		       VE                     
          val tr_FE_VE =  foldEnv (fn id => fn {qualid,...} => fn tr_FE_VE => 
				  if isGlobalName qualid then
				      tr_FE_VE
				  else (translateLocalAccess FunId env id):: tr_FE_VE)
                                 tr_VE 
				 FE                     
          val tr_ME_FE_VE = foldEnv (fn id => fn {qualid,...} => fn tr_ME_FE_VE => 
				  if isGlobalName qualid then
				      tr_ME_FE_VE
				  else (translateLocalAccess ModId env id):: tr_ME_FE_VE)
                                 tr_FE_VE 
				 ME                     
      in
          envelope' (Lstruct tr_ME_FE_VE)
      end
  | (LONGmodexp ii, _) =>
       trVar env ii
  | (CONmodexp (modexp',sigexp), SOME (EXISTSexmod(_,M'))) =>
        trConstrainedModExp env modexp' M'
  | (ABSmodexp (modexp',sigexp), SOME (EXISTSexmod(_,M'))) =>
        trConstrainedModExp env modexp' M'
  | (LETmodexp (dec,modexp),SOME _) =>
      let val ((rho', depth'), envelope) = trDec env dec
          val env'' = (plusEnv rho rho', depth')
      in envelope(trModExp env'' modexp) end
  | (FUNCTORmodexp(_,(_,funid),ref FUNik,_,modexp),SOME _) =>
      Lfn(trModExp (bindInEnv rho (FunId funid) (Path_local depth),
                    depth+1) 
                   modexp)
  | (FUNCTORmodexp(_,(_,strid),ref STRik,_,modexp),SOME _) =>
      Lfn(trModExp (bindInEnv rho (ModId strid) (Path_local depth),
                    depth+1) 
                   modexp)
(*  | (APPmodexp (funmodexp,modexp), SOME _) =>
        (case funmodexp of 
             (_,(_,ref (SOME (EXISTSexmod(_,FUNmod(T,M,X)))))) =>
		     Lapply(trModExp env funmodexp,                                           
			    [trConstrainedModExp env modexp (normMod M)])
        | _ => fatalError "trModExp:2") *)
  | (APPmodexp (funmodexp,modexp), SOME _) =>
      (case funmodexp of 
	   (_,(_,ref (SOME (EXISTSexmod(_,FUNmod(T,M,X)))))) =>
	      (case (isSafeModExp funmodexp, isSafeModExp modexp) of
		   (true,_) =>
		       Lapply(trModExp env funmodexp,                                           
			      [trConstrainedModExp env modexp (normMod M)])
               |   (false,true) => 
		       Lapply(trModExp env funmodexp,                                           
			      [trConstrainedModExp env modexp (normMod M)])
               |  (false,false) => 
		       let val (rho,depth) = env 
		       in
		       Llet([trModExp env funmodexp,
			     trConstrainedModExp (rho,depth+ 1) modexp (normMod M)],
		             Lapply(Lvar 1, [Lvar 0]))                                        
		       end)
	 | _ => fatalError "trModExp:2")
  | (PARmodexp modexp,SOME _) => trModExp env modexp
(* cvr: unsafe version that works but doesn't check for definedness
  | (RECmodexp((_,strid),ref (SOME RS'),sigexp,modexp),
	      SOME (EXISTSexmod(_,STRmod RS)))=>
      Llet([Lprim(Pmakeblock(CONtag(refTag, 1)), [Lconst constUnit])],
	   Llet([trModExp (bindInEnv rho (ModId strid) (Path_son (0,Path_local depth)),depth+1) modexp],
		Lseq(Lprim(Psetfield 0,[Lvar 1,coerceRecStr (Lvar 0) RS RS']),
		     Lvar 0)))
*)
  | (RECmodexp((_,strid),ref (SOME RS'),sigexp,modexp),
	      SOME (EXISTSexmod(_,STRmod RS)))=>
      Llet([Lprim(Pmakeblock(CONtag(refTag, 1)), 
		  [Lprim(Pmakeblock(CONtag(0, 2)),[Lconst constUnit])])],
	   Llet([trModExp (bindInEnv rho (ModId strid) (Path_rec depth),depth+1)
		          modexp],
		Lseq(Lprim(Psetfield 0,
			   [Lvar 1,
			    (Lprim(Pmakeblock(CONtag(1, 2)),
				   [coerceRecStr (Lvar 0) RS RS']))]),
		     Lvar 0)))
  | (_,_) => fatalError "trModExp:3"
and trConstrainedModExp env (modexp as (_, (modexp',ref (SOME (EXISTSexmod ((_,M))))))) M' = 
    (case (modexp',M,M') of
	(DECmodexp dec,STRmod RS,STRmod RS') => 
	    let val (env', envelope') = trDec env dec
	    in
		envelope' (coerceDec env' RS RS')
	    end
      | _ =>  coerceMod (trModExp env modexp) M M')
  | trConstrainedModExp _ _ _ = fatalError "trConstrainedModExp"
and tr1ValBind onTop (env as (rho, depth)) (ValBind(ref pat, arg)) =
  let val (env', add_lets) = mkEnvOfPats depth [pat]
      val tr_arg = trExp env arg
      val m_env = (rho, depth+1)
      val loc = xLR pat
      fun envelope lam =
            Llet([tr_arg],
              translateMatch m_env (partial_let onTop loc) loc
                             [([pat], add_lets lam)])
  in (env', envelope) end

and trValBind onTop (env as (rho, depth)) = fn
    [] => ((NILenv, depth), fn lam => lam)
  | [vb] =>
      tr1ValBind onTop env vb
  | vb :: vbs =>
      let val (env' as (rho', depth'),  envelope') =
             tr1ValBind onTop env vb
          val (env'' as (rho'', depth''), envelope'') =
             trValBind onTop (rho, depth') vbs
      in ((plusEnv rho' rho'', depth''), envelope' o envelope'') end

and trRecValBind (env as (rho, depth)) = fn
    [] => ((NILenv, depth), fn lam => lam)
  | vbs =>
      let val pats = map (fn ValBind(ref p, _) => p) vbs
          val args = map (fn ValBind(_, e) => e) vbs
          val (rho', depth') = mkEnvOfRecPats depth pats
	  val rho'' = mkHashEnv (length pats) rho'
          val new_env = (plusEnv rho rho'', depth')
          val tr_bindings = map (trExp new_env) args
          fun envelope lam = Lletrec(tr_bindings, lam)
      in ((rho'', depth'), envelope) end

and trMatch loc (env as (rho, depth)) failure_code mrules =
  let val m_env = (rho, depth + curriedness mrules)
      fun trMRule (MRule(ref pats, exp)) =
        let val ((rho', depth'), add_lets) = mkEnvOfPats depth pats
            val new_env = (plusEnv rho rho', depth')
        in (pats, add_lets (trExp new_env exp)) end
  in translateMatch m_env failure_code loc (map trMRule mrules) end

and trLetArgs (env as (rho, depth)) = fn
    [] =>  []
  | exp :: exps =>
      trExp env exp :: trLetArgs (rho, depth+1) exps

and trBindings (env as (rho, depth)) = fn
    [] => []
  | (pat, exp) :: rest =>
      trExp env exp :: trBindings (rho, depth+1) rest

and trExBindList (env as (rho, depth)) ebs =
  let val id_path_list =
        mapFrom (fn depth =>
                 fn
                    (EXDECexbind(ii, _))   =>
                              (hd(#id(#qualid ii)), Path_local depth)
                  | (EXEQUALexbind(ii, _)) =>
                              (hd(#id (#qualid ii)), Path_local depth))
                depth ebs
      and len = List.length ebs
      and args = mapFrom (fn i => fn eb => trExBind (rho, i) eb) depth ebs
      val rho' = foldR (fn (id, path) => fn rho => bindInEnv rho (ValId id) path)
                       NILenv id_path_list
  in ((rho', depth+len), fn lam => Llet(args, lam)) end

and trExBind env = fn
    EXDECexbind(ii, _) =>
      let val uname = ATOMsc(STRINGscon(currentUnitName()))
          val exid  = ATOMsc(STRINGscon (hd(#id (#qualid ii))))
          val en = exid (* ps: TODO: BLOCKsc(CONtag(0,1), [exid, uname]) *)
      in Lprim(Pmakeblock(CONtag(refTag, 1)), [Lconst en]) end
  | EXEQUALexbind(ii, ii') =>
      translateExName env ii';

(* Translation of toplevel declarations *)

fun makeSeq f [] = Lunspec
  | makeSeq f [x] = f x
  | makeSeq f (x::rest) = Lseq(f x, makeSeq f rest)
;

fun lookupLocalRenEnv asId renEnv id =
  let val mangled_id = mangle (asId id)
  in 
    mkUniqueGlobalName (mangled_id, lookup mangled_id renEnv)
    handle Subscript => fatalError "lookupLocalRenEnv"
  end
;

fun storeGlobal asId renEnv env var =
  Lprim(Pset_global (lookupLocalRenEnv asId renEnv var),
          [translateLocalAccess asId env var]) 
;



fun equGlobal asId  renEnv var0 var =
  Lprim(Pset_global (lookupLocalRenEnv asId renEnv var),
    [Lprim(Pget_global (lookupLocalRenEnv asId renEnv var0), [])])
;

fun tr1ToplevelRecValBind renEnv rho = fn
    ([], exp) => Lunspec
  | ([var], exp) =>
      Lprim(Pset_global (lookupLocalRenEnv ValId renEnv var), [trExp (rho, 0) exp])
  | (var :: vars, exp) =>
      Lseq(Lprim(Pset_global (lookupLocalRenEnv ValId renEnv var),
                 [trExp (rho, 0) exp]),
        makeSeq (equGlobal ValId renEnv var) vars)
;

fun revWithoutDuplicates [] acc = acc
  | revWithoutDuplicates (x :: xs) acc =
      if member x acc then
        revWithoutDuplicates xs acc
      else
        revWithoutDuplicates xs (x :: acc)
;

datatype TopLambda =
    NILtlam
  | SEQtlam of TopLambda * TopLambda
  | LAMtlam of bool * Lambda
;

fun flattenTLam tlam acc =
  case tlam of
      NILtlam => acc
    | SEQtlam(tlam1, tlam2) =>
        flattenTLam tlam1 (flattenTLam tlam2 acc)
    | LAMtlam(is_pure, lam) => (is_pure, lam) :: acc
;

fun trToplevelDec rho (dec as (_, dec')) =
  case dec' of
      VALdec (_, ([ValBind(ref (_,VARpat ii), exp)], [])) =>
        let val id = hd(#id(#qualid ii))
            val id' = mkUniqueGlobalName (renameId (mangle (ValId id)))
        in
          (mk1Env (ValId id) (Path_global id'),
            LAMtlam(isSafe exp,
              Lprim(Pset_global id', [trExp (rho, 0) exp])))
        end
    | VALdec (_, ([], rvbs)) =>
        let val ves = map (fn ValBind(ref p, e) => (domPat p, e)) rvbs
            val vars = foldL (fn (vs, _) => fn acc => vs @ acc) [] ves
            val renEnv = map (renameId o mangle o ValId) vars
            val rho' =
              foldR (fn (vid,(id' as (id, _))) => fn rho =>
                       bindInEnv rho (ValId vid) (Path_global (mkUniqueGlobalName id')))
                    NILenv (zip2 vars renEnv)
	    val rho'' = mkHashEnv (length vars) rho'
        in
          (rho'',
           LAMtlam(true,
             makeSeq (tr1ToplevelRecValBind renEnv (plusEnv rho rho'')) ves))
        end
    | VALdec (_, (pvbs, rvbs)) =>
        let val ((rho', depth'), envelope) =
               trValDec true (rho, 0) pvbs rvbs
            val vars = foldEnv (fn (ValId id) => (fn _ => fn vars => id :: vars)
				|  _ => fatalError "trTopLevelDec:1") 
		               [] 
			       rho'
	    val n = length vars
	    val rho'' = mkHashEnv n rho'
            val renEnv = map (renameId o mangle o ValId) vars
	    val renrho = 
		foldR (fn (vid,(id' as (id,_))) => fn rho =>
		       bindInEnv rho (ValId vid) (Path_global (mkUniqueGlobalName id')))
		      NILenv (zip2 vars renEnv)
        in
          (mkHashEnv n renrho,
           LAMtlam(
             all (fn ValBind(_, e) => isSafe e) pvbs,
             envelope (makeSeq (storeGlobal ValId renEnv (rho'', depth'))
                               (revWithoutDuplicates vars []))))
        end
    | PRIM_VALdec _ => (NILenv, NILtlam)
    | FUNdec (ref (UNRESfundec _)) => fatalError "trToplevelDec:2"
    | FUNdec (ref (RESfundec dec)) => trToplevelDec rho dec
    | TYPEdec _ => (NILenv, NILtlam)
    | PRIM_TYPEdec _ => (NILenv, NILtlam)
    | DATATYPEdec(dbs, _) => (NILenv, NILtlam)
    | DATATYPErepdec _ => (NILenv, NILtlam)
    | ABSTYPEdec(dbs, _, dec2) =>
        trToplevelDec rho dec2
    | EXCEPTIONdec mbs => 
        let val ((rho',depth'), envelope) = trExBindList (rho,0) mbs 
            val vars = foldEnv (fn (ValId id) => (fn _ => fn vars => id :: vars)
				| _ => fatalError "trToplevelDec:3") 
		               [] 
			       rho'
	    val n = length vars
	    val rho'' = mkHashEnv n rho'
            val renEnv = map (renameId o mangle o ValId) vars
	    val renrho = 
		foldR (fn (eid,(id' as (id,_))) => fn rho =>
		       bindInEnv rho (ValId eid) (Path_global (mkUniqueGlobalName id')))
		      NILenv (zip2 vars renEnv)
        in
          (mkHashEnv n renrho,
           LAMtlam(
             false, (* cvr: TODO I don't think this can be safe coz it might create new references *)
             envelope (makeSeq (storeGlobal ValId renEnv (rho'', depth'))
                               (revWithoutDuplicates vars []))))
        end
    | LOCALdec(dec1,dec2) =>
        let val (rho' , tlam')  = trToplevelDec rho dec1
            val (rho'', tlam'') = trToplevelDec (plusEnv rho rho') dec2
        in (rho'', SEQtlam(tlam', tlam'')) end
    | OPENdec longmodidinfos => 
        let val ((rho',depth'), envelope) = 
	      trOpenLongModIdInfos (rho,0) longmodidinfos
            val vars = foldEnv (fn Id => fn _ => fn vars => Id :: vars) [] rho'
	    val n = length vars
	    val rho'' = mkHashEnv n rho'
            val renEnv = map (renameId o mangle) vars
	    val renrho = 
		foldR (fn (Id,(id' as (id,_))) => fn rho =>
		       bindInEnv rho Id (Path_global (mkUniqueGlobalName id')))
		      NILenv (zip2 vars renEnv)
        in
          (mkHashEnv n renrho,
           LAMtlam(
             true,
             envelope (makeSeq (fn ValId vid => storeGlobal ValId renEnv (rho'', depth') vid
				|  ModId mid => storeGlobal ModId renEnv (rho'', depth') mid
				|  FunId fid => storeGlobal FunId renEnv (rho'', depth') fid)
                               (revWithoutDuplicates vars []))))
        end
    | EMPTYdec => (NILenv, NILtlam)
    | SEQdec(dec1,dec2) =>
        let val (rho' , tlam')  = trToplevelDec rho dec1
            val (rho'', tlam'') = trToplevelDec (plusEnv rho rho') dec2
        in (plusEnv rho' rho'', SEQtlam(tlam', tlam'')) end
    | FIXITYdec  _ =>  (NILenv, NILtlam)
    | STRUCTUREdec mbs => 
        let val ((rho',depth'), envelope) = trModBindList (rho,0) mbs 
            val vars = foldEnv (fn (ModId id) => (fn _ => fn vars => id :: vars)
				| _ => fatalError "trToplevelDec:4") [] rho'
	    val n = length vars
	    val rho'' = mkHashEnv n rho'
            val renEnv = map (renameId o mangle o ModId) vars
	    val renrho = 
		foldR (fn (mid,(id' as (id,_))) => fn rho =>
		       bindInEnv rho (ModId mid) (Path_global (mkUniqueGlobalName id')))
		      NILenv (zip2 vars renEnv)
        in
          (mkHashEnv n renrho,
           LAMtlam(
             all (fn MODBINDmodbind(_, modexp) => isSafeModExp modexp 
	           | ASmodbind(_,_,exp) => isSafe exp) 
		 mbs,
             envelope (makeSeq (storeGlobal ModId renEnv (rho'', depth'))
                               (revWithoutDuplicates vars []))))
        end
    | FUNCTORdec fbs => 
        let val ((rho',depth'), envelope) = trFunBindList (rho,0) fbs 
            val vars = foldEnv (fn (FunId id) => (fn _ => fn vars => id :: vars)
				| _ => fatalError "trToplevelDec:5") [] rho'
	    val n = length vars
	    val rho'' = mkHashEnv n rho'
            val renEnv = map (renameId o mangle o FunId) vars
	    val renrho = 
		foldR (fn (funid,(id' as (id,_))) => fn rho =>
		       bindInEnv rho (FunId funid) (Path_global (mkUniqueGlobalName id')))
		      NILenv (zip2 vars renEnv)
        in
          (mkHashEnv n renrho,
           LAMtlam(
             all (fn FUNBINDfunbind (_,modexp) => isSafeModExp modexp 
	           | ASfunbind(_,_,exp) => isSafe exp)
		 fbs,         
             envelope (makeSeq (storeGlobal FunId renEnv (rho'', depth'))
                               (revWithoutDuplicates vars []))))
        end
    | SIGNATUREdec  _ =>  (NILenv, NILtlam)
;

fun REofRho1 id (Path_global (_, stamp)) re = (mangle id, stamp) :: re
  | REofRho1 _  _            _              = fatalError "REofRho1"

fun REofRho rho =
  foldEnv REofRho1 [] rho
;

fun translateToplevelDec dec =
  let val (rho, tlam) = trToplevelDec NILenv dec
  in (REofRho rho, flattenTLam tlam []) end
;

