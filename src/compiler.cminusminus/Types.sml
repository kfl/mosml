open Misc List Fnlib Mixture 
     Config 
     Const Smlprim Globals Location Units;

(* cvr: operations on semantic structures *)

fun SofRecStr RS =
    case RS of
      RECrec(RS,RS') => SofRecStr RS'
    | NONrec S => S   
;

fun MEofStr (STRstr (ME,_,_,_,_)) = ME
  | MEofStr (SEQstr (Str1,Str2)) = plusEnv (MEofStr Str1) (MEofStr Str2)

fun FEofStr (STRstr (_,FE,_,_,_)) = FE
  | FEofStr (SEQstr (Str1,Str2)) = plusEnv (FEofStr Str1) (FEofStr Str2)

fun GEofStr (STRstr (_,_,GE,_,_)) = GE
  | GEofStr (SEQstr (Str1,Str2)) = plusEnv (GEofStr Str1) (GEofStr Str2)

fun TEofStr (STRstr (_,_,_,TE,_)) = TE
  | TEofStr (SEQstr (Str1,Str2)) = plusEnv (TEofStr Str1) (TEofStr Str2)

fun VEofStr (STRstr (_,_,_,_,VE)) = VE
  | VEofStr (SEQstr (Str1,Str2)) = plusEnv (VEofStr Str1) (VEofStr Str2)

fun removeGEofStr S =
    case S of 
	STRstr (ME,FE,NILenv,TE,VE) => S (* share if possible *)
      | STRstr (ME,FE,GE,TE,VE) => STRstr (ME,FE,NILenv,TE,VE)
      | SEQstr (S1,S2) => SEQstr (removeGEofStr S1,removeGEofStr S2) (* cvr: TODO improve sharing? *)
;

fun removeGEofRecStr RS = 
    case RS of
       RECrec(_,_) => RS
    |  NONrec S => NONrec (removeGEofStr S)
;

fun VEofCE (ConEnv CE : ConEnv) =
    foldL (fn cs => fn env =>
           let val {qualid, info} = cs
           in bindInEnv env (hd(#id qualid)) 
                   {qualid = qualid, 
                    info = (#conType (!info),CONname info)} end)
           NILenv 
	   CE
  | VEofCE _ = fatalError "VEofCE"
;


(* cvr: paths locate errors that occur during signature matching *)
datatype path = NILpath
              | IDpath of string 
              | DOTpath of path * string
              | DOMpath of path 
              | RNGpath of path 
              | UNITpath 
;

datatype ScopeViolation = 
    TYNAMEsv of TyName
  | TYPEVARsv of TypeVar;
(* unification *)
datatype reason = 
    UnifyCircular | UnifyEquality | UnifyExplicit
  | UnifyTup | UnifyRec of Lab | UnifyOther
  | UnifyMod of matchReason option * matchReason option
  | UnifyScope of TypeVar * ScopeViolation
and matchReason = 
    MissingValue of path * string * VarInfo
|   MissingStructure of path * string * ModInfo
|   MissingFunctor of path * string * FunInfo
|   MissingSignature of path * string * SigInfo
|   MissingType of path * string * TyInfo
|   MissingInfixStatus of path * string * InfixStatus
|   InfixStatusMismatch of path * string * InfixStatus * InfixStatus
|   SignatureMismatch of path * string * SigInfo * SigInfo * matchReason option * matchReason option
|   SchemeMismatch of path * string * VarInfo * VarInfo
|   StatusMismatch of path * string * VarInfo * VarInfo
|   ConEnvMismatch of  path * string * TyInfo * TyInfo
|   ArityMismatch of path * string * TyInfo * TyInfo * int * int
|   RefEqualityMismatch of path * string * TyInfo * TyInfo
|   EqualityMismatch of path * string * TyInfo * TyInfo
|   TransparentMismatch of path * string * TyInfo * TyInfo
|   PatternMismatch of path * string * TyStr * TyStr * TyName * ScopeViolation
|   CircularMismatch of path * string * TyStr * TyStr * TyName
|   DatatypeMismatch of path * string * TyInfo * TyInfo
|   ModuleMismatch  of path * string * string
                     (* path * infDesc * specDesc *)
;

exception Unify of reason;

exception MatchError of matchReason;

local
 (* cvr: REVISE?*)
 (* fun mktyname qual name info =
    {qualid={qual=qual, id=[name]}, info=ref info}  :  TyName;  *)
  fun mktyname qual name info =
    {qualid={qual="General", id=[name]}, info=ref info}  :  TyName; 
  fun mkSML name info =
    mktyname "General" name info;
  val mkSMLStamp = 
      let val next_stamp = ref 0
      in fn () => (incr (next_stamp);
		   ("General",!next_stamp))
      end
in
(* Some predefined type names *)
val tyname_unit = mkSML "unit"      
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_bool = mkSML "bool"      
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_char = mktyname "Char" "char" 
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_exn = mkSML "exn" 
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=FALSEequ, tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_frag = mkSML "frag"
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 1, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_int = mktyname "Int" "int" 
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_list = mkSML "list"  
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 1, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_option = mkSML "option"
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 1, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_order = mkSML "order"
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_ppstream = mkSML "ppstream"  
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=FALSEequ, tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_real = mktyname "Real" "real" 
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_ref = mkSML "ref"
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 1, tnEqu=REFequ,   tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_string = mktyname "String" "string" 
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
(* cvr: REVISE hardwiring Substring.substring and the other type names seems
   a horrible hack - can't
   we simplify this? *)
and tyname_substring = mktyname "Substring" "substring" 
    {tnStamp=("Substring",1),  tnKind=ARITYkind 0, tnEqu=FALSEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_syserror = mktyname "OS" "syserror" 
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=FALSEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_vector = mkSML "vector" 
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 1, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_word = mktyname "Word" "word" 
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
and tyname_word8 = mktyname "Word8" "word8" 
    {tnStamp=mkSMLStamp(),  tnKind=ARITYkind 0, tnEqu=TRUEequ,  tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE}
val tyname_bogus =
  mkSML "bogus" {tnStamp=mkSMLStamp(), tnKind=ARITYkind 0, tnEqu=TRUEequ, tnSort=PARAMETERts, tnLevel=0, tnConEnv=ref NONE};
end;

val type_bogus = CONt([], NAMEtyapp tyname_bogus);
val sc_bogus = TypeScheme{ tscParameters=[], tscBody=type_bogus };


fun mkConInfo () =
  ref{ conArity=(~1), conSpan=(~1),
       conIsGreedy=false,
       conTag=(~1), conType=sc_bogus }
;

fun mkExConInfo () = ref{ exconArity=(~1) };

fun mkPrimInfo arity prim =
  { primArity=arity, primOp=prim }
;

(* ps: fun isExConStatic (ei : ExConInfo) =
  case #exconTag(!ei) of
      SOME _ => true
    | NONE => false
;
*)

fun isNilRowType rho =
  case !rho of
      NILrow => true
    | VARrow _ => false
    | _ => fatalError "isNilRowType"
;

fun normalizeRecType (r: RecType) =
  case !(#rho(!r)) of
      NILrow => ()
    | VARrow _ => ()
    | LINKrow rho' =>
        (setRtRho r rho'; normalizeRecType r)
    | FIELDrow(lab, tau, rho') =>
        (setRtFields r (insertField (lab, tau) (#fields(!r)));
         setRtRho r rho'; normalizeRecType r)
;

(* Binding levels *)

val binding_level = ref 0;

fun resetBindingLevel() = binding_level := 0;
fun incrBindingLevel() = incr binding_level;
fun decrBindingLevel() = decr binding_level;
fun currentBindingLevel() = !binding_level; (* cvr: added *)

fun setCurrentBindingLevel isOverloaded = fn
    VARt var =>
      (setTvOvl var isOverloaded;
       setTvLevel var (!binding_level))
  | _ => fatalError "setCurrentBindingLevel"
;

fun newExplicitTypeVar syntaxTypeVar =
  let val tv = ref
    {
      tvKind = Explicit syntaxTypeVar,
      tvLevel = !binding_level,
      tvImp = !value_polymorphism,
      tvEqu = false,
      tvOvl = false
    }
  in
    (case explode syntaxTypeVar of
        #"'" :: #"'" :: #"_" :: _ =>
          (setTvEqu tv true; setTvImp tv true)
      | #"'" :: #"'" :: _ =>
          setTvEqu tv true
      | #"'" :: #"_" :: _ =>
          setTvImp tv true
      | _ => ());
    tv
  end;

fun mkTypeVar equ imp ovl level = ref
{
  tvKind = NoLink,
  tvEqu = equ,
  tvImp = !value_polymorphism orelse imp,
  tvOvl = ovl,
  tvLevel = level
};

fun newTypeVar equ imp ovl =
  mkTypeVar equ imp ovl (!binding_level)
;

fun newTypeVars 0 = []
  | newTypeVars n = newTypeVar false false false :: newTypeVars (n-1)
;

fun refreshTypeVar (tv : TypeVar) =
  let val {tvEqu, tvImp, tvOvl, ...} = !tv
  in newTypeVar tvEqu tvImp tvOvl end
;


fun isExplicit (tv : TypeVar) =
  let val {tvKind, ...} = !tv in
    case tvKind of
        Explicit _ => true
      | _ => false
  end;

fun newUnknown() =
  VARt (newTypeVar false false false)
;

(* cvr: added *)

(* type parameters *)
fun mkTypeParameter name equ imp ovl level = ref
{
  tvKind = Explicit name,
  tvEqu = equ,
  tvImp = !value_polymorphism orelse imp,
  tvOvl = ovl,
  tvLevel = level
};

fun newTypeParameter equ imp ovl =
  mkTypeParameter "" equ imp ovl (!binding_level)
;

fun newTypeParameters [] = []
  | newTypeParameters (h::t) = 
    ((VARt (newTypeParameter false (* cvr: should this be true? *) false false)
     ::
     newTypeParameters t))
;

fun refreshTypeParameter (tv : TypeVar) =
  let val {tvKind, tvEqu, tvImp, tvOvl, ...} = !tv
      val tvName = case tvKind of Explicit name => name | _ =>  ""
  in mkTypeParameter tvName tvEqu tvImp tvOvl (!binding_level) end
;

(* *)

fun TypeOfTypeVar tv = VARt tv;

fun freshNilRowType() = ref NILrow;

fun mkVarRowType equ imp level =
  ref (VARrow (ref {rvEqu=equ, rvImp=imp, rvLevel=level}))
;


fun kindTyName (tn:TyName) = #tnKind (!(#info tn));

fun kindTyApp tyapp =
    case tyapp of
      NAMEtyapp tn => kindTyName tn
    | APPtyapp (tyapp,app) => 
         (case kindTyApp tyapp of 
           ARROWkind(k,k') => k'
         | _ => fatalError "kindTyApp")
and kindTyFun tyfun =
    case tyfun of
      APPtyfun tyapp => kindTyApp tyapp
    | LAMtyfun (tn,tyfun) => 
           ARROWkind(kindTyName tn,kindTyFun tyfun)
    | TYPEtyfun (vs,tau) => ARITYkind (List.length vs)
;

fun equTyName (tn:TyName) = #tnEqu (!(#info tn));

fun etaExpandTyApp tyapp =
    case kindTyApp tyapp of 
	ARITYkind n => 
	    let val _ = incrBindingLevel();
		val vs = foldInt 
		          (fn n => fn vs =>
			   newTypeParameter false false false::vs) 
			  [] 
			  n
	    in  decrBindingLevel();
		TYPEtyfun(vs,CONt(map TypeOfTypeVar  vs,tyapp))
	    end
      | _ => fatalError "etaExpandTyApp";
fun etaExpandTyFun tyfun = 
    case tyfun of 
	APPtyfun tyapp => etaExpandTyApp tyapp
      | TYPEtyfun _ => tyfun
      | LAMtyfun _ => fatalError "etaExpandTyFun";
      
(* cvr: new, optimized and highly dodgy copying *)

local 
fun restrictBns bns1 bns2 = 
    drop (fn (tn1,NAMEtyapp tn1') => 
	       exists (fn (tn2,NAMEtyapp tn2') =>
		           tn1 = tn2 andalso tn1' = tn2'
	               | _ => false) bns2
	  |  _ => false) bns1;
fun restrictBvs bvs1 bvs2 = 
    drop (fn (tv1,VARt tv1') => 
	       exists (fn (tv2,VARt tv2') =>
		           tv1 = tv2 andalso tv1' = tv2'
			| _ => false) bvs2
	  |  _ => false) bvs1;
fun copyAndRealiseTyName (tn:TyName) tyfun =  
    case tn of	
	{qualid,info = ref { tnKind,tnEqu,tnStamp,tnSort = _, tnLevel,tnConEnv}} =>
	    {qualid = qualid, 
	     info = ref {tnKind = tnKind,
			 tnEqu = tnEqu, 
			 tnSort = REAts tyfun, 
			 tnStamp = newTyNameStamp(),
			 tnLevel = tnLevel,
			 tnConEnv = ref NONE}}
;

fun copyEnv copyInfo bns bvs env = 
    case env of
      NILenv => (bns,bvs,true,env)
    | BNDenv(k, info, env') =>
	  let val (bns,bvs,sinfo,cinfo) = copyInfo bns bvs info
	      val (bns,bvs,senv',cenv') = copyEnv copyInfo bns bvs env'
	  in
		if sinfo andalso senv' 
		    then (bns,bvs,true,env)
		else (bns,bvs,false,BNDenv(k,cinfo,cenv'))
	  end
    | COMPenv(env1, env2) =>
	  let val (bns,bvs,senv1,cenv1) = copyEnv copyInfo bns bvs env1
	      val (bns,bvs,senv2,cenv2) = copyEnv copyInfo bns bvs env2
	  in
	      if senv1 andalso senv2 
		  then (bns,bvs,true,env)
	      else (bns,bvs,false,COMPenv(cenv1,cenv2))
	  end
    | TOPenv(t, env') => 
	let val ct = Hasht.new 17
	    val (bns,bvs,st) = 
		Hasht.fold 
		(fn k => fn info => fn (bns,bvs,st) => 
		 let val (bns,bvs,sinfo,cinfo) = copyInfo bns bvs info
		 in
		     if sinfo 
		     then (Hasht.insert ct k cinfo;
			   (bns,bvs,st))
		     else (Hasht.insert ct k cinfo;
			  (bns,bvs,false))
		 end)
		(bns,bvs,true) t
	    val ct = if st then t else ct
	    val (bns,bvs,senv',cenv') = copyEnv copyInfo bns bvs env'
	in
	    if st andalso senv'
	    then (bns,bvs,true,env)
	    else (bns,bvs,false,TOPenv(ct,cenv'))
	end
;

fun copyGlobal copyInfo bns bvs (global as {qualid,info}) =  
	    let val (bns,bvs,sinfo,cinfo) = copyInfo bns bvs info
	     in
		 if sinfo 
		 then (bns,bvs,true,global)
		 else (bns,bvs,false,{qualid = qualid,info = cinfo})
	     end
;

fun copyTyNameSet tnSort bns bvs T =  
  let val bns' = 
         map (fn tn =>
	        case tn of
		    {qualid,info = ref {tnKind,tnEqu,tnStamp,tnSort = _,tnLevel,tnConEnv}} =>
                     let val tn' = {qualid = qualid, 
				    info = ref {tnKind = tnKind,
						tnEqu = tnEqu, 
						tnSort = tnSort, 
						tnStamp = newTyNameStamp(),
						tnLevel= !binding_level,
						tnConEnv = ref NONE}}
		     in (tn, NAMEtyapp tn')
		     end) 
	 T
      val bns'' = bns'@bns
      fun copyConEnvs bns bvs [] = (bns,bvs,[])
	| copyConEnvs bns bvs ((tn,NAMEtyapp tn')::bns') =
	   (case !(#tnConEnv (!(#info tn))) of
		NONE => 
		    let 
			val (bns,bvs,T') = copyConEnvs bns bvs bns'
		    in
			(bns,bvs,tn'::T')
		    end
	      | SOME conenv => 
		    let val (bns,bvs,sconenv,cconenv) = 
			    copyConEnv bns bvs conenv
			val (bns,bvs,T') = copyConEnvs bns bvs bns'
		    in
			if sconenv 
		        then setTnConEnv (#info tn') (#tnConEnv(!(#info tn)))
			else setTnConEnv (#info tn') (ref (SOME cconenv));
			(bns,bvs,tn'::T')
		    end)
         | copyConEnvs _ _ _ = fatalError "copyConEnvs"
       val (bns,bvs,T') = copyConEnvs bns'' bvs bns'
   in
        (restrictBns bns bns',bvs,T',bns')
   end
and copyTyName tnSort bns bvs tn =  
    case copyTyNameSet tnSort bns bvs [tn] of
      (bns,bvs,[tn'],[tn2tn']) =>
	(bns,bvs,tn',tn2tn')
    | _ => fatalError "copyTyName"
and copyType bns bvs tau = 
    case tau of
	VARt var =>
	    (let val ctau = lookup var bvs 
		 val stau = case ctau of 
		                VARt var' => var = var' 
			     |  _ => false
	     in
		 (bns,bvs,stau,ctau)
	     end
	     handle Subscript => 
		 (case #tvKind(!var) of
		      NoLink => (bns,bvs,true,tau)
		    | Explicit s => (bns,bvs,true,tau)
		    | LinkTo tau' =>
			  let val (bns,bvs,shared,tau'') = 
			      copyType bns bvs tau' 
			  in
 			      if shared then (bns,(var,tau)::bvs,true,tau)
			      else (bns,(var,tau'')::bvs,false,tau'')
			  end))
      | ARROWt(t,u) =>
	    let val (bns,bvs,st,ct) = copyType bns bvs t
		val (bns,bvs,su,cu) = copyType bns bvs u
	    in
		if st andalso su 
		    then (bns,bvs,true,tau)
		else (bns,bvs,false,ARROWt(ct,cu))
	    end
      | CONt(ts, tyapp) =>
	    let val (bns,bvs,sts,cts) = copyTypeList bns bvs ts
		val (bns,bvs,styapp,ctyapp) = copyTyApp bns bvs tyapp
	    in 
		if sts andalso styapp 
		    then  (bns,bvs,true,tau)
		else (bns,bvs,false,CONt(cts,ctyapp))
	    end
      | RECt (ref {fields, rho = rowtype}) =>
	    let val (bns,bvs,sfields,cfields) = copyFields bns bvs fields
	    in 
		if sfields then (bns,bvs,true,tau)
		else (bns,bvs,false,RECt (ref {fields = cfields,rho = rowtype}))
	    end
      | PACKt X => 
	    let val (bns,bvs,sX,cX) = copyExMod bns bvs X
	    in
		if sX then (bns,bvs,true,tau)
		else (bns,bvs,false,PACKt cX)
	    end
and copyTypeList bns bvs (ts as []) = (bns,bvs,true,ts)
|   copyTypeList bns bvs (tts as (t::ts)) = 
    let val (bns,bvs,st,ct)= copyType bns bvs t
	val (bns,bvs,sts,cts) = copyTypeList bns bvs ts
    in  
	if st andalso sts 
	    then (bns,bvs,true,tts)  
	else (bns,bvs,false,ct::cts)
    end
and copyFields bns bvs (fields as []) = (bns,bvs,true,fields)
|   copyFields bns bvs (fields as ((field as (lab,t))::fields')) = 
    let val (bns,bvs,st,ct)= copyType bns bvs t
	val (bns,bvs,sfields',cfields') = copyFields bns bvs fields'
    in  
	if st andalso sfields' 
	    then (bns,bvs,true,fields)    
	else (bns,bvs,false,(lab,ct)::cfields')

    end
and copyTypeScheme bns bvs (scheme as TypeScheme {tscParameters=vs,tscBody=ty})  = 
        let val _ = incrBindingLevel()
            val vs' = map refreshTypeParameter vs 
	    val bvs' = zip2 vs (map VARt vs')
	    val (bns,bvs,sty,cty) = copyType bns (bvs'@bvs) ty
	    val bvs = restrictBvs bvs bvs' 
        in   
	    decrBindingLevel();
            if sty 
	    then (bns,bvs,true,scheme)
	    else (bns,bvs,false,TypeScheme{tscParameters = vs',tscBody = cty})
        end
and copyTyApp bns bvs tyapp  =  
    case tyapp of
       NAMEtyapp tyname => 
         (let val ctyapp = lookup tyname bns
	  in
		 (bns,bvs,false,ctyapp)
	  end
          handle Subscript =>
             (case #tnSort(!(#info(tyname))) of
		  VARIABLEts => (bns,bvs,true,tyapp)
		| PARAMETERts => (bns,bvs,true,tyapp)
		| REAts tyfun =>
		      let 
			  val (bns,bvs,styfun,ctyfun) = 
			      copyTyFun bns bvs tyfun 
		      in
			  if styfun then
			      let val ctyname = 
				      copyAndRealiseTyName tyname tyfun
				  val ctyapp = NAMEtyapp ctyname
			      in ((tyname,ctyapp)::bns,bvs,false,ctyapp)
			      end 
			  else
			      let val ctyname = 
				      copyAndRealiseTyName tyname ctyfun
				  val ctyapp = NAMEtyapp ctyname
			      in ((tyname,ctyapp)::bns,bvs,false,ctyapp)
			      end
		      end))
    |  APPtyapp (tyapp',tyfun) =>  
	    let val (bns,bvs,styapp',ctyapp') = copyTyApp bns bvs tyapp'
		val (bns,bvs,styfun,ctyfun) = copyTyFun bns bvs tyfun
	    in
		if styapp' andalso styfun 
		    then (bns,bvs,true,tyapp)
		else (bns,bvs,false,APPtyapp(ctyapp',ctyfun))
	    end
and copyTyFun bns bvs tyfun =
    case tyfun of 
       TYPEtyfun (vs,ty) => 
        let val _ = incrBindingLevel()
            val vs' = map refreshTypeParameter vs 
	    val bvs' = zip2 vs (map VARt vs')
	    val (bns,bvs,sty,cty) = copyType bns (bvs'@bvs) ty
	    val bvs = restrictBvs bvs bvs' 
        in   
	    decrBindingLevel();
            if sty 
	    then (bns,bvs,true,tyfun)
	    else (bns,bvs,false,TYPEtyfun(vs',cty))
        end
    |  LAMtyfun (tn,tyfun') =>
        let val () = incrBindingLevel ();
            val (bns,bvs,tn',tn2tn') = copyTyName PARAMETERts bns bvs tn
	    val (bns,bvs,styfun',ctyfun') = 
		copyTyFun (tn2tn'::bns) bvs tyfun' 
	    val bns = restrictBns bns [tn2tn']
        in   
            decrBindingLevel();
            if styfun'
	    then (bns,bvs,true,tyfun)
	    else (bns,bvs,false,LAMtyfun(tn',ctyfun'))
        end
    |  APPtyfun tyapp => 
        let val (bns,bvs,styapp,ctyapp) = copyTyApp bns bvs tyapp
        in   
            if styapp
	    then (bns,bvs,true,tyfun)
	    else (bns,bvs,false,APPtyfun ctyapp)
        end
and copyConInfo bns bvs (coninfo as ref { conArity, conIsGreedy, conSpan, conTag, conType}) = 
    let val (bns,bvs,sconType,cconType) = copyTypeScheme bns bvs conType
    in 
	if sconType 
	then (bns,bvs,true,coninfo)
	else (bns,
	      bvs,
	      false,
	      ref {conArity = conArity,
		   conIsGreedy = conIsGreedy,
		   conSpan = conSpan,
		   conTag = conTag,
		   conType = cconType})
    end
and copyConStatusDesc bns bvs csd =
    (case csd of 
      CONname coninfo => 
	  let val (bns,bvs,sconinfo,cconinfo) = copyConInfo bns bvs coninfo
	  in
	      if sconinfo 
	      then (bns,bvs,true,csd)
	      else (bns,bvs,false,CONname cconinfo)
	  end
    |   _ => (bns,bvs,true,csd))
and copyVarEnv bns bvs env = 
    copyEnv (copyGlobal 
	     (fn bns => fn bvs =>
	      fn (info as (scheme,status)) =>  
	      let val (bns,bvs,sscheme,cscheme) = copyTypeScheme bns bvs scheme
		  val (bns,bvs,sstatus,cstatus) = copyConStatusDesc bns bvs status
	      in
		  if sscheme andalso sstatus
		      then (bns,bvs,true,info)
		  else (bns,bvs,false,(cscheme,cstatus))
	      end))
    bns bvs env
and copyConBindList bns bvs (coninfos as []) =
    (bns,bvs,true,coninfos)
|   copyConBindList bns bvs (coninfos as (coninfo :: coninfos')) =
    let val (bns,bvs,sconinfo,cconinfo)= copyGlobal copyConInfo bns bvs coninfo
	val (bns,bvs,sconinfos',cconinfos') = copyConBindList bns bvs coninfos'
    in  
	if sconinfo andalso sconinfos' 
	    then (bns,bvs,true,coninfos)    
	else (bns,bvs,false,cconinfo::cconinfos')
    end
and copyConEnv bns bvs conenv =
    case conenv of
	ConEnv CE => 
	    let val (bns,bvs,sCE,cCE) = copyConBindList bns bvs CE
	    in 
		if sCE
		then (bns,bvs,true,conenv)
		else (bns,bvs,false,ConEnv cCE)
	    end
      | LAMconenv(tn,conenv') =>
        let val () = incrBindingLevel ();
            val (bns,bvs,tn',tn2tn') = copyTyName PARAMETERts bns bvs tn;
	    val (bns,bvs,sconenv',cconenv') = 
		copyConEnv (tn2tn'::bns) bvs conenv'
	    val bns = restrictBns bns [tn2tn']
        in   
            decrBindingLevel();
            if sconenv'
	    then (bns,bvs,true,conenv)
	    else (bns,bvs,false,LAMconenv(tn',cconenv'))
        end
and copyTyEnv bns bvs TE = 
    copyEnv (fn bns => fn bvs => 
	     fn (info as (tyfun,CE)) => 
	     let val (bns,bvs,styfun,ctyfun) = copyTyFun bns bvs tyfun
		 val (bns,bvs,sCE,cCE) = copyConEnv bns bvs CE
	     in
		 if styfun andalso sCE 
		     then (bns,bvs,true,info)
		 else (bns,bvs,false,(ctyfun,cCE))
	     end)
    bns bvs TE
and copyStr bns bvs S =
    case S of
       STRstr (ME,FE,GE,TE,VE) =>
	     let val (bns,bvs,sME,cME) = copyModEnv bns bvs ME
		 val (bns,bvs,sFE,cFE) = copyFunEnv bns bvs FE
		 val (bns,bvs,sGE,cGE) = copySigEnv bns bvs GE
		 val (bns,bvs,sTE,cTE) = copyTyEnv bns bvs TE
		 val (bns,bvs,sVE,cVE) = copyVarEnv bns bvs VE
	     in
	        if sME andalso sFE andalso sGE andalso sTE andalso sVE 
		then (bns,bvs,true,S) 
		else (bns,bvs,false,STRstr (cME,cFE,cGE,cTE,cVE))
	     end
     |  SEQstr (S1,S2) =>
	     let val (bns,bvs,sS1,cS1) = copyStr bns bvs S1
		 val (bns,bvs,sS2,cS2) = copyStr bns bvs S2
	     in
		 if sS1 andalso sS2
		     then (bns,bvs,true,S)
		 else (bns,bvs,false,SEQstr(cS1,cS2))
	     end
and copyRecStr bns bvs RS =
    case RS of
       RECrec (RS1,RS2) =>
	     let val (bns,bvs,sRS1,cRS1) = copyRecStr bns bvs RS1
		 val (bns,bvs,sRS2,cRS2) = copyRecStr bns bvs RS2
	     in
		 if sRS1 andalso sRS2
		     then (bns,bvs,true,RS)
		 else (bns,bvs,false,RECrec(cRS1,cRS2))
	     end
    | NONrec S =>
	     let val (bns,bvs,sS,cS) = copyStr bns bvs S
	     in
		 if sS 
		     then (bns,bvs,true,RS)
		 else (bns,bvs,false,NONrec(cS))
	     end
and copyMod bns bvs M = 
    case M of
      STRmod RS => 
	  let val (bns,bvs,sRS,cRS) = copyRecStr bns bvs RS
	  in
             if sRS
	     then (bns,bvs,true,M) 
	     else (bns,bvs,false,STRmod cRS)
	  end		  
    | FUNmod F => 
	  let val (bns,bvs,sF,cF) = copyFun bns bvs F
	  in
             if sF
	     then (bns,bvs,true,M) 
	     else (bns,bvs,false,FUNmod cF)
	  end		
and copyModEnv bns bvs ME = 
    copyEnv (copyGlobal copyRecStr) bns bvs ME
and copyFunEnv bns bvs FE =
    copyEnv (copyGlobal copyFun) bns bvs FE
and copySigEnv bns bvs GE =
    copyEnv (copyGlobal copySig) bns bvs GE
and copyFun bns bvs (F as (T,M,X)) =
    let val () = incrBindingLevel ();
	val (bns,bvs,T',T2T') = copyTyNameSet PARAMETERts bns bvs T 
	val (bns,bvs,sM,cM) = copyMod ((T2T')@bns) bvs M
	val (bns,bvs,sX,cX) = copyExMod bns bvs X
	val bns = restrictBns bns T2T'
    in
        decrBindingLevel();	
	if sM andalso sX 
	    then (bns,bvs,true,F)
	else (bns,bvs,false,(T',cM,cX))
    end
and copyExMod bns bvs (X as EXISTSexmod(T,M)) = 
    let val () = incrBindingLevel ();
	val (bns,bvs,T',T2T') = copyTyNameSet PARAMETERts bns bvs T 
	val (bns,bvs,sM,cM) = copyMod ((T2T')@bns) bvs M
	val bns = restrictBns bns T2T'
    in
        decrBindingLevel();	
	if sM 
	then (bns,bvs,true,X)
	else (bns,bvs,false,EXISTSexmod(T',cM))
    end
and copySig bns bvs (G as LAMBDAsig(T,M)) = 
    let val () = incrBindingLevel ();
	val (bns,bvs,T',T2T') = copyTyNameSet PARAMETERts bns bvs T 
	val (bns,bvs,sM,cM) = copyMod ((T2T')@bns) bvs M
	val bns = restrictBns bns T2T'
    in
        decrBindingLevel();	
	if sM 
	then (bns,bvs,true,G)
	else (bns,bvs,false,LAMBDAsig(T',cM))
    end;
in
    val copyTyName = fn tnSort => fn  bns => fn bvs => fn  tn =>
	let val (_,_,tn',tn2tn') = copyTyName tnSort bns bvs tn
	in
	    (tn',tn2tn')
	end
    val copyTyNameSet = fn tnSort => fn bns => fn bvs => fn T =>
	let val (_,_,T',T2T') = copyTyNameSet tnSort bns bvs T
	in
	    (T',T2T')
	end
    val copyType = fn bns => fn bvs => fn t => 
	#4 (copyType bns bvs t)
    val copyTyFun = fn bns => fn bvs => fn tyfun => 
	#4 (copyTyFun bns bvs tyfun)
    val copyTyApp = fn bns => fn bvs => fn tyapp => 
	#4 (copyTyApp bns bvs tyapp)
    val copyConEnv = fn bns => fn bvs => fn conenv => 
	#4 (copyConEnv bns bvs conenv)
    val copyStr = fn bns => fn bvs => fn S => 
	#4 (copyStr bns bvs S)
    val copyRecStr = fn bns => fn bvs => fn S => 
	#4 (copyRecStr bns bvs S)
    val copyGenFun = fn bns => fn bvs => fn F => 
	#4 (copyFun bns bvs F) 
    val copyMod = fn bns => fn bvs => fn M => 
	#4 (copyMod bns bvs M)
    val copyExMod = fn bns => fn bvs => fn X => 
	#4 (copyExMod bns bvs X)
    val copySig = fn bns => fn bvs => fn G => 
	#4 (copySig bns bvs G)
end;

(* free variables (type names, type vars and rho vars) *)
(* cvr: TODO freevars currently done in reverse order for envs --- change this as it affects
   pretty printing*)
fun freeVarsType bns bvs (fnvs as (fns, fvs, frvs)) tau = 
          case normType tau of
                VARt var =>
                  if member var fvs orelse member var bvs 
                  then fnvs 
                  else (fns, var::fvs,frvs)
              | ARROWt(t,t') =>
                  (freeVarsType bns bvs (freeVarsType bns bvs fnvs t) t')
              | CONt(ts, tyapp) =>
                 freeVarsTyApp bns bvs (foldL (fn t => fn fnvs => freeVarsType bns bvs fnvs t) fnvs ts) tyapp 
              | RECt (ref{fields, rho = rowtype}) =>
                 freeVarsRowType bns bvs 
		 (foldL (fn (lab,ty) => fn fnvs => freeVarsType bns bvs fnvs ty) fnvs fields)
                 rowtype
              | PACKt X => freeVarsExMod bns bvs fnvs X  
and freeVarsRowType bns bvs (fnvs as (fns,fvs,frvs)) rowtype = 
    case !rowtype of
	 NILrow => fnvs 
      |  VARrow rowvar => 
	     if member rowvar frvs 
		 then fnvs 
	     else (fns,fvs,rowvar::frvs) 
      |  FIELDrow (lab,ty,rowtype) =>
		 freeVarsRowType bns bvs 
		 (freeVarsType bns bvs fnvs ty)
                 rowtype
      |  LINKrow rowtype => fatalError "freeVarRowType"
and freeVarsTypeScheme   bns bvs fnvs (TypeScheme {tscParameters,tscBody})  = 
    freeVarsType bns (tscParameters@bvs) fnvs tscBody
and freeVarsTyApp bns bvs (fnvs as (fns, fvs,frvs)) tyapp  = 
    case tyapp of
       NAMEtyapp tyname => 
(*                  if member tyname fns orelse member tyname bns  *)
                  if exists (isEqTN tyname) fns 
		      orelse exists (isEqTN tyname) bns
                  then fnvs 
                  else (tyname::fns,fvs,frvs)
    |  APPtyapp (tyapp,tyfun) => 
        freeVarsTyFun bns bvs (freeVarsTyApp bns bvs fnvs tyapp) tyfun
and freeVarsTyFun bns bvs fnvs tyfun =
    case normTyFun tyfun of 
       TYPEtyfun (vs,ty) => freeVarsType bns (vs@bvs) fnvs ty
    |  LAMtyfun (v,tyfun) => freeVarsTyFun (v::bns) bvs fnvs tyfun
    |  APPtyfun tyapp => freeVarsTyApp bns bvs fnvs tyapp
and freeVarsVarInfo bns bvs fnvs {qualid=_,info = (sc,_)} = freeVarsTypeScheme bns bvs fnvs sc
and freeVarsVarEnv bns bvs fnvs VE = 
       foldEnv (fn _ => fn info => fn fnvs => freeVarsVarInfo bns bvs fnvs info) fnvs  VE 
and freeVarsTyEnv bns bvs fnvs VE = 
       foldEnv (fn _ => fn tystr =>  fn fnvs => freeVarsTyStr bns bvs fnvs tystr) fnvs VE
and freeVarsTyStr bns bvs fnvs (tyfun,conenv) = 
      freeVarsConEnv bns bvs (freeVarsTyFun bns bvs fnvs tyfun) conenv
and freeVarsConEnv bns bvs fnvs conenv =
      case conenv of
	  ConEnv CE =>
	      foldL (fn {info=ref {conType,...},...} => 
		     fn fnvs => freeVarsTypeScheme bns bvs fnvs conType)
	             fnvs CE
      | LAMconenv(tn,conenv) =>
	    freeVarsConEnv (tn::bns) bvs fnvs conenv
and freeVarsStr  bns bvs fnvs S = 
    case S of 
       STRstr (ME,FE,GE,TE,VE) =>
        freeVarsModEnv bns bvs (freeVarsFunEnv bns bvs (freeVarsSigEnv bns bvs (freeVarsTyEnv bns bvs (freeVarsVarEnv bns bvs fnvs VE) TE) GE) FE) ME
    |  SEQstr (S,S') => 
        (freeVarsStr bns bvs (freeVarsStr bns bvs fnvs S) S')
and freeVarsRecStr  bns bvs fnvs RS = 
    case RS of
	RECrec (RS,RS') => 
	    (freeVarsRecStr bns bvs (freeVarsRecStr bns bvs fnvs RS) RS')
      | NONrec S => 
	    (freeVarsStr bns bvs fnvs S)
and freeVarsMod bns bvs fnvs M = 
    case M of
      STRmod S => freeVarsRecStr bns bvs fnvs S
    | FUNmod F => freeVarsGenFun bns bvs fnvs F
and freeVarsModInfo bns bvs fnvs {qualid=_,info = RS} = freeVarsRecStr bns bvs fnvs RS
and freeVarsModEnv bns bvs fnvs ME = 
       foldEnv (fn id => fn info => fn fnvs => freeVarsModInfo bns bvs fnvs info) fnvs ME
and freeVarsGenFun  bns bvs fnvs (T,M,X) = 
       let val bns' = T @ bns in
          (freeVarsExMod (bns') bvs (freeVarsMod bns' bvs fnvs M) X)
       end
and freeVarsFunInfo bns bvs fnvs {qualid=_,info = F} = freeVarsGenFun bns bvs fnvs F
and freeVarsSigInfo bns bvs fnvs {qualid=_,info = G} = freeVarsSig bns bvs fnvs G
and freeVarsFunEnv bns bvs fnvs FE = 
       foldEnv (fn id => fn info => fn fnvs => freeVarsFunInfo bns bvs fnvs info) fnvs FE
and freeVarsExMod  bns bvs fnvs (EXISTSexmod(T,M)) = freeVarsMod  (T@bns) bvs fnvs M
and freeVarsSig  bns bvs fnvs (LAMBDAsig(T,M)) = freeVarsMod  (T@bns) bvs fnvs M
and freeVarsSigEnv bns bvs fnvs GE =
       foldEnv (fn id => fn {qualid, info = G} => fn fnvs => freeVarsSig bns bvs fnvs G) fnvs GE
and freeVarsExEnv bns bvs fnvs (EXISTS(T,(ME,FE,GE,VE,TE))) = 
        freeVarsModEnv bns bvs (freeVarsFunEnv bns bvs (freeVarsSigEnv bns bvs (freeVarsTyEnv bns bvs (freeVarsVarEnv bns bvs fnvs VE) TE) GE) FE) ME

and apptycon tys tyfun = 
   case tyfun of 
    (* TYPEtyfun([],tau) => tau *)
     TYPEtyfun(tvs,tau) => copyType [] (zip2 tvs tys) tau
   | APPtyfun tyapp => CONt (tys,tyapp)
   | _ => fatalError "apptycon"
and apptyfun tyfun tyfun' =
   case tyfun of

(*     LAMtyfun(tn,tyfun) => 
          let val () = incrBindingLevel ();
              val (tn',tn2tn') = copyTyName (REAts tyfun') [] [] tn;  
                (* cvr: rewrite using proper substitution? *)
              val tyfun'' = LAMtyfun(tn', copyTyFun [tn2tn'] [] tyfun)
          in 
              decrBindingLevel();
              tyfun'
          end
*)
     LAMtyfun(tn,tyfun) => 
          let val () = incrBindingLevel ();
              val (tn',tn2tn') = copyTyName (REAts tyfun') [] [] tn;  
                (* cvr: rewrite using proper substitution? *)
              val tyfun'' = copyTyFun [tn2tn'] [] tyfun
          in 
              decrBindingLevel();
              tyfun''
          end
   | APPtyfun tyapp => APPtyfun(APPtyapp(tyapp,tyfun'))
   | _ => fatalError "apptyfun"   
and type_subst UE tau = copyType [] UE tau
(*
  case normType tau of
      VARt var =>
        (lookup var UE
         handle Subscript => tau ) (* cvr: surely this is the correct behaviour? *)
         (* fatalError "type_subst: Unknown variable") *)
    | ARROWt(t,t') =>
        ARROWt(type_subst UE t, type_subst UE t')
    | CONt(ts, tn) =>
        CONt(map (type_subst UE) ts, tn) (* cvr: TODO *)
    | RECt (ref{fields, rho}) =>
        RECt (ref{fields=map_fields (type_subst UE) fields, rho=rho})*)
and normType tau =
  case tau of
      VARt var =>
        (case #tvKind(!var) of
             LinkTo tau' =>
               let val tau'' = normType tau' in
                 setTvKind var (LinkTo tau'');
                 tau''
               end
           |  _ => tau)
    | RECt r =>
        (normalizeRecType r; tau)
    | CONt (tys,tyapp) => 
       (* apptycon tys (normTyApp tyapp) not good enough *)
	(case normTyApp tyapp of
	    APPtyfun tyapp =>CONt(tys,tyapp)
          | tyfun => normType (apptycon tys tyfun))
    | _ => tau
and normTyApp tyapp =
  case tyapp of
      NAMEtyapp tn =>
        (case #tnSort (!(#info tn)) of
(*  cvr: TODO Review path compression invalidates the hack of forgetting realisations... because we identify shared type names by swinging pointers,
instead of copying. Which do we want?
              REAts tyfun => 
               let val tyfun' = normTyFun tyfun in
                   setTnSort (#info tn) (REAts tyfun');
                   tyfun'
               end
*)
              REAts tyfun => 
             	   normTyFun tyfun 
            | _ => APPtyfun tyapp
         )
      | APPtyapp (tyapp,tyfun) =>
	 (case normTyApp tyapp of
	    APPtyfun tyapp => APPtyfun (APPtyapp (tyapp,tyfun))
          | tyfun' => normTyFun (apptyfun tyfun' tyfun))
     (*                 (* normTyFun *) (apptyfun (normTyApp tyapp) tyfun) 
         (* cvr: TODO does the outer norm cause looping? *) *)
and normTyFun tyfun = 
  case tyfun of
    LAMtyfun _ => tyfun
  | APPtyfun tyapp => normTyApp tyapp
  | TYPEtyfun _ => tyfun;

(* cvr: normalizing a module type sorts the entries of its term components   
        so their field positions can be calculated correctly *)

fun normStr S = 
    STRstr (sortEnv (mapEnv normModBind (MEofStr S)),
            sortEnv (FEofStr S),
	    GEofStr S,
	    TEofStr S,
	    sortEnv (VEofStr S))
and normRecStr RS =
  case RS of
    NONrec S => NONrec (normStr S)
  | RECrec (RS,RS') => normRecStr RS'
and normMod M = 
  case M of
    STRmod S => STRmod (normRecStr S)
  | M => M
and normModBind  id {qualid, info = RS} = 
    {qualid = qualid,info = normRecStr  RS}
and normExMod (EXISTSexmod(T,M)) = EXISTSexmod(T,normMod M);

(* cvr: parameterization of T over P *)
fun parameteriseTyNameSet T P =  
  let val (T',T2T') = 
         foldR (fn  tn as {qualid,info = ref {tnKind,tnEqu,tnStamp,tnSort = _,tnLevel,tnConEnv}}:TyName =>
                fn (T',T2T') => 
	      let val tnKind' = foldR (fn p => fn k' => ARROWkind (kindTyName p,k')) tnKind P
		  val tnEqu' = foldR (fn p => fn k' => ARROWequ (equTyName p,k')) tnEqu P
		  val tn' = {qualid = {qual = "", id = #id qualid}, 
			     info = ref {tnKind = tnKind',
					 tnEqu = tnEqu', 
					 tnSort = PARAMETERts, 
					 tnStamp = newTyNameStamp(),
					 tnLevel= currentBindingLevel(),
					 tnConEnv = ref NONE}}
                  val tyapp = foldL (fn tn => fn tyapp => APPtyapp (tyapp,APPtyfun (NAMEtyapp tn))) (NAMEtyapp tn') P
	      in (tn'::T',(tn,tyapp)::T2T')
	      end)
          ([],[]) T 
      val _ =  app2 (fn tn => fn tn' => 
		     case !(#tnConEnv (!(#info tn))) of
			 NONE => ()
		       | SOME conenv => 
			     let val (P',P2P') = 
				      copyTyNameSet PARAMETERts [] [] P
				 val T2P2P'T'= 
				     map (fn (tn,tyapp)=> 
					  (tn,copyTyApp P2P' [] tyapp)) 
               				  T2T'
				 val conenv' = 
				     copyConEnv (P2P'@T2P2P'T') [] conenv
			     in
				 (#tnConEnv (!(#info tn'))) := 
				 SOME (foldR (fn p => fn conenv => LAMconenv(p,conenv))
				       conenv' 
				       P')
			     end)
			 T 
			 T' 
   in
       (T',T2T')
   end
;


fun stripTyApp tyfuns tyapp =
      case tyapp of 
	  NAMEtyapp tn => (tn,tyfuns)
	| APPtyapp (tyapp,tyfun) =>stripTyApp (tyfun::tyfuns) tyapp

local 
  fun stripConEnv conenv =
      case conenv of
	  ConEnv CE => ([],conenv)
	| LAMconenv (tn,conenv) =>
	      let val (tns,conenv) = stripConEnv conenv
	      in  (tn::tns,conenv) 
	      end
in

 fun conEnvOfTyApp tyapp =
     let val (tn,tyfuns) = stripTyApp [] tyapp
     in
	 case !(#tnConEnv (!(#info tn))) of
	     NONE => NONE
           | conenvopt as (SOME (ConEnv _)) => conenvopt (* cvr: needn't copy *)
	   | SOME conenv =>
		 let val conenv = copyConEnv [] [] conenv
		     val (tns,conenv) = stripConEnv conenv
		 in 
		    app2 (fn tn => fn tyfun => 
			  setTnSort (#info tn) (REAts tyfun))
		          tns 
			  tyfuns;
		    SOME conenv
		 end
     end

end
;		
	     

fun isTupleType tau = 
    case normType tau of
	RECt rt => let val {fields, rho} = !rt
		    in isNilRowType rho andalso isTupleRow fields end
      | _       => false;


(* Correct binding level will be set later, *)
(* during type-checking *)

fun fresh3DotType() = mkVarRowType false false ~1;

fun contentsOfRowTypeAcc acc rho =
  case !rho of
      NILrow => (acc, false)
    | VARrow _ => (acc, true)
    | LINKrow rho' =>
        contentsOfRowTypeAcc acc rho'
    | FIELDrow(lab, tau, rho')  =>
        contentsOfRowTypeAcc ((lab,tau)::acc) rho'
;

fun contentsOfRowType rho =
  contentsOfRowTypeAcc [] rho
;

fun extractLab r r' (lab: Lab) (rho: RowType) =
  case !rho of
      NILrow => 
	  if isTupleType (RECt r) andalso isTupleType (RECt r') then
	      raise Unify UnifyTup
	  else 
	      raise Unify (UnifyRec lab)
    | VARrow rv =>
        let val {rvEqu=equ, rvImp=imp, rvLevel=level, ...} = !rv
            val rho' = mkVarRowType equ imp level
            val tau' = VARt (mkTypeVar equ imp false level)
        in
          rho := (FIELDrow(lab, tau', rho'));
          (tau', rho')
        end
    | LINKrow _ => fatalError "extractLab"
    | FIELDrow _ => fatalError "extractLab"
;

fun occur_check var fvs =                     
      (* cvr: TODO it might be more efficient to proceed as above and only caculate free vars for PACKt *)
    app (fn  var' =>
          if var = var' then
            raise Unify UnifyCircular
          else ())
    fvs;

exception ScopeViolation of ScopeViolation;

fun prune_level max_level (fns,fvs,frvs) = 
   (app (fn (tn:TyName) =>
          if #tnLevel(!(#info tn)) > max_level then
               raise ScopeViolation (TYNAMEsv tn)
          else ()) fns;
    app (fn (tv:TypeVar) =>
          if #tvLevel(!tv) > max_level then
            (if isExplicit tv then
               raise ScopeViolation (TYPEVARsv tv)
             else
               setTvLevel tv max_level)
          else ()) fvs;
    app (fn (rv:RowVar) =>
        if #rvLevel(!rv) > max_level then
          setRvLevel rv max_level
        else ()) frvs)
;


fun assumingEqualityTypeVars tvs f a =
    let val tvRecords = 
 	  map (fn tv as (ref tvRecord) => (setTvEqu tv true;tvRecord)) tvs
	val res = f a
    in
         map2 (fn tv => fn tvRecord => tv := tvRecord) tvs tvRecords;
         res
    end
;

fun typeViolatesEquality tau =
  case normType tau of
      VARt tv => 
	  if isExplicit tv 
	      then not(#tvEqu(!tv))
	  else false
    | ARROWt _ => true
    | CONt(ts, tyapp) =>
        (case EqualityOfTyApp tyapp of
             FALSEequ => true
           | TRUEequ  => exists typeViolatesEquality ts
           | REFequ   => false
	   | _ => fatalError "typeViolatesEquality")
    | RECt (ref{fields, ...}) =>
        exists_field typeViolatesEquality fields
    | PACKt _ => true
and EqualityOfTyName tn = equTyName tn
and EqualityOfTyApp tyapp = 
  case tyapp of
    NAMEtyapp tn => EqualityOfTyName tn
  | APPtyapp (tyapp,tyfun) => 
      (case EqualityOfTyApp tyapp of 
            ARROWequ(equ,equ') => equ'
          | _ => fatalError "EqualityOfTyApp")
and EqualityOfTyFun tyfun = 
  case normTyFun tyfun of
    APPtyfun tyapp => EqualityOfTyApp tyapp
  | TYPEtyfun(tvs, ty)=>
     (case (tvs,normType ty) of
	  ([tv],CONt([VARt tv'],NAMEtyapp tn)) =>
	      if tv = tv' andalso isEqTN tyname_ref tn 
		  (* tyfun is an eta-expansion of ref *)
		  then REFequ
	      else 
		  assumingEqualityTypeVars tvs
		  (fn ty => 
		   if typeViolatesEquality ty 
		       then FALSEequ 
		   else TRUEequ) 
		  ty
	| (tvs,ty) => assumingEqualityTypeVars tvs
		  (fn ty => 
		   if typeViolatesEquality ty 
		       then FALSEequ 
		   else TRUEequ) 
		  ty)
  | LAMtyfun (tn,tyfun) => 
	ARROWequ(EqualityOfTyName tn,EqualityOfTyFun tyfun)
;


fun makeEqualityRho (rho: RowType) =
  case !rho of
      VARrow rv =>
        setRvEqu rv true
    | _ => ()
;

fun makeEquality t = (* cvr: TODO review for tyapp *)
  case normType t of
    VARt var =>
      if #tvEqu(!var) then ()
      else if isExplicit var then
        raise Unify UnifyEquality
      else
        setTvEqu var true
  | ARROWt(t1,t2) =>
      raise Unify UnifyEquality
  | CONt(ts, tyapp) =>
      (case EqualityOfTyApp tyapp of
           FALSEequ => raise Unify UnifyEquality
         | TRUEequ =>
             (app makeEquality ts 
	      (* cvr: TODO remove ;makeEqualityTyApp tyapp *)
	      )
         | REFequ => ()
	 | _ => fatalError "makeEquality")
   | RECt (ref {fields, rho}) =>
      (app_field makeEquality fields;
       makeEqualityRho rho)
   | PACKt _ => raise Unify UnifyEquality
;


fun makeImperativeRho rho =
  case !rho of
      VARrow rv =>
        setRvImp rv true
    | _ => ()
;

fun makeImperative (fnvs as (fns,fvs,frvs)) =
    (app (fn var =>
	  if #tvImp(!var) then ()
	  else if isExplicit var 
		   then raise Unify UnifyExplicit
	       else
		   setTvImp var true
		   ) fvs;
     app (fn rowvar =>
          setRvImp rowvar true
	  ) frvs);  

fun makeOverloaded t =
  case normType t of
    VARt var =>
      if #tvOvl(!var) then ()
      else if isExplicit var then
        raise Unify UnifyExplicit
      else
        setTvOvl var true
  | ARROWt _ => ()
  | CONt _ => ()
  | RECt _ => ()
  | PACKt _ => ()
;

fun updateAttributes (var : TypeVar) t fnvsOft =
  let val {tvEqu, tvImp, tvOvl, ...} = !var in
    if tvEqu then makeEquality t else ();
    if tvImp andalso not (!value_polymorphism) then makeImperative fnvsOft else ();
    if tvOvl then makeOverloaded t else ()
  end;

fun unifyRho rho1 rho2 =
  if rho1 = rho2 then () else
  case (!rho1, !rho2) of
      (NILrow, NILrow) => ()
    | (NILrow, VARrow _) =>
        rho2 := LINKrow rho1
    | (VARrow _, NILrow) =>
        rho1 := LINKrow rho2
    | (VARrow rv1, VARrow rv2) =>
        let val {rvEqu=rvEqu1, rvImp=rvImp1, rvLevel=rvLevel1, ...} = !rv1
            and {rvEqu=rvEqu2, rvImp=rvImp2, rvLevel=rvLevel2, ...} = !rv2
        in
          if rvLevel2 > rvLevel1 then
            setRvLevel rv2 rvLevel1
          else ();
          setRvEqu rv2 (rvEqu1 orelse rvEqu2);
          setRvImp rv2 (rvImp1 orelse rvImp2);
          rho1 := LINKrow rho2
        end
    | (_, _) => fatalError "unifyRow"
;



fun makeTyNameParameter kind  =
    { qualid= {qual=[],id=""},
      info=ref { tnStamp=newTyNameStamp(), tnKind=kind, tnLevel = !binding_level,
                 tnEqu=TRUEequ, tnSort = PARAMETERts, tnConEnv = ref NONE }}
    ;
    
val matchExModRef = 
    let fun dummyMatchExMod (_:ExMod) (_:ExMod) : unit = fatalError "dummyMatchExMod"
    in ref dummyMatchExMod
    end;


fun unify (tau1: Type) (tau2: Type) =
  let val tau1' = normType tau1
      and tau2' = normType tau2
  in
    case (tau1', tau2') of
        (VARt var1, VARt var2) =>
         (if var1 = var2 then () else
            (case (#tvKind(!var1), #tvKind(!var2)) of
                (Explicit _, Explicit _) =>
                  raise Unify UnifyOther
              | (Explicit _,_) =>
                  linkVarToType var2 tau1'
              | (_, Explicit _) =>
                  linkVarToType var1 tau2'
              | (_,_) =>
		  if #tvLevel(!var1) <= #tvLevel(!var2) then
		      linkVarToType var1 tau2'
		  else
		      linkVarToType var2 tau1'))
      | (VARt var, _) =>
          if isExplicit var then raise Unify UnifyOther
          else linkVarToType var tau2'
      | (_, VARt var) =>
          if isExplicit var then raise Unify UnifyOther
          else linkVarToType var tau1'
      | (ARROWt(a,b), ARROWt(a',b')) =>
          (unify a a'; unify b b')
      | (CONt(ts, tyapp), CONt(ts', tyapp')) =>
          if not ((kindTyApp tyapp) = (kindTyApp tyapp')) then raise Unify UnifyOther
          else (unifyTyApp tyapp tyapp';unifySeq ts ts')
      | (RECt r, RECt r') =>
          if r = r' then () else
          (unifyRec r r' : unit)
      | (PACKt exmod, PACKt exmod') =>
	  let val exmod = copyExMod [] [] exmod;
              val exmod' = copyExMod [] [] exmod';
          in
	      (((!matchExModRef) exmod exmod')
	       handle MatchError reason => raise (Unify (UnifyMod(SOME reason,NONE)));
	       ((!matchExModRef) exmod' exmod)
	       handle MatchError reason => raise (Unify ((UnifyMod(NONE,SOME reason)))))
	  end
      | (_, _) =>
          raise Unify UnifyOther
  end
and unifyTyApp tyapp tyapp' =
    if not ((kindTyApp tyapp) = (kindTyApp tyapp')) (* cvr: TODO remove this test *)
    then fatalError "unifyTyApp"
    else
    case (tyapp,tyapp') of
       (NAMEtyapp tn, NAMEtyapp tn') =>
         if isEqTN tn tn' then () else raise Unify UnifyOther
    |  (APPtyapp (tyapp,tyfun), APPtyapp(tyapp',tyfun')) =>
         (unifyTyApp tyapp tyapp';
          unifyTyFun (normTyFun tyfun) (normTyFun tyfun'))
    |  (_,_) => raise Unify UnifyOther
and unifyTyFun tyfun tyfun' =
    case (tyfun,tyfun') of
       (APPtyfun tyapp, APPtyfun tyapp') => 
          unifyTyApp tyapp tyapp'
    |  (LAMtyfun (tn,tyfun), LAMtyfun (tn',tyfun')) =>
          let val () = incrBindingLevel ();
              val (tn'',tn2tn'') = copyTyName PARAMETERts [] [] tn;
          in 
              unifyTyFun (copyTyFun [tn2tn''] []  tyfun)
                    (copyTyFun [(tn',NAMEtyapp tn'')] [] tyfun');
              decrBindingLevel()
          end
    | (LAMtyfun (tn,tyfun), APPtyfun tyapp) =>
          let val () = incrBindingLevel ();
              val (tn',tn2tn') = copyTyName PARAMETERts [] [] tn;
          in 
              unifyTyFun (copyTyFun [tn2tn'] [] tyfun)
                         (APPtyfun (APPtyapp (tyapp,APPtyfun (NAMEtyapp tn'))));
              decrBindingLevel()
          end
    | (TYPEtyfun (vs,ty), TYPEtyfun (vs',ty')) =>
          let val () = incrBindingLevel ();
              val tys  = newTypeParameters vs;
              val UE = zip2 vs tys;
              val UE' = zip2 vs' tys;
          in 
              unify (copyType [] UE ty) (copyType [] UE' ty');
              decrBindingLevel()
          end
    | (TYPEtyfun (vs,ty), APPtyfun tyapp) =>
          let val () = incrBindingLevel ();
              val tys  = newTypeParameters vs;
              val UE = zip2 vs tys;
          in 
              unify (copyType [] UE ty) (CONt(tys,tyapp));
              decrBindingLevel()
          end
    | (APPtyfun tyapp, TYPEtyfun (vs,ty)) =>
          let val () = incrBindingLevel ();
              val tys  = newTypeParameters vs;
              val UE = zip2 vs tys;
          in 
              unify (CONt(tys,tyapp)) (copyType [] UE ty);
              decrBindingLevel()
          end
    | (_,_) => fatalError "unifyTyFun" 
(*
and linkVarToType var tau =
(
  occur_check var tau;
  prune_level (#tvLevel(!var)) tau;
  updateAttributes var tau;
  setTvKind var (LinkTo tau)
)
*)
and linkVarToType var tau =
 let val fnvs as (fns,fvs,frvs) = freeVarsType [] [] ([],[],[]) tau
  in
      occur_check var fvs;
      (prune_level (#tvLevel(!var)) fnvs
       handle ScopeViolation sv => raise (Unify (UnifyScope (var,sv))));
      updateAttributes var tau fnvs;
      setTvKind var (LinkTo tau)
  end

and unifySeq (ts: Type list) (ts': Type list) = app2 unify ts ts'

and unifyRec (r : RecType) (r' : RecType) =
  let val {fields=fs,  rho=rho}  = !r
      val {fields=fs', rho=rho'} = !r'
      fun unifyRecAcc fs1 rho1 fs2 rho2 acc =
            ((case (fs1, fs2) of
                  ([], []) => (unifyRho rho1 rho2; (rev acc, rho1))
                | ((lab1,t1)::fs1', []) =>
                    let val (t2, rho2') = extractLab r r' lab1 rho2 in
                      unify t1 t2;
                      unifyRecAcc fs1' rho1 [] rho2' ((lab1,t1)::acc)
                    end
                | ([], (lab2,t2)::fs2') =>
                    let val (t1, rho1') = extractLab r r' lab2 rho1 in
                      unify t1 t2;
                      unifyRecAcc [] rho1' fs2' rho2 ((lab2,t2)::acc)
                    end
                | ((lab1,t1)::fs1', (lab2,t2)::fs2') =>
                   (if lt_lab lab1 lab2 then
                      let val (t2, rho2') = extractLab r r' lab1 rho2 in
                        unify t1 t2;
                        unifyRecAcc fs1' rho1 fs2 rho2' ((lab1,t1)::acc)
                      end
                    else if lt_lab lab2 lab1 then
                      let val (t1, rho1') = extractLab r r' lab2 rho1 in
                        unify t1 t2;
                        unifyRecAcc fs1 rho1' fs2' rho2 ((lab2,t2)::acc)
                      end
                    else (* lab1 = lab2 *)
                      (unify t1 t2;
                       unifyRecAcc fs1' rho1 fs2' rho2 ((lab1,t1)::acc))))
             : (Lab * Type) list * RowType)
      val (fs'', rho'') = unifyRecAcc fs rho fs' rho' []
  in
    setRtFields r fs''; setRtRho r rho'';
    setRtFields r' fs''; setRtRho r' rho''
  end
;

val equalsTyFunTyName = fn tyfun => fn tn =>
    if kindTyName tn = kindTyFun tyfun
	then 
	    (unifyTyFun (normTyFun tyfun) (APPtyfun(NAMEtyapp tn));
	     true)
	    handle Unify _ => false
    else false;



fun generalization isExpansive tau =
  let val (_,fvs,_)= freeVarsType [] [] ([],[],[]) tau
      val parameters = 
	  foldL (fn var => fn parameters => 
		 let val {tvImp, tvOvl, tvLevel, ...} = !var in
		     if member var parameters then parameters
		     else if tvLevel <= !binding_level then
			 parameters
		     else if tvOvl then
			 (setTvLevel var (!binding_level);
			  parameters)
		     else if tvImp andalso isExpansive then
			 (setTvLevel var (!binding_level);
			  parameters)
		     else
			 var :: parameters 
		 end)
	  []
	  fvs
 
  in
    TypeScheme {tscParameters = parameters, tscBody = tau}
  end;

(* cvr: TODO simplify by call to freeVars?*)
fun checkClosedType parameters tau = 
          case normType tau of
                VARt var =>
                  if member var (parameters) 
                  then ()
                  else raise Subscript
              | ARROWt(t,t') =>
                  (checkClosedType parameters t; checkClosedType parameters t')
              | CONt(ts, tyapp) =>
                  (app (checkClosedType parameters) ts; checkClosedTyApp parameters tyapp)
              | RECt (ref{fields, ...}) =>
                  app_field (checkClosedType parameters) fields
              | PACKt X => checkClosedExMod parameters X
and checkClosedTypeScheme parameters (TypeScheme {tscParameters,tscBody})  = 
    checkClosedType (tscParameters@parameters) tscBody
and checkClosedTyApp parameters tyapp =
    case (* normTyApp *) tyapp of
       NAMEtyapp _ => ()              
    |  APPtyapp (tyapp,tyfun) => (checkClosedTyApp parameters tyapp; checkClosedTyFun parameters tyfun)
and checkClosedTyFun parameters tyfun =
    case normTyFun tyfun of 
       TYPEtyfun (vs,ty) => checkClosedType (vs@parameters) ty
    |  LAMtyfun (v,tyfun) => checkClosedTyFun parameters tyfun
    |  APPtyfun tyapp => checkClosedTyApp parameters tyapp
and checkClosedVarEnv parameters VE = 
       traverseEnv (fn id => fn {qualid, info = (sc,_)} => checkClosedTypeScheme parameters sc) VE
and checkClosedTyEnv parameters VE = 
       traverseEnv (fn id => fn (tyfun,_) => checkClosedTyFun parameters tyfun) VE
and checkClosedStr parameters str = 
    case str of 
       STRstr (ME,FE,GE,TE,VE) =>
        (checkClosedModEnv parameters ME;
	 checkClosedFunEnv parameters FE;
	 checkClosedSigEnv parameters GE;
         checkClosedTyEnv parameters TE;
         checkClosedVarEnv parameters VE)
    |  SEQstr (str,str') => 
        (checkClosedStr parameters str;checkClosedStr parameters str')
and checkClosedRecStr parameters RS = 
    case RS of
      RECrec (RS,RS') =>
        (checkClosedRecStr parameters RS;checkClosedRecStr parameters RS')
    | NONrec S => (checkClosedStr parameters S)
and checkClosedMod parameters M = 
    case M of
      STRmod RS => checkClosedRecStr parameters RS
    | FUNmod (T,M,X) => (checkClosedMod parameters M;checkClosedExMod parameters X)
and checkClosedModEnv parameters ME = 
       traverseEnv (fn id => fn {qualid, info = RS} => checkClosedRecStr parameters RS) ME
and checkClosedGenFun parameters (T,M,X) = (checkClosedMod parameters M; checkClosedExMod parameters X)
and checkClosedFunEnv parameters FE = 
       traverseEnv (fn id => fn {qualid, info = F} => checkClosedGenFun parameters F) FE
and checkClosedExMod parameters (EXISTSexmod(T,M)) = (checkClosedMod parameters M)
and checkClosedSig parameters (LAMBDAsig(T,M)) = (checkClosedMod parameters M)
and checkClosedSigEnv parameters GE =
       traverseEnv (fn id => fn {qualid, info = G} => checkClosedSig parameters G) GE;




local fun errorToplevelImperativeVar desc id =
(
  msgIBlock 0;
  if !value_polymorphism then
      errPrompt ("Value polymorphism: Free type variable at top level in "^desc^" identifier "^id)
  else
      errPrompt ("Free imperative type variable at top level in "^desc^" identifier "^id);
  msgEOL();
  msgEBlock();
  raise Toplevel
)
in
fun checkClosedCSig (csig:CSig) = 
      ( Hasht.apply (fn id => fn {qualid, info = (sc,_)} => 
		     (checkClosedTypeScheme [] sc) 
		     handle Subscript => errorToplevelImperativeVar "value" id)
       (varEnvOfSig csig);
       Hasht.apply (fn id => fn (tyfun,_) => 
                    (checkClosedTyFun [] tyfun) 
                    handle Subscript => errorToplevelImperativeVar "type" id)
       (tyEnvOfSig csig);
       Hasht.apply (fn id => fn {qualid, info = RS} => 
                    (checkClosedRecStr [] RS) 
                    handle Subscript => errorToplevelImperativeVar "structure" id)
       (modEnvOfSig csig);
       Hasht.apply (fn id => fn {qualid, info = F} => 
                    (checkClosedGenFun [] F) 
                    handle Subscript => errorToplevelImperativeVar "functor" id)
       (funEnvOfSig csig);
       Hasht.apply (fn id => fn {qualid, info = G} => 
                    (checkClosedSig [] G) 
                    handle Subscript => errorToplevelImperativeVar "signature" id)
       (sigEnvOfSig csig))
end;








fun mkScheme pars tau =
  TypeScheme{ tscParameters=pars, tscBody=tau }
;

fun trivial_scheme tau = TypeScheme{tscParameters = [], tscBody = tau};

fun scheme_1u_attr equ imp builder =
  let val () = incrBindingLevel()
      val v = VARt (newTypeVar equ imp false)
      val tau = builder v
  in
    decrBindingLevel();
    generalization false tau
  end;

fun scheme_1u     builder = scheme_1u_attr false false builder;
fun scheme_1u_eq  builder = scheme_1u_attr true  false builder;
fun scheme_1u_imp builder = scheme_1u_attr false true  builder;

fun scheme_2u builder =
  let val () = incrBindingLevel()
      val a1 = newUnknown()
      val a2 = newUnknown()
      val tau = builder a1 a2
  in
    decrBindingLevel();
    generalization false tau
  end;

fun scheme_3u builder =
  let val () = incrBindingLevel()
      val a1 = newUnknown()
      val a2 = newUnknown()
      val a3 = newUnknown()
      val tau = builder a1 a2 a3
  in
    decrBindingLevel();
    generalization false tau
  end;

(*
fun specialization scheme =
  let val TypeScheme{ tscParameters, tscBody } = scheme in
    case tscParameters of
      [] => tscBody
    | _ =>
        let val newUnknowns =
              map (fn var => (var, VARt(refreshTypeVar var))) tscParameters
            fun copy tau =
                  case normType tau of
                     t as (VARt var) =>
                      (lookup var newUnknowns
                       handle Subscript => t)
                  | ARROWt(t,t') =>
                      ARROWt(copy t, copy t')
                  | CONt(ts, tn) =>
                      CONt(map copy ts, tn)
                  | RECt (ref{fields, rho}) =>
                      RECt (ref{fields=map_fields copy fields, rho=rho})
        in copy tscBody end
  end;
*)

fun specialization scheme =
  let val TypeScheme{ tscParameters, tscBody } = scheme in
    case tscParameters of
      [] => tscBody
    | _ =>
        let 
	    val UE =
              map (fn var => (var, VARt(refreshTypeVar var))) tscParameters;
        in copyType [] UE tscBody end
  end;

fun TypeOfScheme (TypeScheme {tscBody, ...}) = tscBody;

fun freshSchemeOfType tau =
  let
    val (_,parameters,_) = freeVarsType [] [] ([],[],[]) tau 
    val vs = map (fn _ => newTypeParameter false false false) parameters
    val UE = Fnlib.map2 (fn var => fn var' => (var, VARt var'))
                        parameters vs
  in
    TypeScheme{tscParameters = vs, tscBody = copyType [] UE tau}
  end;

fun schemeViolatesEquality sch =
  let val TypeScheme {tscBody, tscParameters} = sch in
    case normType tscBody of
        ARROWt(t,t') =>
          assumingEqualityTypeVars tscParameters typeViolatesEquality t
      | _ => false
  end;

fun rhoIsImperative rho =
  case !rho of
      NILrow => true
    | VARrow (ref{rvImp, ...}) => rvImp
    | _ => fatalError "rhoIsImperative"
;

fun typeIsImperative tau =
  case normType tau of
      VARt (ref{tvImp, ...}) => tvImp
    | ARROWt(t,t') =>
        (typeIsImperative t andalso typeIsImperative t')
    | CONt(ts, _) =>
        all typeIsImperative ts
    | RECt (ref{fields, rho}) =>
        (all_fields typeIsImperative fields andalso rhoIsImperative rho)
    | PACKt _ => false

;


fun setRho_level (rho: RowType) =
  case !rho of
      VARrow rv =>
        setRvLevel rv (!binding_level)
    | _ => fatalError "setRho_level"
;

(* Some predefined types *)

fun type_con ts tyname = CONt(ts, NAMEtyapp tyname)
and type_arrow t1 t2 = ARROWt(t1, t2)
and type_rigid_record fs =
  RECt (ref{fields=sortRow fs, rho=freshNilRowType()})
and type_flexible_record fs rho =
(
  setRho_level rho;
  RECt (ref{fields=sortRow fs, rho=rho})
);

fun type_pair t t' = type_rigid_record (mkPairRow t t');
fun type_product ts = type_rigid_record (mkTupleRow ts);

val type_bool      = type_con [] tyname_bool;
val type_char      = type_con [] tyname_char;
val type_exn       = type_con [] tyname_exn;
fun type_frag t    = type_con [t] tyname_frag;
val type_int       = type_con [] tyname_int;
fun type_list t    = type_con [t] tyname_list;
fun type_option t  = type_con [t] tyname_option;
val type_order     = type_con [] tyname_order;
val type_ppstream  = type_con [] tyname_ppstream;
val type_real      = type_con [] tyname_real;
fun type_ref t     = type_con [t] tyname_ref;
val type_string    = type_con [] tyname_string;
val type_substring = type_con [] tyname_substring;
val type_syserror  = type_con [] tyname_syserror;
val type_word      = type_con [] tyname_word;
val type_word8     = type_con [] tyname_word8;
val type_unit      = type_product [];
fun type_vector t  = type_con [t] tyname_vector;

val sc_bool = trivial_scheme type_bool;
val sc_order = trivial_scheme type_order;

local
  fun mktyname qual name info =
    {qualid={qual=qual, id=[name]}, info=ref info}  :  ConInfo global;
  fun mkSML name info =
    mktyname "General" name info;
in
val infoFalse = mkSML "false"
  { conArity=0,   conIsGreedy=false,   conTag=0,   conSpan=2,
    conType=sc_bool }
and infoTrue = mkSML "true"
  { conArity=0,   conIsGreedy=false,   conTag=1,   conSpan=2,
    conType=sc_bool }
and infoNil = mkSML "nil"
  { conArity=0,   conIsGreedy=false,   conTag=0,   conSpan=2,
    conType= scheme_1u (fn a => type_list a) }
and infoCons = mkSML "::"
  { conArity=2,   conIsGreedy=true,    conTag=1,   conSpan=2,
    conType= scheme_1u (fn a =>
      type_arrow (type_pair a (type_list a)) (type_list a)) }
and infoNONE = mkSML "NONE"
  { conArity=0,   conIsGreedy=false,   conTag=0,   conSpan=2,
    conType= scheme_1u (fn a => type_option a) }
and infoSOME = mkSML "SOME"
  { conArity=1,   conIsGreedy=false,   conTag=1,   conSpan=2,
    conType= scheme_1u (fn a =>
      type_arrow a (type_option a)) }
and infoEQUAL = mkSML "EQUAL"
  { conArity=0,   conIsGreedy=false,   conTag=0,   conSpan=3,
    conType=sc_order }
and infoGREATER = mkSML "GREATER"
  { conArity=0,   conIsGreedy=false,   conTag=1,   conSpan=3,
    conType=sc_order }
and infoLESS = mkSML "LESS"
  { conArity=0,   conIsGreedy=false,   conTag=2,   conSpan=3,
    conType=sc_order }
and infoANTIQUOTE = mkSML "ANTIQUOTE"
  { conArity=1,   conIsGreedy=false,   conTag=0,   conSpan=2,
    conType= scheme_1u (fn a =>
      type_arrow a (type_frag a)) }
and infoQUOTE = mkSML "QUOTE"
  { conArity=1,   conIsGreedy=false,   conTag=1,   conSpan=2,
    conType= scheme_1u (fn a =>
      type_arrow type_string (type_frag a)) }
;
end;

val initial_bool_CE = ConEnv [infoFalse, infoTrue];
val initial_list_CE = ConEnv [infoNil, infoCons];
val initial_option_CE = ConEnv [infoNONE, infoSOME];
val initial_order_CE = ConEnv [infoEQUAL, infoGREATER, infoLESS];
val initial_frag_CE = ConEnv [infoANTIQUOTE, infoQUOTE];

val unit_General = newSig "General" "General" STRmode;

val () = setTnSort (#info tyname_unit)      (REAts (TYPEtyfun([], type_unit)));
val () = (#tnConEnv (!(#info tyname_bool))) := SOME (initial_bool_CE);
val () = (#tnConEnv (!(#info tyname_list))) := SOME (initial_list_CE);
val () = (#tnConEnv (!(#info tyname_option))) := SOME (initial_option_CE);
val () = (#tnConEnv (!(#info tyname_order))) := SOME (initial_order_CE);
val () = (#tnConEnv (!(#info tyname_frag))) := SOME (initial_frag_CE);

local fun lookupStr envOfStr (STRstr S) id = lookupEnv (envOfStr S) id
	| lookupStr envOfStr (SEQstr (S,S')) id = 
             (lookupStr envOfStr S' id)
	     handle Subscript => lookupStr envOfStr S id
in      
fun lookupStr_ModEnv S path id info =
   (lookupStr #1 S id)    
   handle Subscript => 
       raise MatchError (MissingStructure (path, id, info))
fun lookupStr_FunEnv S path id info =
   (lookupStr #2 S id)    
   handle Subscript => 
       raise MatchError (MissingFunctor (path, id, info))
fun lookupStr_SigEnv S path id info =
   (lookupStr #3 S id)    
   handle Subscript => 
       raise MatchError (MissingSignature (path, id, info))
fun lookupStr_TyEnv S path id info =
   (lookupStr #4 S id)
   handle Subscript => 
       raise MatchError (MissingType (path, id, info))
fun lookupStr_VarEnv S path id info =
   (lookupStr #5 S id)    
   handle Subscript => 
       raise MatchError (MissingValue (path, id, info))
end;


(* cvr: since substitution now expands realisations automatically by normalisation we can simplify expandRea to:  *)

fun expandRea UE tau = 
  type_subst UE tau; 

fun newParTypeVar () =
  mkTypeVar false false false 0
;

fun newHardTypeVar () =
  let val tv = mkTypeVar false false false 0 in
    setTvKind tv (Explicit "");
    tv
  end;

fun isTypeFcnEqu vs' tau' vs tau =
  let val ts = map (fn _ => TypeOfTypeVar(newHardTypeVar())) vs
      val UE = zip2 vs ts
      val tau0 = expandRea UE tau
      val UE' = zip2 vs' ts
      val tau'0 = type_subst UE' tau'
  in
    (unify tau'0 tau0; true)
    handle Unify _ => false
  end
;

fun matchDatatype path (id : string) (infTyStr as (_,ConEnv infCE : ConEnv)) 
                                     (specTyStr as (_,ConEnv specCE : ConEnv)) =
    (*cvr: changed ^^^^^^^^^^^^, ^^^^^^           and  ^^^^^^ *)
    let val domCE  = map (fn gci => hd(#id(#qualid gci))) ( infCE)
	val domCE' = map (fn gci => hd(#id(#qualid gci))) ( specCE)
    in
	(* domCE' is non-empty, because `abstype' is not allowed *)
	(* in signatures, and "primitive" types are represented  *)
	(* as NILts. *)
	if domCE <> domCE' 
	    then raise MatchError (ConEnvMismatch (path, id, infTyStr, specTyStr))
	else ()
        (* We don't have to compare the types of constructors here, *)
        (* because they will be compared as values. Note that all *)
        (* constructors are visible, for redefining values in signatures *)
        (* is not allowed. *)
    end
  | matchDatatype _ _ _ _  = fatalError "matchDatatype"
;

fun refreshHardTypeVar (var : TypeVar) =
  let val {tvEqu, tvImp, ...} = !var
      val tv = mkTypeVar tvEqu tvImp false (!binding_level)
  in
    setTvKind tv (Explicit "");
    tv
  end;

fun refreshFlexibleTypeVar (var : TypeVar) = (* cvr: TODO is it ok to ignore overloading? *)
  let val {tvEqu, tvImp, ...} = !var
  in
    mkTypeVar tvEqu tvImp false (!binding_level)
  end;

(* cvr: refreshing type names to change their status *)
fun refreshTyName tnSort ({qualid,info}:TyName) = 
  let val { tnStamp=stamp, 
	    tnKind=kind, 
	    tnEqu=equ, 
	    tnSort=sort, 
	    tnLevel=_,
            tnConEnv=tnConEnv} = !info in
      info := {tnStamp=stamp, 
	       tnKind=kind, (* change the status *)
	       tnEqu=equ, 
	       tnSort=tnSort, 
	       tnLevel= !binding_level, (* update the level *)
	       tnConEnv = tnConEnv}
  end;    

fun refreshTyNameSet tnSort (T:TyNameSet) = 
    app (refreshTyName tnSort) T;

(* cvr: matching type schemes *)

fun matchTypeSchemes path id (infInfo as {info = (infSc,_),qualid = _}) 
                             (specInfo as {info = (specSc,_),qualid = _}) =
  let
    val TypeScheme{tscParameters=vs, tscBody=tau} = specSc
    val ts = map (fn v => TypeOfTypeVar(refreshHardTypeVar v)) vs
    val UE = zip2 vs ts
    val tau0 = expandRea UE tau
    val TypeScheme{tscParameters=vs', tscBody=tau'} = infSc
    val ts' = map (fn v => TypeOfTypeVar(refreshFlexibleTypeVar v)) vs'
    val UE' = zip2 vs' ts'
    val tau'0 = type_subst UE' tau'
  in
    unify tau'0 tau0
    handle Unify _ =>
      (let val ts = map TypeOfTypeVar vs
	   val UE = zip2 vs ts
	   val tau0 = expandRea UE tau
       in
	   raise MatchError (SchemeMismatch (path, id,infInfo,specInfo))
       end)
  end;

exception NotAPattern;
fun patternOfTyFun (APPtyfun tyapp) = patternOfTyApp (tyapp,[])
  | patternOfTyFun tyfun = raise NotAPattern
and patternOfTyApp (NAMEtyapp (tn as {info = ref {tnSort = VARIABLEts,...},...}),tns) =  (tn,tns)
  | patternOfTyApp (APPtyapp (tyapp,
			      APPtyfun (NAMEtyapp (tn as {info = ref {tnSort = PARAMETERts,...},...}))),tns) =
    patternOfTyApp (tyapp,tn::tns)
  | patternOfTyApp _ = raise NotAPattern;

fun realizeTyStr path id (infTyStr : TyFun * ConEnv) (specTyStr : TyFun * ConEnv) =
(*cvr: modified   case (#1 infTyStr, #1 specTyStr) of *)
 case (normTyFun (#1 infTyStr), normTyFun (#1 specTyStr)) of (* cvr: inserted call to normTyFun *)
    (infTyFun, specTyFun) =>
 (let      
  in
    (case (kindTyFun infTyFun, kindTyFun specTyFun) of
      (ARITYkind infArity, ARITYkind specArity) =>
      if specArity  <> infArity then 
	   raise MatchError (ArityMismatch (path,id,infTyStr,specTyStr,infArity,specArity))
      else ()
    | (_,_) => fatalError "realizeTyStr:1" (* cvr: TODO *));
    (case EqualityOfTyFun specTyFun of 
	       (* cvr: TODO revise - it should be sufficient 
                  (and more efficient) to do this 
                  check only once we've determined that specTyFun 
		  is a pattern *)
        REFequ =>
          if (EqualityOfTyFun infTyFun) <> REFequ then 
	      raise MatchError (RefEqualityMismatch (path,id,infTyStr,specTyStr))
          else ()
      | TRUEequ =>
          if (EqualityOfTyFun infTyFun) = FALSEequ then 
	      raise MatchError (EqualityMismatch (path,id,infTyStr,specTyStr))
          else ()
      | FALSEequ =>
          ()
      | _ => fatalError "realizeTyStr:2");
    ((case patternOfTyFun specTyFun of
	   (tn,tns) =>
	       let val tnLevel = #tnLevel(!(#info tn))
		   val (fns,fvs,frvs) = freeVarsTyFun tns [] ([],[],[]) infTyFun 
		   val _ = (* occur check *)
		       app (fn tn' => 
			    if tn = tn' 
			    then raise MatchError (CircularMismatch(path,
								    id,
								    infTyStr,
								    specTyStr,
								    tn'))
			    else ()) 
		            fns
                   val _ = (prune_level (tnLevel) (fns,fvs,frvs))
                           handle ScopeViolation sv => 
			       raise MatchError (PatternMismatch (path,
								 id,
								 infTyStr,
								 specTyStr,
								 tn,
								 sv))
	       in
		   setTnSort (#info tn) (REAts (foldR (fn tn => fn tyfun => LAMtyfun(tn,tyfun)) (normTyFun infTyFun) tns))
               end)
	   handle  NotAPattern => ())
      (* cvr: *)
  end)

fun checkRealization (* (inferredSig : CSig) (specSig : CSig)*)
                path id (infTyStr : TyFun * ConEnv) (specTyStr : TyFun * ConEnv) =
    case (normTyFun (#1 specTyStr),(#2 specTyStr)) of
      (specTyFun, ConEnv []) =>
          let  val infTyFun = normTyFun (#1 infTyStr) 
          in
	      unifyTyFun infTyFun specTyFun (* cvr: CHECK THIS *)
	      handle Unify _ => 
		  raise MatchError (TransparentMismatch (path,id,infTyStr,specTyStr))
	  end
    | (specTyFun, specCE) => 
         case (normTyFun (#1 infTyStr),(#2 infTyStr)) of
             (infTyfun, ConEnv []) =>                           
		 raise MatchError (DatatypeMismatch (path,id,infTyStr,specTyStr))
         |   (infTyfun, infCE) =>            
		 matchDatatype path id infTyStr specTyStr

;

fun matchIdStatus (* os *) path id 
    (infInfo as {info = (_,infStatus),qualid = infQualid})
    (specInfo as {info = (_,specStatus),qualid = specQualid}) =
  let
      val {qual=infQual, ...} = infQualid
      val {qual=specQual,...} = specQualid 
  in
    case specStatus of
        VARname ovltype =>
          (case infStatus of
               VARname ovltype' =>
                 (if ovltype <> ovltype' then 
		      raise MatchError (StatusMismatch (path,id,infInfo,specInfo))
                  else ())
             | REFname => raise MatchError (StatusMismatch (path,id,infInfo,specInfo))
	     | _ => ())
      | PRIMname pi =>
          (case infStatus of
               PRIMname pi'=>
                 if pi <> pi' 
                 then 
		   raise MatchError (StatusMismatch (path,id,infInfo,specInfo))
		 else ()
             | _  => 
		     raise MatchError (StatusMismatch (path,id,infInfo,specInfo)))
      | CONname ci =>
          (case infStatus of
               CONname ci' =>
                 if #conArity(!ci) <> #conArity(!ci')
                 orelse #conIsGreedy(!ci) <> #conIsGreedy(!ci')
                 orelse #conTag(!ci) <> #conTag(!ci')
                 orelse #conSpan(!ci) <> #conSpan(!ci')
		 then raise MatchError (StatusMismatch (path,id,infInfo,specInfo))
                 else ()
             | _ => raise MatchError (StatusMismatch (path,id,infInfo,specInfo)))
      | EXNname ei =>
          (case infStatus of
              EXNname ei' =>
       	        (if #exconArity(!ei) <> #exconArity(!ei')
		 then raise MatchError (StatusMismatch (path,id,infInfo,specInfo))
                 else ())
             | _ => raise MatchError (StatusMismatch (path,id,infInfo,specInfo)))
      | REFname =>
          (case infStatus of
	       REFname => ()
	     | _ => raise MatchError (StatusMismatch (path,id,infInfo,specInfo)))
  end
and matchStr path S S' = 
    case S' of
      STRstr (ME,FE,GE,TE,VE) => 
	  (traverseEnv (fn id => fn specInfo =>
			realizeTyStr path id 
			  (lookupStr_TyEnv S path id specInfo) specInfo)
	   TE;
	   traverseEnv (fn id => fn specInfo =>
			checkRealization path id 
			(lookupStr_TyEnv S path id specInfo) specInfo)
	   TE;
	   traverseEnv (fn id => fn specInfo =>
			let val infInfo = lookupStr_VarEnv S path id specInfo
			in
			    (matchTypeSchemes path id infInfo specInfo;
			     matchIdStatus path id infInfo specInfo)
			end) 
	   VE;
	   traverseEnv (fn id => fn specInfo =>
			matchModBind path id 
			(lookupStr_ModEnv S path id specInfo) specInfo)
	   ME;
	   traverseEnv (fn id => fn specInfo =>
			matchFunBind path id 
			(lookupStr_FunEnv S path id specInfo) specInfo)
	   FE;
	   traverseEnv (fn id => fn specInfo =>
			matchSigBind path id 
			(lookupStr_SigEnv S path id specInfo) specInfo)
	   GE)
  |  SEQstr (S1',S2') => (matchStr path S S1'; matchStr path S S2')
and matchRecStr path RS (RECrec (RS1',RS2')) = 
    (matchRecStr path RS RS1'; matchRecStr path RS RS2')
|   matchRecStr path (NONrec S) (NONrec S') = 
    (matchStr path S S')
|   matchRecStr path (RECrec(RS1,RS2)) RS' = 
    (matchRecStr path RS2 RS')
and matchFun path (T,M,X) (T',M',X') = 
   (incrBindingLevel();
    refreshTyNameSet PARAMETERts T';
    refreshTyNameSet VARIABLEts T;
    matchMod (DOMpath(path)) M' M; 
    matchExMod (RNGpath(path)) X X';
    decrBindingLevel())
and matchModBind path id {qualid = _,info = RS} {qualid = _,info = RS'} =
    matchRecStr (DOTpath(path,id))  RS RS'
and matchFunBind path id {qualid = _,info = F} {qualid = _,info = F'} =
    matchFun  (DOTpath(path,id)) F F'
and matchSigBind path id (infInfo as {qualid = _,info = G}) (specInfo as  {qualid = _,info = G'}) =
    ((matchSig  (NILpath) G G')
     handle MatchError reason => 
	 raise MatchError (SignatureMismatch(path,id,infInfo,specInfo,SOME reason,NONE));
     (matchSig  (NILpath) G' G)
     handle MatchError reason => 
	 raise MatchError (SignatureMismatch(path,id,infInfo,specInfo,NONE,SOME reason)))
and matchMod path M M' =
  case (M,M') of
     (STRmod RS,STRmod RS') => matchRecStr path RS RS'
  |  (FUNmod F, FUNmod F') => matchFun path F F'
  |  (_,STRmod _) => 
	 raise MatchError (ModuleMismatch(path,"functor","structure"))
  |  (_,FUNmod _) => 
	 raise MatchError (ModuleMismatch(path,"structure","functor"))

and matchExMod path (EXISTSexmod(T,M)) (EXISTSexmod(T',M')) =
   (incrBindingLevel();
    refreshTyNameSet PARAMETERts T;
    refreshTyNameSet VARIABLEts T';
    matchMod path M M';
    refreshTyNameSet PARAMETERts T'; (* forget the realisation *)
    decrBindingLevel())
and matchSig path (LAMBDAsig(T,M)) (LAMBDAsig(T',M')) =
   (incrBindingLevel();
    refreshTyNameSet PARAMETERts T;
    refreshTyNameSet VARIABLEts T';
    matchMod path M M';
    refreshTyNameSet PARAMETERts T'; (* forget the realisation *)
    decrBindingLevel());

fun matchInfixStatus path id infFixity specFixity = 
    if specFixity = infFixity then ()
    else raise MatchError(InfixStatusMismatch(path,id,infFixity,specFixity))
;

fun matchInfixBasis path infIBas specIBas =
   (Hasht.apply 
    (fn id => fn specFixity =>
        matchInfixStatus path id 
	                  ((Hasht.find infIBas id) 
			   handle Subscript => 
			       raise MatchError (MissingInfixStatus (path,id,specFixity)))
			  specFixity)
    specIBas)
;

fun matchCSig (inferredSig:CSig) (specSig:CSig) =
  case !(strOptOfSig specSig) of 
    NONE => fatalError "matchSignature"
  | SOME RS => 
     (* NB: the infix bases and sigenv's will be empty in STRmode *)
     let val (T,RS) = 
	   case copySig [] [] (LAMBDAsig(!(tyNameSetOfSig specSig),STRmod RS)) of 
	       LAMBDAsig(T,STRmod RS) => (T,RS)
	     | _  => fatalError "matchCSig"
	 val RS' = NONrec (STRstr (mk1TopEnv (#uModEnv inferredSig),
				   mk1TopEnv (#uFunEnv inferredSig),
				   mk1TopEnv (#uSigEnv inferredSig),
				   mk1TopEnv (#uTyEnv  inferredSig),
				   mk1TopEnv (#uVarEnv inferredSig)))
	     
     in
	 refreshTyNameSet VARIABLEts T;
	 matchRecStr UNITpath RS' RS;
	 matchInfixBasis UNITpath (iBasOfSig inferredSig) (iBasOfSig specSig)
     end
;

(* define the exported variants *)

local 
    fun pathOfLongStrId [id] = IDpath id
    | pathOfLongStrId (id::ids) = DOTpath(pathOfLongStrId ids,id)
    | pathOfLongStrId [] = NILpath
in
fun realizeLongTyCon (qualid as {qual,id = tycon::longstrid}) infTyStr specTyStr = 
    let val path = pathOfLongStrId longstrid
    in  realizeTyStr path tycon infTyStr specTyStr
    end
  | realizeLongTyCon _ _ _ = fatalError "realizeLongTyCon"
end;
   
   
val matchMod = matchMod NILpath;
val matchSig = matchSig NILpath;

(* tie the knot *)

val () = matchExModRef := matchExMod NILpath;

(* cvr: printing semantic objects *)

val free_tyname_names = ref ([] : (TyName * (string * int)) list);
val free_tyname_counter = ref 0;

val free_variable_names = ref ([] : (TypeVar * string) list);
val free_variable_counter = ref 0;

val savePrState = fn () =>
    (let val temp_freetyname_names = !free_tyname_names
         val temp_freetyname_counter = !free_tyname_counter
	 val temp_free_variable_names = !free_variable_names
	 val temp_free_variable_counter = !free_variable_counter
     in fn () => (free_tyname_names := temp_freetyname_names;
           	  free_tyname_counter := temp_freetyname_counter;
		  free_variable_names := temp_free_variable_names;
		  free_variable_counter := temp_free_variable_counter)
     end);

fun under_binder f a = 
    (let val temp_freetyname_names = !free_tyname_names
         val temp_freetyname_counter = !free_tyname_counter
	 val temp_free_variable_names = !free_variable_names
	 val temp_free_variable_counter = !free_variable_counter
         val r = f a
     in  free_tyname_names := temp_freetyname_names;
	 free_tyname_counter := temp_freetyname_counter;
	 free_variable_names := temp_free_variable_names;
	 free_variable_counter := temp_free_variable_counter;
         r
     end);

fun alphaOfInt i =
  if i < 26 then
    CharVector.fromList [Char.chr (i + 97)]
  else
    alphaOfInt(i div 26) ^ alphaOfInt(i mod 26)
;

fun choose_arbitrary_tyname () =
        let fun choose_arbitrary_tyname name =
	    if exists (fn (_,(name',_)) => name' = name) (!free_tyname_names)
		then (incr free_tyname_counter;
		      choose_arbitrary_tyname (alphaOfInt( !free_tyname_counter)))
	    else (incr free_tyname_counter;
		  (name,0))
	in
	    choose_arbitrary_tyname (alphaOfInt( !free_tyname_counter))
	end;

(*
fun choose_derived_tyname name =
	if exists (fn (_,name') => name' = name) (!free_tyname_names)  
	    then choose_derived_tyname (name ^ "'") 
	else name;
*)

fun choose_derived_tyname name =
	   let val (_,(_,lastsuffix)) = 
	       choose (fn (_,(name',_)) => name' = name) (!free_tyname_names)
	   in
	       (name,lastsuffix + 1)
	   end
           handle Subscript => (name,0)
;



fun choose_arbitrary_variable () =
        let fun choose_arbitrary_variable name =
	    if exists (fn (_,name') => name' = name) (!free_variable_names)
		then (incr free_variable_counter;
		      choose_arbitrary_variable (alphaOfInt( !free_variable_counter)))
	    else (incr free_variable_counter;
		  name)
	in
	    choose_arbitrary_variable (alphaOfInt( !free_variable_counter))
	end;

local 
    fun choose_derived_variable name =
	if exists (fn (_,name') => name' = name) (!free_variable_names)  
	    then choose_derived_variable (name ^ "'") 
	else name;
in
    val choose_derived_variable = 
	fn "" => choose_arbitrary_variable ()
	| name => choose_derived_variable (implode (case explode name of 
							#"'" :: #"_" :: rest => 
							rest
						      | #"'" :: #"'" :: #"_" :: rest => 
							rest
						      | #"'" :: #"'"  :: rest => 
							rest
						      | #"'"  :: rest => 
							rest
						      | rest => rest))
end

(* cvr: TODO  rationalise *)
fun collectExplicitVarsInObj freeVarsObj obj =
  let val (fns,fvs,_) = 
      freeVarsObj (map #1 (!free_tyname_names)) 
                   (map #1 (!free_variable_names))
                   ([],[],[])
		   obj
  in revApp (fn var =>
	  let val newname = 
	      case #tvKind(!var) of
		  Explicit name => choose_derived_variable name
		| _ => choose_arbitrary_variable ()
          in
	       free_variable_names := ((var, newname) :: !free_variable_names)
	  end)
          fvs;
      revApp (fn tn as {qualid={id = id,...},...} =>
	       (case id of
		    [""] => free_tyname_names := ((tn,choose_arbitrary_tyname()) :: !free_tyname_names)
		  | [name] =>
			let val newname = choose_derived_tyname name
			in
			    free_tyname_names := ((tn, newname) :: !free_tyname_names)
			end
		  | _ => free_tyname_names := ((tn,choose_arbitrary_tyname()) :: !free_tyname_names)))
     fns
  end
;


fun collectExplicitVars tau = collectExplicitVarsInObj freeVarsType tau;

fun collectTopVars ExEnv =
  let val (fns,fvs,_) = 
      freeVarsExEnv (map #1 (!free_tyname_names)) 
                    (map #1 (!free_variable_names))
		    ([],[],[])
		    ExEnv
  in revApp (fn var =>
	  let val newname = 
	      case #tvKind(!var) of
		  Explicit name => choose_derived_variable name
		| _ => choose_arbitrary_variable ()
          in
	       free_variable_names := ((var, newname) :: !free_variable_names)
	  end)
          fvs
  end
;



fun prTypeVar var =
 let val {tvEqu, tvImp, tvKind,...} = !var
     val name = lookup var (!free_variable_names)
	 handle Subscript =>
	     let val newname =
		 (case tvKind of 
		      Explicit name => 
			  choose_derived_variable name
		    |  _ => choose_arbitrary_variable ())
	     in
		 free_variable_names := (var, newname) :: !free_variable_names;
		 newname
	     end
 in
	msgString ("'"^
		   (if tvEqu then "'" else "") ^
		   (if tvImp andalso not (!value_polymorphism) then "_" else "") ^
                   name)
 end;


local fun prNameSuffix (name,0) = msgString name
      |   prNameSuffix (name,suffix) = (msgString name;msgString "/";msgInt suffix)
      fun prEqu equ =    
	  case equ of
	      FALSEequ => msgString ""
	    | TRUEequ => msgString "="
	    | REFequ => msgString "="
	    | ARROWequ(_,equ) => prEqu equ
in
fun prTyName showTnEqu tn = 
    (let val namesuffix = find (isEqTN tn) (!free_tyname_names)
            (* we use find instead of lookup because tynames in different
               units may be equivalent according to isEqTn without being
	       equivalent references *)
     in
	 prNameSuffix namesuffix
     end
    handle Subscript =>
	let val namesuffix as (name,suffix) =
	    case (#id(#qualid(tn))) of
		[""] => choose_arbitrary_tyname ()
	      | [name] => choose_derived_tyname name
	      | _ => choose_arbitrary_tyname ()
	in
	   free_tyname_names := (tn, namesuffix) :: !free_tyname_names;
	   if showTnEqu 
	       then prEqu (EqualityOfTyName tn)
           else ();
	   prNameSuffix namesuffix
	end)
end
;

fun arrowsToList tau =
    case normType tau of
	ARROWt(t, t') => t :: arrowsToList t'
      | t => [t]
;	    

fun prEnv prInfo env initial =
    foldEnv (fn k => fn v => fn initial => 
		(if initial 
		    then ()
		else (msgString ",";
		      msgBreak(1, 0));
                msgIBlock 0;
                prInfo k v;	       
                msgEBlock();
		false)) initial env;

fun prType prior tau =
  let
    fun prParen prior' s = if prior >= prior' then msgString s else ()
  in
    case normType tau of
      VARt var =>
        prTypeVar var
    | ARROWt(t,t') =>
        let val ts = t :: arrowsToList t' in
          prParen 1 "("; msgIBlock 0;
          prTypeSeq 1 " ->" 0 ts;
          prParen 1 ")"; msgEBlock()
        end
    | CONt(ts, NAMEtyapp tn) =>
        (case ts of
             []  => ()
           | [t] => (prType 2 t; msgString " ")
           | _ =>
               (msgIBlock 0; msgString "(";
                prTypeSeq 0 "," 1 ts;
                msgString ") "; msgEBlock());
	 prTyName false tn)
    | CONt(ts, tyapp) => (* cvr: TODO revise *)
        (case ts of
             []  => ()
           | [t] => (prType 2 t; msgString " ")
           | _ =>
               (msgIBlock 0; msgString "(";
                prTypeSeq 0 "," 1 ts;
                msgString ") "; msgEBlock());
	 prTyApp 1 tyapp)
    | RECt rt =>
        let val {fields=fs, rho=rho} = !rt in
          if isNilRowType rho then
            (if null fs then
               prTyName false tyname_unit
             else if isTupleRow fs then
               (prParen 2 "(";
                msgIBlock 0;
                prTypeSeq 2 " *" 0 (map snd fs);
                prParen 2 ")"; msgEBlock())
             else
               (msgString "{"; msgIBlock 0;
                prTypeRow fs; msgString "}"; msgEBlock()))
          else if null fs then
            msgString "{...}"
          else
            (msgString "{"; msgIBlock 0;
             prTypeRow fs; msgString ","; msgBreak(1, 0);
             msgString "...}"; msgEBlock())
        end
    | PACKt X => 
            (msgString "["; msgIBlock 0;
             prExMod 0 X;
             msgString "]"; msgEBlock())
  end
and prTypeSeq prior sep offset ts =
  case ts of
      [] => ()
    | [t] =>
        prType prior t
    | t :: rest =>
        (prType prior t; msgString sep; msgBreak(1, offset);
         prTypeSeq prior sep offset rest)
and prTypeVarSeq vs sep = (* cvr:TODO *)
  case vs of
      [] => ()
    | [v] =>
        prTypeVar v
    | v :: vs =>
        (prTypeVar v; msgString sep; msgBreak(1, 1);
         prTypeVarSeq vs sep)
and prTyNameSet T sep  = (* cvr:TODO *)
  case T of
      [] => ()
    | [tn] =>
         prTyName true tn
    | tn :: T' =>
        (prTyName true tn; msgString sep; msgBreak(1, 1);
         prTyNameSet T' sep)
and prTypeRow fs =
  case fs of
      [] => ()
    | [(lab,t)] =>
        (msgIBlock 0; printLab lab; msgString " :";
         msgBreak(1, 2); prType 0 t; msgEBlock())
    | (lab,t) :: rest =>
        (msgIBlock 0; printLab lab; msgString " :"; msgBreak(1, 2);
         prType 0 t;
         msgString ","; msgEBlock(); msgBreak(1, 0); prTypeRow rest)
and prTypeScheme sch = under_binder 
    (fn (TypeScheme {tscParameters,tscBody})  =>
     (case tscParameters of
	  [] => ()
	| _ => (msgString "!";
		prTypeVarSeq tscParameters "";
		msgString ".");
        prType 0 tscBody)) sch
and prTyApp prior tyapp  = 
  let
    fun prParen prior' s = if prior >= prior' then msgString s else ()
  in
    case stripTyApp [] tyapp of
       (tyname,[]) => 
	   prTyName false tyname
    |  (tyname,tyfuns) => 
        ( prParen 1 "(";  (* cvr: TODO revise *)
	  msgIBlock 0;
          prTyName false tyname;
          app (fn tyfun =>
	       (msgBreak(1,2);
	        prTyFun 1 tyfun))
	       tyfuns;
          prParen 1 ")";
	  msgEBlock())
  end
and prTyFun prior tyfun =
  let
      fun prParen prior' s = if prior >= prior' then msgString s else ()
  in
    case normTyFun tyfun of 
       TYPEtyfun ([],ty) =>
         prType prior ty
    |  TYPEtyfun vsty =>
        (prParen 1 "(";
         under_binder 
	   (fn (vs,ty) =>
	    ((case vs of
		 []  => ()
	       | _ =>
		     (msgIBlock 0; 
		      msgString "/\\";
		      prTypeVarSeq  vs ",";
		      msgString ".";
		      msgEBlock()));
	     prType prior ty))
	    vsty;
	  prParen 1 ")")
    |  LAMtyfun (tn,tyfun) => 
         (prParen 1 "(";
	  prBoundTyNameSet "/\\" 2 [tn] (fn () => prTyFun 0 tyfun);
	  prParen 1 ")")
    |  APPtyfun tyapp => 
       prTyApp prior tyapp
  end
and prInfixStatus id status =
(
  (case status of
       NONFIXst =>
         msgString "nonfix "
     | INFIXst i =>
         (msgString "infix ";
          msgInt i; msgString " ")
     | INFIXRst i =>
         (msgString "infixr ";
          msgInt i; msgString " "));
  msgString id
)
and prVarInfo prVal id info = 
    under_binder (fn {qualid,info = (TypeScheme {tscParameters,tscBody},status)} =>
		 (msgString
		  (case status of
		       VARname  _ => "val "
		     | PRIMname _ => "val "
		     | CONname  _ => "con "
		     | EXNname  _ => "exn "
		     | REFname    => "con ");
                  (case tscParameters of
		      [] => ()
		   |  [v] => (prTypeVar v;
			      msgBreak (1,1))
		   |  _  => (msgString "(";
			     prTypeVarSeq tscParameters ",";
			     msgString ")";
			     msgBreak (1,1)));
		  msgString id;
		  prVal info;
		  msgString " :";
		  msgBreak(1, 2);
		  (case status of  (* cvr: REVISE *)
		       VARname ovltype => 
			   (case ovltype of
			     REGULARo => prType 0 tscBody
			   | OVL1NNo => msgString "num -> num "
			   | OVL1NSo => msgString "numtext -> string "
			   | OVL2NNBo => msgString "numtext * numtext -> bool "
			   | OVL2NNNo => msgString "num * num -> num "
			   | OVL1TXXo => msgString "'a -> 'a "
			   | OVL1TPUo => msgString "(ppstream -> 'a -> unit) -> unit "
			   | OVL2EEBo => msgString  "''a * ''a -> bool ")
                      | CONname ci =>
			  (if (#conIsGreedy(!ci)) orelse (#conSpan(!ci) = 1)
			       then prType 0 tscBody
			   else case tscBody of
				  ARROWt(t,t') =>
				    (case normType t of   
					 RECt (ref {fields=(_::_),...}) =>
					     (msgString "("; msgIBlock 0;
					      prType 0 t;
					      msgEBlock ();
					      msgString ")";
					      msgString " -> "; 
					      prType 0 t')
				     | _ => prType 0 tscBody)
		                | _ => prType 0 tscBody)
		     | _ => prType 0 tscBody)))
                 info
and prTyInfo id (tyfun,conenv) = 
    let fun prTypeArgs vs = 
	    case vs of
		[] => ()
	      | [v] => (prTypeVar v; 
			msgBreak (1,1))
	      |  _  => (msgString "(";
			prTypeVarSeq vs ",";
			msgString ")";
			msgBreak (1,1))
    in
	case (etaExpandTyFun (normTyFun tyfun),conenv) of
	    (TYPEtyfun(vs,ty),ConEnv []) =>
		(msgString "type ";
		 under_binder (fn () =>
			       (prTypeArgs vs;
				msgString id;
				msgString " =";
				msgBreak(1, 2);
				prType 0 ty))
		              ())
	  | (TYPEtyfun(vs,ty),ConEnv _) =>
		(msgString "datatype ";
		 under_binder (fn () =>
			       (prTypeArgs vs;
				msgString id;
				msgString " =";
				msgBreak(1, 2);
				msgCBlock 1;
				msgString "(";
				prType 0 ty;
				msgString ",";
			        msgBreak(0,0)
				))
		               ();
		 prConEnv conenv;
		 msgString ")";
		 msgEBlock())
	 | (_,_) => fatalError "prTyInfo"
    end
and prConEnv CE = 
	    (msgString "{";
	     msgCBlock 0;
	     prEnv (prVarInfo (fn info => ())) 
	           (VEofCE CE)
		   true;
	     msgString "}";
 	     msgEBlock())
and prStrBody S initial = 
    case S of 
       STRstr (ME,FE,GE,TE,VE) =>
        let val initial = prEnv prModInfo ME initial  
	    val initial = prEnv prFunInfo FE initial  
	    val initial = prEnv prSigInfo GE initial  
	    val initial = prEnv prTyInfo TE initial 
	in prEnv (prVarInfo (fn info => ())) VE initial
	end
    |  SEQstr (S,S') => 
        let val initial = prStrBody S initial  
	in  prStrBody S' initial 
	end
and prRecStr RS = 
    case RS of
       NONrec S =>  
	   (msgString "{";
	    msgCBlock 0;
	    prStrBody S true ;  
	    msgString "}";
	    msgEBlock())
     | RECrec (RS,RS') =>
	   (msgString "rec (";
	    msgIBlock 0;
	    prRecStr RS;
	    msgString ",";             
	    msgBreak (0,3);
	    prRecStr RS';
	    msgEBlock();
	    msgString ")")
and prModInfo id {qualid,info = RS} =
		 (msgString "structure ";
		  msgString id;
		  msgString " :";
		  msgBreak(1, 2);
		  prRecStr RS)
and prBoundTyNameSet binder offset T =
    under_binder (fn prBody =>
		  (msgCBlock 0;
		   (case T of
			[] => ()
		      |   _ => (msgString binder;
				msgIBlock 0;
				prTyNameSet T "";
				msgString ".";
				msgEBlock();
				msgBreak(0,offset))
			);
(*				msgEBlock()));
                   msgBreak(0,offset); *)
		   prBody ();
		   msgEBlock()))
and prMod prior M = 
  let
    fun prParen prior' s = if prior >= prior' then msgString s else ()
  in
    case M of
     STRmod S => prRecStr S
   | FUNmod F => prGenFun prior F (* cvr: REVISE *)
  end
and prGenFun prior (T,M,X) = 
  let
    fun prParen prior' s = if prior >= prior' then msgString s else ()
  in
   (prParen 1 "(";
    prBoundTyNameSet "!" 1 T 
         (fn () => 
             (msgIBlock 0;
	      prMod 1 M;
	      msgString "->";             
	      msgBreak (0,4);
	      prExMod 0 X;
              msgEBlock()));
    prParen 1 ")")
  end
and prFunInfo id {qualid,info = F} =
		(msgString "functor ";
		 msgString id;
		 msgString " :";
		 msgBreak(1, 2);
		 prGenFun 0 F)
and prExMod prior (EXISTSexmod(T,M)) =
  let
    fun prParen prior' s = if prior >= prior' then msgString s else ()
  in
   (prParen 1 "(";
    prBoundTyNameSet "?" 1 T (fn () => prMod 0 M); (* cvr: REVISE*)
    prParen 1 ")")
  end
and prSig (LAMBDAsig(T,M)) =
   prBoundTyNameSet "/\\" 2 T (fn () => prMod 0 M)
and prSigInfo id {qualid,info = G} =
    (msgString "signature ";
     msgString id;
     msgString " =";
     msgBreak(1, 2);
     prSig G)
;

val prMod = prMod 0;
val prTyFun = prTyFun 0;
val prType = prType 0;

fun resetTypePrinter () =
( free_tyname_names := []; 
  free_tyname_counter := 0;
  free_variable_names := [];
  free_variable_counter := 0;
  app (fn tn as {qualid,...} =>
       if isGlobalName qualid andalso 
	  not (member (#qual qualid) (!preopenedPreloadedUnits)) andalso
	  not (member (#qual qualid) (pervasiveOpenedUnits))
	    then
(*		free_tyname_names := (tn,showQualId qualid) :: !free_tyname_names *)
		free_tyname_names := (tn,(showQualId qualid,0)) :: !free_tyname_names  (* cvr: TODO revise *)
	else
	    (case #id(qualid) of
		 [""] => free_tyname_names := ((tn,choose_arbitrary_tyname())
					       :: !free_tyname_names)
	       | [name] =>
		     let val newname = choose_derived_tyname name
		     in
			 free_tyname_names := ((tn, newname)
					       :: !free_tyname_names)
		     end
	       | _ => free_tyname_names := ((tn,choose_arbitrary_tyname()) :: !free_tyname_names)))
   (mkGlobalT ())
);

local val checkpointed_free_variable_names = ref [] 
in
val checkpoint_free_typevar_names = fn () => 
    (checkpointed_free_variable_names := map (fn (tv,string) => ((tv,!tv))) (!free_variable_names))
val rollback_free_typevar_names = fn () =>
    app (fn (tv,string) => 
	   tv := ((lookup tv (!checkpointed_free_variable_names)) 
	          handle Subscript => !tv))
        (!free_variable_names)
val commit_free_typevar_names = fn () =>
    free_variable_names := drop (fn (tv,name) => 
				 let val {tvEqu,tvImp,tvKind,...} =  
					 (lookup tv (!checkpointed_free_variable_names))
					  handle Subscript => !tv
				 in if (#tvEqu(!tv)) <> tvEqu
				       orelse (#tvImp(!tv)) <> tvImp
				       orelse case ((#tvKind(!tv)),tvKind) of
				                (LinkTo _, NoLink) => true | _ => false
                                    then
					let val tau = normType(VARt tv) 
					in
					    collectExplicitVars tau;
					    msgIBlock 0;
					    errPrompt "Warning: the free type variable ";
					    let val {tvEqu,tvImp,...} = 
						(lookup tv (!checkpointed_free_variable_names))
						handle Subscript => !tv
					    in 
						msgString ("'"^
							   (if tvEqu then "'" else "") ^
								(if tvImp andalso not (!value_polymorphism) 
								     then "_" else "") ^
								     name)
					    end;
					    msgString " has been instantiated to ";
					    prType tau;
					    msgEOL();
					    msgEBlock();
                           		    case ((#tvKind(!tv)),tvKind) of
				                (LinkTo _, NoLink) => true (* drop only linked vars *)
					      | _ => false
					end
				    else false
				 end)
                                (!free_variable_names);
end;


(* cvr: REVISE *)
fun printType tau =
  under_binder
  (fn tau =>
   (collectExplicitVars tau;
    prType  tau))
  tau
;

fun printNextType tau =
  prType  tau
;

fun printNextType tau =
  prType  tau
;

fun collectSchemeExplicitVars scheme =
  let val TypeScheme {tscBody, ...} = scheme
  in collectExplicitVars tscBody end
;

fun printScheme scheme = prTypeScheme scheme;
(*
  let val TypeScheme {tscBody, ...} = scheme
  in printType tscBody end
;*)

fun printNextScheme scheme = prTypeScheme scheme;
(*
  let val TypeScheme {tscBody, ...} = scheme
  in prType  tscBody end
;
*)

(* error reporting *)
local
 fun prPath path = 
     case path of
	 NILpath => 
	     (fn () => ())
       | IDpath id => 
	     (msgString " ";msgString id;
	      fn () => ())
       | DOTpath (NILpath,id) => 
	     (msgString " ";msgString id;
	      fn () => ())
       | DOTpath (path as DOMpath _,id) => 
	     (msgString " ";msgString id;
	      fn () => (msgString " of";
			prPath path()))
       | DOTpath (path as RNGpath _,id) => 
	     (msgString " ";msgString id;
	      fn () => (msgString " of";
			prPath path()))
       | DOTpath (UNITpath,id) => 
	     (case (modeOfSig (!currentSig)) of
		  STRmode => (msgString " ";msgString (!(#uIdent(!currentSig)));
			      msgString ".";msgString id;
			      fn () =>
			      (msgString " in the unit ";
			       msgString (currentUnitName())))
		| TOPDECmode => (msgString " ";msgString id;
				 fn () =>
				   (msgString " in the toplevel unit ";
				    msgString (currentUnitName())))) 
       | DOTpath (path,id) => 
	     (let val cont = prPath path
	      in
		 msgString ".";
		 msgString id;
                 cont
	     end)
      | DOMpath(NILpath) =>
	    (fn () => msgString " the domain")
      | DOMpath(path) =>
	    (fn () =>(msgString " the domain of";
		     prPath path()))
      | RNGpath(NILpath) =>
	    (fn () => msgString " the range")
      | RNGpath(path) =>
	    (fn () => (msgString " the range of";
		       prPath path ()))
      | UNITpath =>
	    (fn () => (case (modeOfSig (!currentSig)) of
			 STRmode =>
			     (msgString " the structure unit ";
			      msgString (currentUnitName()))
		       | TOPDECmode =>
			     (msgString " the toplevel unit ";
			      msgString (currentUnitName()))))
in
    val prPath = fn path => prPath path ()
end
;

(* cvr: it may be worth restructuring this code to print out the actual info for each field *)	     


fun errMatchReason infDesc specDesc matchreason = 
let fun prInf path =
        case path of
	    NILpath => msgString infDesc
	  | UNITpath => msgString infDesc
	  | IDpath _ => msgString infDesc
	  | DOTpath (path,_) => prInf path 
	  | DOMpath path => prSpec path 
	  | RNGpath path => prInf path 
    and prSpec path  =
	case path of
	    NILpath => msgString specDesc
	  | UNITpath => msgString specDesc
	  | IDpath _ => msgString specDesc
	  | DOTpath (path,_) => prSpec path
	  | DOMpath path => prInf path 
	  | RNGpath path => prSpec path

   fun errMissingDeclaration (path,id,info) s freeVarsInfo prInfo =
	under_binder (fn info => 
		      (collectExplicitVarsInObj freeVarsInfo info;	     
		       msgIBlock 0;
		       errPrompt "Missing declaration: ";msgString s;
		       prPath (DOTpath(path,id));msgEOL();
		       errPrompt "is specified in the ";
		       prSpec path;msgString " as ";
		       msgEOL();
		       errPrompt "  ";prInfo id info;msgEOL();
		       errPrompt "but not declared in the ";
		       prInf path;
		       msgEOL();
		       msgEBlock()))
	               info
in
    case matchreason of	
    MissingValue pathidinfo =>
	errMissingDeclaration pathidinfo 
	                      "value"
			      freeVarsVarInfo 
			      (prVarInfo (fn info => ()))
  | MissingType pathidinfo =>
	errMissingDeclaration pathidinfo 
	                      "type constructor"
			      freeVarsTyStr 
			      prTyInfo
  | MissingStructure pathidinfo =>
	errMissingDeclaration pathidinfo 
	                      "structure"
			      freeVarsModInfo 
			      prModInfo
  | MissingFunctor pathidinfo =>
	errMissingDeclaration pathidinfo 
	                      "functor"
			      freeVarsFunInfo 
			      prFunInfo
  | MissingSignature pathidinfo =>
	errMissingDeclaration pathidinfo 
	                      "signature"
			      freeVarsSigInfo 
			      prSigInfo
  | MissingInfixStatus pathidinfo =>
	errMissingDeclaration pathidinfo 
	                      "the infix status of"
			      (fn _ => fn _ => fn _ => fn _ => ([],[],[]))
			      prInfixStatus
  | InfixStatusMismatch (path,id,infInfo,specInfo) =>
       	    (msgIBlock 0;
	     errPrompt "Infix status mismatch: value identifier";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified with fixity status ";msgEOL();
	     errPrompt "  ";prInfixStatus id specInfo;msgEOL();
	     errPrompt "in the ";prSpec path;msgEOL();
             errPrompt "but declared with status ";msgEOL();
	     errPrompt "  ";prInfixStatus id infInfo;msgEOL();
	     errPrompt "in the ";prInf path;msgEOL();
	     msgEBlock())
  | SignatureMismatch (path,id,infInfo,specInfo,reasonopt,reasonopt') =>
	    under_binder
	    (fn () =>
	    (collectExplicitVarsInObj freeVarsSigInfo specInfo;
	     collectExplicitVarsInObj freeVarsSigInfo infInfo;
	     msgIBlock 0;
	     errPrompt "Signature mismatch: signature identifier";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified as ";msgEOL();
	     errPrompt "  ";prSigInfo id specInfo;msgEOL();
	     errPrompt "in the ";prSpec path;msgEOL();
	     errPrompt "but is declared as";
	     msgEOL();
 	     errPrompt "  ";prSigInfo id infInfo;msgEOL();
	     errPrompt "in the ";prInf path;msgEOL();
	     (case reasonopt of 
		  NONE => () 
		| SOME reason => 
		      (errPrompt "The declaration does \
		         \not match the specification because ...";
		       msgEOL();
		       errMatchReason "declared signature" "specified signature" reason);
 	      case reasonopt' of 
		  NONE => () 
		| SOME reason => 
		      (errPrompt "The specification does \
		         \not match the declaration because";
		       msgEOL();
		       errMatchReason "specified signature" "declared signature" reason));
	     errPrompt "The signatures should be equivalent.";
	     msgEOL();
	     msgEBlock()))
            ()
    (* cvr: we trap this StatusMismatch for two constructors 
       to report representation mismatches better *)
  | StatusMismatch (path,id,infInfo as {info=(_,CONname infConInfo),...},
			    specInfo as {info=(_,CONname specConInfo),...})=>
	let val (argdesc,fs) = 
	    (case specInfo of
		{info = (TypeScheme {tscBody = ARROWt(t,t'),...},_),...} =>
		    (case normType t of   
			 RECt (ref {fields = fs,...}) =>
			     (if isTupleRow fs then ("tuple",fs) else ("record",fs))
		       | _ => ("record",[]))
	      | _ => ("record",[]))
	    fun describeConInfo (ref {conArity=thisArity,
				    conIsGreedy=thisIsGreedy,
				    conTag=thisTag,
				    conSpan=thisSpan,
				    ...}) 
	                           (ref {conArity=otherArity,
				    conIsGreedy=otherIsGreedy,
				    conTag=otherTag,
				    conSpan=otherSpan,
				    ...}) = 
            (if otherArity <> thisArity andalso thisIsGreedy
	     then (msgString "a constructor carrying ";msgInt thisArity; 
		   msgString " fields of a "; msgString argdesc)
             else if otherArity <> thisArity andalso not(thisIsGreedy)
	     then (msgString "a constructor carrying a ";msgString argdesc;msgString " with ";msgInt otherArity; 
		   msgString " fields")
             else if not(otherIsGreedy) andalso thisIsGreedy
	     then (msgString "a constructor carrying one field of a ";msgString argdesc)
	     else if otherIsGreedy andalso not(thisIsGreedy)		  
             then msgString "a constructor carrying a record with one field"
             else if otherTag <> thisTag
	     then (msgString "constructor ";msgInt thisTag; msgString " of ";
		   msgInt thisSpan; msgString " constructor(s)")
             else (* otherSpan <> thisSpan *)
	          (msgString "one constructor out of ";
		   msgInt thisSpan; msgString " constructor(s)"))
           fun prFields fs = 
	       let fun prTy_ n = (msgString "<ty_";msgInt n;msgString ">")
		   fun prRow fs n =
		   case fs of
		       [] => ()
		     | [(lab,t)] =>
			   (msgIBlock 0; printLab lab; msgString " :";
			    msgBreak(1, 2); 
			    prTy_ n;
			    msgEBlock())
		     | (lab,t) :: rest =>
		       (msgIBlock 0; printLab lab; msgString " :"; msgBreak(1, 2);
			prTy_ n;
			msgString ","; msgEBlock(); msgBreak(1, 0); 
			prRow rest (n+1))
		   fun prTuple fs n =
		   case fs of
		       [] => ()
		     | [(lab,t)] =>
			   (msgIBlock 0; 
			    prTy_ n; msgEBlock())
		     | (lab,t) :: rest =>
		       (msgIBlock 0; 
			prTy_ n;
			msgString " *"; msgEBlock(); msgBreak(1,0);
                        prTuple rest (n+1))
	       in
		   if isTupleRow fs 
		       then prTuple fs 1
		   else (msgString "{";prRow fs 1;msgString "}")
	       end
           fun rectifyConInfo msg prDesc (ref {conArity=thisArity,
				    conIsGreedy=thisIsGreedy,
				    conTag=thisTag,
				    conSpan=thisSpan,
				    ...}) 
	                           (ref {conArity=otherArity,
				    conIsGreedy=otherIsGreedy,
				    conTag=otherTag,
				    conSpan=otherSpan,
				    ...}) = 
            (if otherArity <> thisArity orelse otherIsGreedy <> thisIsGreedy
	     then if thisIsGreedy
                  then (errPrompt msg;msgEOL();
			errPrompt "- in the ";prDesc path;
			msgString ", enclose the argument type of the constructor in parentheses:";msgEOL();
			errPrompt "  change \"";msgString id;msgString " of  ";
			                        prFields fs; msgString "\""; msgEOL();
			errPrompt "  to     \"";msgString id;msgString " of (";
                                                prFields fs; msgString ")\"";
			    msgEOL())
		  else (errPrompt msg;msgEOL();
			errPrompt "- in the ";prDesc path; 
			msgString", re-express the argument type of the constructor as a syntactic ";
			msgString argdesc;msgString ":";msgEOL();
			errPrompt "  change \"";msgString id;msgString " of ";
					 msgString "<ty>"; msgString "\""; msgEOL();
			errPrompt "  to     \"";msgString id;msgString " of ";
			                 prFields fs; msgString "\"";msgEOL())
	     else ())
       in
       under_binder (fn () =>
	    (collectExplicitVarsInObj freeVarsVarInfo specInfo;	     
	     collectExplicitVarsInObj freeVarsVarInfo infInfo;	     
	     msgIBlock 0;
	     errPrompt "Status mismatch: constructor ";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified as ";
	     describeConInfo specConInfo infConInfo;
	     msgString " in the ";prSpec path;msgEOL();
	     errPrompt "  ";prVarInfo (fn info => ()) id specInfo;msgEOL();
             errPrompt "but declared as ";
             describeConInfo infConInfo specConInfo;
	     msgString " in the ";prInf path;msgEOL();
	     errPrompt "  ";prVarInfo (fn info => ()) id infInfo;msgEOL();
	     rectifyConInfo "EITHER: edit the specification to match the declaration: " prSpec specConInfo infConInfo;
	     rectifyConInfo "OR: edit the declaration to match the specification: "  prInf infConInfo specConInfo;
	     msgEBlock())) 
             ()
       end
  | StatusMismatch (path,id,infInfo as {info=(_,infStatus),...},
			    specInfo as {info=(_,specStatus),...})=>
       under_binder (fn () =>
	    (collectExplicitVarsInObj freeVarsVarInfo specInfo;	     
	     collectExplicitVarsInObj freeVarsVarInfo infInfo;	     
	     msgIBlock 0;
	     errPrompt "Status mismatch: identifier";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified as ";
	     msgString (case (infStatus,specStatus) of
			    (VARname _ ,VARname _) => "an ordinary value"
			  | (_         ,VARname _)=> "a value"
			  | (PRIMname _,PRIMname _) => "one primitive"
			  | (_         ,PRIMname _) => "a primitive"
			  | ( CONname _,CONname _) => "a constructor"
			  | (_         ,CONname _) => "a constructor"
			  | (EXNname infei,EXNname specei) => 
				fatalError "errMatchReason 1"
(* ps:				(case(#exconTag(!infei),#exconTag(!specei)) of
				     (NONE,SOME _) => "a static exception"
				   | (SOME _,NONE) => "a dynamic exception"
				   | (_,_) => "an exception") *)
			  | (_         ,EXNname _) => "an exception"
			  | (_         ,REFname)    => "the `ref' constructor");
	     msgString " in the ";prSpec path;msgEOL();
	     errPrompt "  ";prVarInfo (fn info => ()) id specInfo;msgEOL();
             errPrompt "but declared as ";
	     msgString (case (infStatus,specStatus) of
			    (VARname  _,VARname _) => "an overloaded value"
			  | (VARname _,_)=> "an ordinary value"
			  | (PRIMname _,PRIMname _) => "a different primitive"
			  | (PRIMname _,_) => "a primitive"
			  | (CONname _,CONname _) => "a constructor with a different representation"
			  | (CONname _,_) => "a constructor"
			  | (EXNname infei,EXNname specei) => 
				fatalError "errMatchReason 2"
				(* ps:
				(case(#exconTag(!infei),#exconTag(!specei)) of
			             (NONE,SOME _) => "a dynamic exception"
				   | (SOME _,NONE) => "a static exception"
				   | (_,_) => "an exception with a different representation") *)
			  | (EXNname _,_) => "an exception"
			  | (REFname,_) => "the `ref' constructor");
	     msgString " in the ";prInf path;msgEOL();
	     errPrompt "  ";prVarInfo (fn info => ()) id infInfo;msgEOL();
	     msgEBlock())) 
             ()
  | ConEnvMismatch (path,id,infTyStr,specTyStr) => 
	 under_binder (fn () =>
	    (collectExplicitVarsInObj freeVarsTyStr specTyStr;
	     collectExplicitVarsInObj freeVarsTyStr infTyStr;
	     msgIBlock 0;
	     errPrompt "Datatype mismatch: type constructor";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified as the datatype";msgEOL();
	     errPrompt "  ";prTyInfo id specTyStr;msgEOL();
	     errPrompt "in the ";prSpec path;msgEOL();
	     errPrompt "but declared as the different datatype";msgEOL();
 	     errPrompt "  ";prTyInfo id infTyStr;msgEOL();
	     errPrompt "in the ";prInf path;msgEOL();
	     errPrompt "The datatypes should agree on the names \
                       \and the order of value constructors";
	     msgEOL();
	     msgEBlock()))
            ()
  | SchemeMismatch (path,id,infInfo,specInfo) =>
	    under_binder
	    (fn () =>
	    (collectExplicitVarsInObj freeVarsVarInfo specInfo;
	     collectExplicitVarsInObj freeVarsVarInfo infInfo;
	     msgIBlock 0;
	     errPrompt "Scheme mismatch: value identifier";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified with type scheme ";msgEOL();
	     errPrompt "  ";prVarInfo (fn info => ()) id specInfo;msgEOL();
	     errPrompt "in the ";prSpec path;msgEOL();
	     errPrompt "but its declaration has the unrelated type scheme ";
	     msgEOL();
 	     errPrompt "  ";prVarInfo (fn info => ()) id infInfo;msgEOL();
	     errPrompt "in the ";prInf path;msgEOL();
	     errPrompt "The declared type scheme should be at least as general as the specified type scheme";
	     msgEOL();
	     msgEBlock()))
            ()
  | ArityMismatch (path,id,infTyStr,specTyStr,infArity,specArity) =>
	 under_binder (fn () =>
	    (collectExplicitVarsInObj freeVarsTyStr specTyStr;
	     collectExplicitVarsInObj freeVarsTyStr infTyStr;
	     msgIBlock 0;
	     errPrompt "Arity mismatch: type constructor";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified with arity ";
	     msgInt (specArity);msgString " in the ";prSpec path;msgEOL();              
	     errPrompt "  ";prTyInfo id specTyStr;msgEOL();
	     errPrompt "but declared with arity ";msgInt (infArity);
	     msgString " in the ";prInf path;msgEOL();
 	     errPrompt "  ";prTyInfo id infTyStr;msgEOL();
	     errPrompt "The arities should agree";msgEOL();
	     msgEBlock()))
            ()
  | RefEqualityMismatch (path,id,infTyStr,specTyStr)  =>
	 under_binder (fn () =>
	    (collectExplicitVarsInObj freeVarsTyStr specTyStr;
	     collectExplicitVarsInObj freeVarsTyStr infTyStr;
	     msgIBlock 0;
	     errPrompt "Equality type mismatch: type constructor";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified as a `prim_EQtype' in the ";
	     prSpec path;msgEOL();              
	     errPrompt "  ";prTyInfo id specTyStr;msgEOL();
	     errPrompt "but is not declared as a `prim_EQtype' in the ";
	     prInf path;msgEOL();              
 	     errPrompt "  ";prTyInfo id infTyStr;msgEOL();
	     msgEBlock()))
            ()
  | EqualityMismatch (path,id,infTyStr,specTyStr) =>
	 under_binder (fn () =>
	    (collectExplicitVarsInObj freeVarsTyStr specTyStr;
	     collectExplicitVarsInObj freeVarsTyStr infTyStr;
	     msgIBlock 0;
	     errPrompt "Equality type mismatch: type constructor";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified as admitting equality in the ";
	     prSpec path;msgEOL();              
	     errPrompt "  ";prTyInfo id specTyStr;msgEOL();
	     errPrompt "but its declaration does not admit equality in the ";
	     prInf path;msgEOL();              
 	     errPrompt "  ";prTyInfo id infTyStr;msgEOL();
	     msgEBlock()))
            ()
  | TransparentMismatch (path,id,infTyStr,specTyStr) => 
	 under_binder (fn () =>
	    (collectExplicitVarsInObj freeVarsTyStr specTyStr;
	     collectExplicitVarsInObj freeVarsTyStr infTyStr;
	     msgIBlock 0;
	     errPrompt "Type mismatch: type constructor";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified as one abbreviation in the ";
	     prSpec path;msgEOL();              
	     errPrompt "  ";prTyInfo id specTyStr;msgEOL();
	     errPrompt "but declared as a different abbreviation in the ";
	     prInf path;msgEOL();              
 	     errPrompt "  ";prTyInfo id infTyStr;msgEOL();
	     errPrompt "The abbreviations should be equivalent";msgEOL();
	     msgEBlock()))
            ()
  | PatternMismatch (path,id,infTyStr, specTyStr, tn,sv) => 
	    under_binder 
	    (fn () =>
	     (collectExplicitVarsInObj freeVarsTyStr specTyStr;
	      collectExplicitVarsInObj freeVarsTyStr infTyStr;
	      msgIBlock 0;
	      errPrompt "Scope Violation: type constructor";
	      prPath (DOTpath(path,id));
              msgString " has specification:";msgEOL();
              errPrompt "  ";prTyInfo id specTyStr;msgEOL();
  	      errPrompt "in the ";prSpec path;msgEOL();
	      errPrompt "but is implemented by the declaration: ";msgEOL();
              errPrompt "  ";prTyInfo id infTyStr;msgEOL();
  	      errPrompt "in the ";prInf path;msgEOL();
	      errPrompt "The declaration violates the specification because ";
              msgEOL();
              (case sv of
		   TYNAMEsv tn' => 
		       (errPrompt "the type constructor ";
			prTyName false tn';
			msgString " is a parameter " ;
			msgEOL();
			errPrompt "that is declared within \
			 \the scope of ";
			prTyName false tn;
			msgString ".";
			msgEOL()) 
		 | TYPEVARsv tv => 
		       (errPrompt "the type variable ";
			prTypeVar tv;
			msgString " is a parameter " ;
			msgEOL();
			errPrompt "that is declared within \
			           \the scope of ";
			prTyName false tn;
			msgString ".";
			msgEOL()));
	      msgEBlock()))
	    ()
  | CircularMismatch (path,id,infTyStr,specTyStr,tn) => 
	    under_binder 
	    (fn () =>
	     (collectExplicitVarsInObj freeVarsTyStr specTyStr;
	      collectExplicitVarsInObj freeVarsTyStr infTyStr;
	      msgIBlock 0;
	      errPrompt "Circularity: type constructor";
	      prPath (DOTpath(path,id));
              msgString " has specification:";msgEOL();
              errPrompt "  ";prTyInfo id specTyStr;msgEOL();
  	      errPrompt "in the ";prSpec path;msgEOL();
	      errPrompt "but is implemented by the declaration: ";msgEOL();
              errPrompt "  ";prTyInfo id infTyStr;msgEOL();
  	      errPrompt "in the ";prInf path;msgEOL();
	      errPrompt "The declaration violates the specification because ";msgEOL();
	      errPrompt "of the circular occurrence of "; prTyName false tn;
              msgEOL();
	      msgEBlock()))
	    ()
  | DatatypeMismatch (path,id,infTyStr,specTyStr) =>
	 under_binder (fn () =>
	    (collectExplicitVarsInObj freeVarsTyStr specTyStr;
	     collectExplicitVarsInObj freeVarsTyStr infTyStr;
	     msgIBlock 0;
	     errPrompt "Datatype mismatch: type constructor";
	     prPath(DOTpath(path,id));msgEOL();
	     errPrompt "is specified as a datatype in the ";
	     prSpec path;msgEOL();
	     errPrompt "  ";prTyInfo id specTyStr;msgEOL();
	     errPrompt "but not declared as a datatype in the ";prInf path;msgEOL();
 	     errPrompt "  ";prTyInfo id infTyStr;msgEOL();
	     errPrompt "The declaration should also be a datatype";msgEOL();
	     msgEBlock()))
            ()
  | ModuleMismatch (path as NILpath,infDesc,specDesc) => 
	     (msgIBlock 0;
	      errPrompt "Module mismatch: The ";
              prSpec path;msgString " specifies a ";msgString specDesc;
	      msgEOL();
	      errPrompt "but the ";prInf path;msgString " is a " ;msgString infDesc;
	      msgEOL();
	      msgEBlock())
  | ModuleMismatch (path,infDesc,specDesc) => 
	     (msgIBlock 0;
	      errPrompt "Module mismatch:";
	      prPath path;msgEOL();
	      errPrompt "is specified as a ";msgString specDesc;
	      msgString " in the ";prSpec path;msgEOL();
	      errPrompt "but declared as a " ;msgString infDesc;
	      msgString " in the ";prInf path;msgEOL();
	      msgEBlock())
end
;

local 
  fun warnAdditional path desc =
	     (msgIBlock 0;
	      errPrompt "Warning: "; msgString desc;
	      prPath path;msgEOL();
              errPrompt "is declared by the implementation"; msgEOL ();
	      errPrompt "but not specified in the interface";
	      msgEOL();
	      msgEBlock());
  fun checkTyInfo path id (infTyStr : TyFun * ConEnv) (specTyStr : TyFun * ConEnv) =
	case (#2 infTyStr,#2 specTyStr) of
	    (ConEnv [], ConEnv []) => ()
	  | (ConEnv (_::_), ConEnv []) => 
		(msgIBlock 0;
		 errPrompt "Warning: type constructor";
		 prPath (DOTpath(path,id));msgEOL();
		 errPrompt "is declared by the implementation as a datatype";
		 msgEOL();
		 errPrompt "but specified as an ordinary type in the interface";
       	         msgEOL();
		 msgEBlock())
	  | (ConEnv _, ConEnv _) => ()
	  | (_,_) => fatalError "checkTyInfo"

  fun checkVarInfo path id 
    (infInfo as {info = (_,infStatus),qualid = infQualid})
    (specInfo as {info = (_,specStatus),qualid = specQualid}) =
    let
	val {qual=infQual, ...} = infQualid
	val {qual=specQual,...} = specQualid 
    in
      case specStatus of
        VARname ovltype => 
          (case infStatus of
	       CONname _ =>
		   (msgIBlock 0;
		    errPrompt "Warning: value";
		    prPath (DOTpath(path,id));msgEOL();
		    errPrompt "is declared by the implementation as a constructor";
		    msgEOL();
	            errPrompt "but specified as an ordinary value in the interface";
		    msgEOL();
		    msgEBlock())
	     | EXNname _ =>
		   (msgIBlock 0;
		    errPrompt "Warning: value";
		    prPath (DOTpath(path,id));msgEOL();
		    errPrompt "is declared by the implementation as an \
	                       \exception constructor";
		    msgEOL();
		    errPrompt "but specified as an ordinary value in the interface";
		    msgEOL();
		    msgEBlock())
	     | _ => ())
      | _  => ()
  end
  and checkStr path S S' = 
      case S of
	 STRstr (ME,FE,GE,TE,VE) => 
	  (traverseEnv (fn id => fn infInfo =>
			checkTyInfo path id infInfo (lookupEnv (TEofStr S') id)
			handle Subscript => 
			    warnAdditional (DOTpath(path,id)) "type constructor")
	   TE;
	   traverseEnv (fn id => fn infInfo =>
			 checkVarInfo path id infInfo (lookupEnv (VEofStr S') id)
			 handle Subscript => 
			     warnAdditional (DOTpath(path,id)) "value")
	   VE;
	   traverseEnv (fn id => fn infInfo =>
			checkModInfo path id infInfo (lookupEnv (MEofStr S') id)
			handle Subscript =>  
			    warnAdditional (DOTpath(path,id)) "structure")
	   ME;
	   traverseEnv (fn id => fn infInfo =>
			checkFunInfo path id infInfo (lookupEnv (FEofStr S') id)
			handle Subscript => 
			    warnAdditional (DOTpath(path,id)) "functor")
	   FE;
	   traverseEnv (fn id => fn infInfo =>
			checkSigInfo path id infInfo (lookupEnv (GEofStr S') id)
			handle Subscript => 
			    warnAdditional (DOTpath(path,id)) "signature")
	   GE)
       |  SEQstr (S1,S2) => (checkStr path S1 S'; checkStr path S2 S')
  and checkRecStr path RS (RECrec (RS1',RS2')) = 
	checkRecStr path RS RS2'
    | checkRecStr path (NONrec S) (NONrec S') = 
	checkStr path S S'
    | checkRecStr path (RECrec(RS1,RS2)) RS' = 
        checkRecStr path RS2 RS'
  and checkFun path (T,M,X) (T',M',X') = 
	checkExMod (RNGpath(path)) X X'
  and checkModInfo path id {qualid = _,info = RS} {qualid = _,info = RS'} =
        checkRecStr (DOTpath(path,id))  RS RS'
  and checkFunInfo path id {qualid = _,info = F} {qualid = _,info = F'} =
        checkFun  (DOTpath(path,id)) F F'
  and checkSigInfo path id {qualid = _,info = G} {qualid = _,info = G'} =
      ()
  and checkMod path M M' =
      case (M,M') of
	  (STRmod RS,STRmod RS') => checkRecStr path RS RS'
	|  (FUNmod F, FUNmod F') => checkFun path F F'
	|  (_,_) => fatalError "checkMod"
  and checkExMod path (EXISTSexmod(T,M)) (EXISTSexmod(T',M')) =
        checkMod path M M'
  and checkInfixBasis path infIBas specIBas =
      Hasht.apply
        (fn id => fn infInfo =>
	 ((ignore (Hasht.find specIBas id))
	  handle Subscript => 
	      warnAdditional (DOTpath(path,id)) "the infix status of"))
	infIBas
in

fun checkCSig infCSig specCSig = 
    if modeOfSig(specCSig) = STRmode then () 
    else
	case !(strOptOfSig specCSig) of 
	    NONE => fatalError "checkCSig"
	  | SOME RS => 
		let val RS' = NONrec (STRstr (mk1TopEnv (#uModEnv infCSig),
					      mk1TopEnv (#uFunEnv infCSig),
					      mk1TopEnv (#uSigEnv infCSig),
					      mk1TopEnv (#uTyEnv  infCSig),
					      mk1TopEnv (#uVarEnv infCSig)))
		in
		    checkRecStr UNITpath RS' RS; 
		    checkInfixBasis UNITpath (iBasOfSig (infCSig)) 
		                             (iBasOfSig (specCSig))
		end
end;



(* lookup with calculated position in 
   normed (sorted) structures and environments *)

fun sizeModInfo {qualid,info} = if isGlobalName qualid then 0 else 1
fun sizeFunInfo {qualid,info} = if isGlobalName qualid then 0 else 1
fun sizeVarInfo {qualid,info = (_,cs)} =
    if isGlobalName qualid 
	then 0
    else 
         (case cs of
               VARname _ => 1
             | EXNname ei =>
                   (* ps: if isExConStatic ei then 0 else *) 1
             | _ (* PRIMname _ | CONname _ | REFname *) => 0)


fun sizeOfModEnv env =
    foldEnv (fn _ => fn info => fn size => size + sizeModInfo info) 0 env
fun sizeOfFunEnv env = 
    foldEnv (fn _ => fn info => fn size => size + sizeFunInfo info) 0 env
fun sizeOfVarEnv env =
    foldEnv (fn _ => fn info => fn size => size + sizeVarInfo info) 0 env

fun sizeOfStr (STRstr(ME,FE,GE,TE,VE)) = sizeOfModEnv ME + sizeOfFunEnv  FE + sizeOfVarEnv VE
 |  sizeOfStr (SEQstr (S,S')) = sizeOfStr S + sizeOfStr S'


(* cvr: the position calculated by lookupMEofStr and lookupVEofStr will *only* be
   meaningfull if str is normed, but we return a result if it is not
   to support lookup in context where positions are irrelevant (ie, within signatures and
   and type expressions) *)

fun lookupMEofStr str =
    case str of 
	  STRstr(ME,FE,GE,TE,VE) => 
		(fn mid => lookupEnvWithPos sizeModInfo ME mid 0)
	| _ => fn mid => (0, lookupEnv (MEofStr str) mid)

fun lookupFEofStr str =
    case str of 
	 STRstr(ME,FE,GE,TE,VE) => 
	    let val sizeOfME = sizeOfModEnv ME in
		fn fid => lookupEnvWithPos sizeFunInfo FE fid sizeOfME 
	    end
	|  _ => fn fid => (0, lookupEnv (FEofStr str) fid)

fun lookupVEofStr str =
    case str of 
	 STRstr(ME,FE,GE,TE,VE) => 
	    let val sizeOfMEFE = (sizeOfModEnv ME) + (sizeOfFunEnv FE) in
		fn vid => lookupEnvWithPos sizeVarInfo VE vid sizeOfMEFE 
	    end
	| _ => fn vid => (0, lookupEnv (VEofStr str) vid)

fun lookupMEofEnv ((ME,_,_,_,_):Environment) =
    fn mid => lookupEnvWithPos sizeModInfo ME mid 0;


fun lookupFEofEnv ((ME,FE,_,_,_):Environment) =
    let val sizeOfME = sizeOfModEnv ME 
    in
	fn fid => lookupEnvWithPos sizeFunInfo FE fid sizeOfME
    end;
    
fun lookupVEofEnv ((ME,FE,_,VE,_):Environment) =
    let 
	val sizeOfMEFE = sizeOfModEnv ME + sizeOfFunEnv FE
    in
	fn vid => lookupEnvWithPos sizeVarInfo VE vid (sizeOfMEFE)
    end;


local fun warnToplevelImperativeVar desc id =
(
  msgIBlock 0;
  if !value_polymorphism then
      (errPrompt "Warning: Value polymorphism:";
       msgEOL();
       errPrompt ("Free type variable(s) at top level in "
		  ^desc^" identifier "^id))
  else
      errPrompt ("Warning: Free imperative type variable(s) at top level in "^desc^" identifier "^id);
  msgEOL();
  msgEBlock()
)
in
fun checkClosedExEnvironment  (EXISTS(T,(ME,FE,GE,VE,TE))) = 
  let val fvs = (map #1 (!free_variable_names)) 
  in
  ( foldEnv (fn id => fn {qualid, info = (sc,_)} => fn _ =>
                    (checkClosedTypeScheme fvs sc) 
                    handle Subscript => warnToplevelImperativeVar "value" id)
            () VE;
    foldEnv (fn id => fn (tyfun,_) => fn _ =>
                    (checkClosedTyFun fvs tyfun) 
                    handle Subscript => warnToplevelImperativeVar "type" id)
            () TE;
    foldEnv (fn id => fn {qualid, info = RS} => fn _ =>
                    (checkClosedRecStr fvs RS) 
                    handle Subscript => warnToplevelImperativeVar "structure" id)
            () ME;
    foldEnv (fn id => fn {qualid, info = F} => fn _ =>
                    (checkClosedGenFun fvs F) 
                    handle Subscript => warnToplevelImperativeVar "functor" id)
            () FE;
    foldEnv (fn id => fn {qualid, info = G} => fn _ =>
                    (checkClosedSig fvs G) 
                    handle Subscript => warnToplevelImperativeVar "signature" id)
            () GE)
  end
end;


