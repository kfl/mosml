(* Sigmtch.sml *)

open List Fnlib Mixture Const Prim Lambda Globals Units Types;
open Front Back Emit_phr;

(* cvr: TODO these error messages are redundant as they are
   now detected and reported in Types.sml *)

fun errorImplMismatch id =
(
  msgIBlock 0;
  errPrompt "Mismatch between the specification of the value ";
  msgString id; msgEOL();
  errPrompt "in the signature and its implementation in the unit body";
  msgEOL();
  msgEBlock();
  raise Toplevel
);

fun errorConImplMismatch id =
(
  msgIBlock 0;
  errPrompt "Mismatch between the specification of the value constructor ";
  msgString id; msgEOL();
  errPrompt "in the signature and its implementation in the unit body";
  msgEOL();
  msgEBlock();
  raise Toplevel
);

fun errorExConImplMismatch id =
(
  msgIBlock 0;
  errPrompt "Mismatch between the specification of the exception constructor ";
  msgString id; msgEOL();
  errPrompt "in the signature and its implementation in the unit body";
  msgEOL();
  msgEBlock();
  raise Toplevel
);


fun exportValAsVal os valRenList id (infStatus : ConStatus) (specStatus : ConStatus) =
  let val vid =  Const.mangle (Const.ValId id)
      val lam = Lprim(Pset_global (#qualid specStatus, 0),
		      [Lprim(Pget_global (#qualid infStatus, 0), [])])
  in emit_phrase os (compileLambda true lam);
     drop (fn (name,stamp) => name = vid) valRenList
  end
;

fun exportPrimAsVal os valRenList id (pi : PrimInfo) (specStatus : ConStatus) =
  let val vid =  Const.mangle (Const.ValId id)
      val lam = Lprim(Pset_global (#qualid specStatus, 0), 
		      [trPrimVar (#primOp pi)])
  in emit_phrase os (compileLambda true lam);
     drop (fn (name,stamp) => name = vid) valRenList
  end
;

fun exportConAsVal os valRenList id (ci : ConInfo) (specStatus : ConStatus) =
  let val vid =  Const.mangle (Const.ValId id)
      val lam = Lprim(Pset_global (#qualid specStatus, 0), 
		      [trConVar ci])
  in emit_phrase os (compileLambda true lam);
     drop (fn (name,stamp) => name = vid) valRenList
  end
;

fun exportExConAsVal os valRenList id (ei : ExConInfo) (infStatus : ConStatus) (specStatus : ConStatus) =
    let val vid =  Const.mangle (Const.ValId id) 
	val en = Lprim(Pget_global(#qualid infStatus,lookup vid valRenList),[])
	val lam = Lprim(Pset_global (#qualid specStatus, 0), 
			[trTopDynExConVar ei en])
    in
	emit_phrase os (compileLambda true lam);
	drop (fn (name,stamp) => name = vid) valRenList
    end;

fun checkHomeUnits infQual specQual id thing =
  if specQual <> infQual then (
    msgIBlock 0;
    errPrompt "Specified signature expects the ";
    msgString thing; msgString " ";
    msgString id; msgString " to be defined"; msgEOL();
    errPrompt "in the unit "; msgString specQual;
    msgString " but it is defined in the unit ";
    msgString infQual; msgEOL();
    msgEBlock();
    raise Toplevel)
  else ();
;

(* cvr: TODO simplify to remove error checking (now done during matching in Types.sml) *)

fun exportVar os valRenList id {info = (_,infInfo),qualid = infQualid} 
                     {info = (_,specInfo),qualid = specQualid} =
  let
      val {qual=infQual, ...} = infQualid
      val {qual=specQual, ...} = specQualid 
      val infStatus = {qualid = infQualid, info = infInfo}
      val specStatus = {qualid = specQualid, info = specInfo}
  in
    case specInfo of
        VARname ovltype =>
          (* checkHomeUnits infQual specQual id "value"; *)
          (case infInfo of
               VARname ovltype' =>
                 (if ovltype <> ovltype' then errorImplMismatch id
                  else ();
                  if specQual <> infQual then
                    exportValAsVal os valRenList id infStatus specStatus
                  else valRenList)
             | PRIMname pi' =>
                   exportPrimAsVal os valRenList id  pi' specStatus
             | CONname ci' =>
                   exportConAsVal os valRenList id  ci' specStatus
             | EXNname ei' =>
                   exportExConAsVal os valRenList id  ei' infStatus specStatus
             | REFname => errorImplMismatch id)
      | PRIMname pi =>
          (* checkHomeUnits infQual specQual id "prim_value"; *)
          (case infInfo of
               VARname ovltype' => errorImplMismatch id
             | PRIMname pi'=>
                 if pi <> pi' then errorImplMismatch id
		 else valRenList
             | CONname ci' => errorImplMismatch id
             | EXNname ei' => errorImplMismatch id
             | REFname => errorImplMismatch id)
      | CONname ci =>
          (* checkHomeUnits infQual specQual id "value constructor"; *)
          (case infInfo of
               VARname ovltype' => errorImplMismatch id
             | PRIMname pi' => errorImplMismatch id
             | CONname ci' =>
                 if #conArity(!ci) <> #conArity(!ci')
                 orelse #conIsGreedy(!ci) <> #conIsGreedy(!ci')
                 orelse #conTag(!ci) <> #conTag(!ci')
                 orelse #conSpan(!ci) <> #conSpan(!ci')
                 then errorConImplMismatch id
                 else valRenList
             | EXNname ei' => errorImplMismatch id
             | REFname => errorImplMismatch id)
      | EXNname ei =>
          ( 
        (* cvr: TODO review whether call to checkHomeUnits still makes sense
         *)
           checkHomeUnits infQual specQual id "exception"; 
           case infInfo of
               VARname ovltype' => errorImplMismatch id
             | PRIMname pi' => errorImplMismatch id
             | CONname ci' => errorImplMismatch id
             | EXNname ei' =>
                 if #exconArity(!ei) <> #exconArity(!ei')
                 then errorExConImplMismatch id
                 else valRenList
             | REFname => errorImplMismatch id)
      | REFname =>
          (case infInfo of
               VARname ovltype' => errorImplMismatch id
             | PRIMname pi' => errorImplMismatch id
             | CONname ci' => errorImplMismatch id
             | EXNname ei' => errorImplMismatch id
             | REFname => valRenList)
  end
;
 
fun exportMod os valRenList id 
    {info = RS,qualid = infQualid} {info = RS',qualid = specQualid} =
  (* cvr: TODO optimize for null coercions *)
  let val {qual=infQual, id=_} = infQualid
      val {qual=specQual,id=_} = specQualid 
  in
  let val mid = Const.mangle (Const.ModId id)
      val strlam = Lprim(Pget_global({qual=infQual,id =[mid]},
					 lookup mid valRenList),
		      [])
      val lam =
	  Lprim(Pset_global ({qual=specQual,id = [mid]}, 0),
		[coerceRecStr strlam RS RS'])
  in  
      (*   msgIBlock 0; Pr_lam.printLam lam; msgEOL(); msgEBlock(); (* cvr: TODO remove*) *)
      emit_phrase os (compileLambda true lam);
      drop (fn (name,stamp) => name = mid) valRenList
  end
  end
;


(* cvr: the following is redundant until we allow functor signatures in
   unit signatures.
*)
fun exportGenFun os valRenList id 
          {info = F,qualid = infQualid} {info = F',qualid = specQualid} =
  (* cvr: TODO optimize for null coercions *)
  let val {qual=infQual, id=_} = infQualid
      val {qual=specQual,id=_} = specQualid 
  in
  let val fid = Const.mangle (Const.FunId id)
      val funlam = Lprim(Pget_global({qual=infQual, id = [fid]},
					 lookup fid valRenList),
			 [])
      val lam =
	  Lprim(Pset_global ({qual=specQual,id = [fid]}, 0),
		[coerceFun funlam F F'])
  in emit_phrase os (compileLambda true lam);
     drop (fn (name,stamp) => name = fid) valRenList
  end
  end
;


fun matchStamps (inferredSig : CSig) (specSig : CSig) =
  Hasht.apply
    (fn uname => fn stamp =>
      let val stamp' = Hasht.find (#uMentions inferredSig) uname in
        if stamp' <> stamp then (
          msgIBlock 0;
          errPrompt "The signature of "; msgString uname;
          msgString " has changed, while "; msgString (#uName specSig);
          msgString ".sig depends on it."; msgEOL();
          errPrompt "Please, recompile "; msgString (#uName specSig);
          msgString ".sig, before compiling "; msgString (#uName specSig);
          msgString ".sml."; msgEOL();
          msgEBlock();
          raise Toplevel)
        else ()
      end
      handle Subscript => ())
    (#uMentions specSig)
;

(*
fun matchSignature os valRenList (inferredSig : CSig) (specSig : CSig) =
( case !(strOptOfSig specSig) of 
      NONE => fatalError "matchSignature"
    | SOME S =>
  (* Matching stamps of mentioned signatures *)
  (matchStamps inferredSig specSig;
  (* Type realization. *)
  let val LAMBDAsig(T,STRmod S) = 
            copySig [] [] (LAMBDAsig(!(tyNameSetOfSig specSig),STRmod S))
      val S' = STRstr (mk1TopEnv (#uModEnv inferredSig),
                       mk1TopEnv (#uTyEnv  inferredSig),
		       mk1TopEnv (#uVarEnv inferredSig))

  in
      refreshTyNameSet VARIABLEts T;
      (matchStr [currentUnitName()] S' S 
	      handle MatchError matchReasons => 
		  (msgIBlock 0;
		   errPrompt "Signature mismatch: \
		    \the unit does not match the signature ...";
		   msgEOL();
		   msgEBlock();
		   errMatchReasons matchReasons;
		   raise Toplevel));
     (* Status matching. *)
     (* This may cause some code to be generated, *)
     (* if a primitive function or a value constructor is *)
     (* exported as a value. *)
      Hasht.apply (fn id => fn specSc =>
		       exportVar os id 
		          (Hasht.find (varEnvOfSig inferredSig) id) specSc)
                  (varEnvOfSig specSig);
     (* Module matching. *)
     (* This may cause some coercion code to be generated. *)
      Hasht.apply (fn id => fn specInfo =>
		     exportMod os valRenList id 
		         (Hasht.find (modEnvOfSig inferredSig) id) specInfo)
                   (modEnvOfSig specSig);
     (* Generative functor matching. *)
     (* This may cause some coercion code to be generated. *)
      (* cvr: the following is redundant until we allow functor signatures in
              unit signatures. 
      *)
      Hasht.apply (fn id => fn specInfo =>
		     exportGenFun os valRenList id 
		         (Hasht.find (funEnvOfSig inferredSig) id) specInfo)
                   (funEnvOfSig specSig)
end)
);
*)

fun matchSignature os valRenList (inferredSig : CSig) (specSig : CSig) =
( case !(strOptOfSig specSig) of 
      NONE => fatalError "matchSignature"
    | SOME S =>
  (* Matching stamps of mentioned signatures *)
  (matchStamps inferredSig specSig;
  (* Type realization. *)
  let val LAMBDAsig(T,STRmod (NONrec S)) = 
            copySig [] [] (LAMBDAsig(!(tyNameSetOfSig specSig),STRmod (NONrec S)))
      val S' = STRstr (mk1TopEnv (#uModEnv inferredSig),
                       mk1TopEnv (#uFunEnv inferredSig),
                       mk1TopEnv (#uTyEnv  inferredSig),
		       mk1TopEnv (#uVarEnv inferredSig))

  in
      refreshTyNameSet VARIABLEts T;
      (matchStr S' S 
	      handle MatchError matchReason => 
		  (msgIBlock 0;
		   errPrompt "Signature mismatch: \
		    \the unit does not match the signature ...";
		   msgEOL();
		   msgEBlock();
		   errMatchReason "unit" "signature" matchReason;
		   raise Toplevel));
      let (* Status matching. *)
	  (* This may cause some code to be generated, *)
	  (* if a primitive function or a value constructor is *)
	  (* exported as a value. *)
	  val valRenList = 
	    Hasht.fold (fn id => fn specSc => fn valRenList =>
		        exportVar os valRenList id
			(Hasht.find (varEnvOfSig inferredSig) id) specSc)
	    valRenList (varEnvOfSig specSig);
	  (* Module matching also may cause some coercion code
             to be generated. *)
	  val valRenList = 
	    Hasht.fold (fn id => fn specInfo => fn valRenList =>
			exportMod os valRenList id 
			(Hasht.find (modEnvOfSig inferredSig) id) specInfo)
	    valRenList (modEnvOfSig specSig);
          (* functor matching may cause some coercion code to be generated. *)
	  (* cvr: the following is redundant until we allow 
                  functor signatures in unit signatures. *)
          val valRenList = 
 	    Hasht.fold (fn id => fn specInfo => fn valRenList =>
			exportGenFun os valRenList id 
			(Hasht.find (funEnvOfSig inferredSig) id) specInfo)
	    valRenList (funEnvOfSig specSig)
      in valRenList 
      end
end)
);




