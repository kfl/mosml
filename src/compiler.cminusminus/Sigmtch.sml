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
          (case infInfo of
               VARname ovltype' => errorImplMismatch id
             | PRIMname pi'=>
                 if pi <> pi' then errorImplMismatch id
		 else valRenList
             | CONname ci' => errorImplMismatch id
             | EXNname ei' => errorImplMismatch id
             | REFname => errorImplMismatch id)
      | CONname ci =>
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
          (case infInfo of
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

fun exportGenFun os valRenList id 
          {info = F,qualid = infQualid} {info = F',qualid = specQualid} =
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


fun matchModes (inferredSig : CSig) (specSig : CSig) =
  case (modeOfSig inferredSig,modeOfSig specSig) of
      (STRmode,STRmode) => 
	 if !(#uIdent(inferredSig)) = !(#uIdent(specSig))
	     then ()
	 else
         (msgIBlock 0;
          errPrompt "Identifier mismatch: the implementation of ";
	  msgString (#uName inferredSig);msgEOL();
          errPrompt "was compiled as the declaration of the structure ";msgEOL();
	  errPrompt "  "; msgString (!(#uIdent inferredSig));msgEOL();
          errPrompt "but its interface was compiled as the declaration of the signature"; msgEOL();
	  errPrompt "  "; msgString (!(#uIdent specSig));msgEOL();
	  errPrompt "The declarations should agree on the identifier";
	  msgEOL();	  
          msgEBlock();
          raise Toplevel)
  |   (TOPDECmode,TOPDECmode) => ()
  |   (STRmode,TOPDECmode) =>
         (msgIBlock 0;
          errPrompt "Mode mismatch: the implementation of ";
	  msgString (#uName inferredSig);msgEOL();
          errPrompt "was compiled as a structure declaration"; msgEOL();
          errPrompt "but its interface was compiled as a sequence of top level specifications"; msgEOL();
          errPrompt "The implementation and its interface must be compiled in the same mode"; msgEOL();
          msgEBlock();
          raise Toplevel)
  |   (TOPDECmode,STRmode) =>
         (msgIBlock 0;
          errPrompt "Mode mismatch: the implementation of "; 
	  msgString (#uName inferredSig);msgEOL();
          errPrompt "was compiled as a sequence of top level declarations";msgEOL();
          errPrompt "but its interface was compiled as a signature declaration"; msgEOL();
          errPrompt "The implementation and its interface must be compiled in the same mode"; msgEOL();
          msgEBlock();
          raise Toplevel)
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

fun matchSignature os valRenList (inferredSig : CSig) (specSig : CSig) =
  ((* Matching compilation modes *)
   matchModes inferredSig specSig;
   (* Matching stamps of mentioned signatures *)
   matchStamps inferredSig specSig;
  (* Matching of components *)
   (matchCSig  inferredSig specSig
    handle MatchError matchReason => 
	(msgIBlock 0;
	 errPrompt "Interface mismatch: the implementation of unit ";msgString (#uName inferredSig);msgEOL();
	 errPrompt "does not match its interface, because ... ";
	 msgEOL();
	 msgEBlock();
	 errMatchReason "implementation" "interface" matchReason;
	 raise Toplevel));
  (* warn of any un(der)specified (co-variant) declarations in a topdec unit *)
  checkCSig inferredSig specSig;
  (* coercions *)
  let 
      (* value matching may cause some code to be generated, *)
      (* if a primitive function or a value constructor is *)
      (* exported as a value. *)
      val valRenList = 
	  Hasht.fold (fn id => fn specSc => fn valRenList =>
		      exportVar os valRenList id
		      (Hasht.find (varEnvOfSig inferredSig) id) specSc)
	  valRenList (varEnvOfSig specSig);
      (* structure matching may cause some coercion code to be generated. *)
      val valRenList = 
	  Hasht.fold (fn id => fn specInfo => fn valRenList =>
		      exportMod os valRenList id 
		      (Hasht.find (modEnvOfSig inferredSig) id) specInfo)
	  valRenList (modEnvOfSig specSig);
      (* functor matching may cause some coercion code to be generated. *)
      val valRenList = 
	  Hasht.fold (fn id => fn specInfo => fn valRenList =>
		      exportGenFun os valRenList id 
		      (Hasht.find (funEnvOfSig inferredSig) id) specInfo)
	  valRenList (funEnvOfSig specSig)
  in valRenList 
  end)
;




