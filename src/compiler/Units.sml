
open Misc BasicIO Nonstdio Fnlib Config Mixture Const Globals Location;

(* Compiled signatures *)

type CSig =
{
  uMode:       Mode, (* whether to interpret the unit as a structure or topdec *)
  uName:       string,  (* the normalized basename of the filename *)
  uIdent:      string ref,  (* the (non-normalized) 
			   ML structure and signature identifier 
			   for the unit if uMode = STRmode *)
  uIBas:       (string,InfixStatus) Hasht.t,
  uVarEnv:     (string, VarInfo) Hasht.t,
  uTyEnv:      (string, TyInfo) Hasht.t, 
  uModEnv:     (string, ModInfo) Hasht.t,   
  uFunEnv:     (string, FunInfo) Hasht.t,   
  uSigEnv:     (string, SigInfo) Hasht.t,    
  (* uTyNameSet is the set of names introduced in the unit's implementation, 
     or the set of names bound in the unit's interface (if any).
  *)
  uTyNameSet:    TyNameSet ref,  
  (* The optional Str uStrOpt comes from the unit's optional interface.
     It is the body of the signature to be matched against.
   *)
  uStrOpt:     RecStr option ref, 
  uStamp:      SigStamp option ref,
                    (* present, if this signature comes from a .ui file *)
  uMentions:   (string, SigStamp) Hasht.t
};



fun iBasOfSig       (cu : CSig) = #uIBas cu
and varEnvOfSig     (cu : CSig) = #uVarEnv cu
and tyEnvOfSig      (cu : CSig) = #uTyEnv cu
and modEnvOfSig      (cu : CSig) = #uModEnv cu
and funEnvOfSig      (cu : CSig) = #uFunEnv cu
and sigEnvOfSig      (cu : CSig) = #uSigEnv cu
and tyNameSetOfSig   (cu : CSig) = #uTyNameSet cu
and strOptOfSig      (cu : CSig) = #uStrOpt cu
and modeOfSig   (cu : CSig) = #uMode cu
;

(* The table of unit signatures already loaded in memory *)

type SigTable = (string, CSig) Hasht.t;

fun mkSigTable() = (Hasht.new 37 : SigTable);
val dummySigTable = (Hasht.new 0 : SigTable);

val pervSigTable = (Hasht.new 7 : SigTable);

val currentSigTable = ref dummySigTable;

fun newSig nm id mode : CSig =
{
  uName = nm,
  uMode = mode,
  uIdent = ref id,
  uIBas = Hasht.new 7,
  uVarEnv     = Hasht.new 17,
  uTyEnv      = Hasht.new 7,
  uModEnv     = Hasht.new 7,
  uFunEnv     = Hasht.new 7,
  uSigEnv     = Hasht.new 7,
  uTyNameSet  = ref [],
  uStrOpt     = ref NONE,
  uMentions   = Hasht.new 13,
  uStamp      = ref NONE
};

(* the current order of unit signatures *)


(* Current signature *)

val dummySig = newSig "" "" STRmode;

val currentSig = ref dummySig;

val dummyInfixBasis = (Hasht.new 0 : (string, InfixStatus) Hasht.t);

val currentInfixBasis = ref dummyInfixBasis;

val currentTypeStamp = ref 0;
val currentExcStamp = ref 0;
val currentValStamp = ref 0;

val dummyRenEnv = (Hasht.new 0 : (string, int) Hasht.t);

val currentRenEnv = ref dummyRenEnv;

(* To load a signature from a file *)

(*
fun readSig name =
  let val filename = find_in_path (name ^ ".ui")
      val is = open_in_bin filename
  in
    let
      val sigStamp = input(is, 22)
      val () = if size sigStamp < 22 then raise Fail "sigStamp" else ()
      val cu = (input_value is : CSig)
      val {uStamp, uName, ...} = cu
    in
      close_in is;
      uStamp := SOME sigStamp;
      if name <> uName then (
        msgIBlock 0;
        errPrompt "File "; msgString filename;
        msgString " contains the signature of unit ";
        msgString uName; msgEOL();
        errPrompt "instead of the signature of unit ";
        msgString name; msgEOL();
        msgEBlock();
        raise Toplevel)
      else ();
      cu
    end
    handle Fail _ =>
      (close_in is;
       msgIBlock 0;
       errPrompt "Corrupted compiled signature file: ";
       msgString filename; msgEOL();
       msgEBlock();
       raise Toplevel)
  end;
*)
fun readSig filename =
  let 
      val name = normalizedUnitName(Filename.basename filename)
      val filename = find_in_path (filename ^ ".ui")
      val is = open_in_bin filename
  in
    let
      val sigStamp = input(is, 22)
      val () = if size sigStamp < 22 then raise Fail "sigStamp" else ()
      val cu = (input_value is : CSig)
      val {uStamp, uName, ...} = cu
    in
      close_in is;
      uStamp := SOME sigStamp;
      if name <> uName then (
        msgIBlock 0;
        errPrompt "File "; msgString filename;
        msgString " contains the signature of unit ";
        msgString uName; msgEOL();
        errPrompt "instead of the signature of unit ";
        msgString name; msgEOL();
        msgEBlock();
        raise Toplevel)
      else ();
      cu
    end
    handle Fail _ =>
      (close_in is;
       msgIBlock 0;
       errPrompt "Corrupted compiled signature file: ";
       msgString filename; msgEOL();
       msgEBlock();
       raise Toplevel)
  end;

fun readAndMentionSig filename =
  let val cu = readSig filename in
    (case !(#uStamp cu) of
         NONE => ()
       | SOME stamp =>
           let val mentions = #uMentions (!currentSig) in
             ignore(Hasht.find mentions (#uName(cu)))
             handle Subscript => Hasht.insert mentions (#uName(cu)) stamp
           end);
    cu
  end;

(* To find a pervasive signature by its name *)

fun findPervSig uname =
  Hasht.find pervSigTable uname
  handle Subscript =>
    fatalError "findPervSig"
;

(* To find a signature by its name *)

fun findSig loc uname =
  Hasht.find pervSigTable uname
  handle Subscript =>
    (Hasht.find (!currentSigTable) uname
     handle Subscript =>
       (if #uName(!currentSig) = "Top" then
          (ignore(Hasht.find (!watchDog) uname)
           handle Subscript =>
             errorMsg loc ("Cannot access unit " ^ uname ^
                           " before it has been loaded."))
        else ();
        let val cu =
          readSig uname
            handle Fail msg => errorMsg loc msg
        in
          Hasht.insert (!currentSigTable) uname cu; cu
        end))
;

(* --- The current state of the compiler --- *)

val pervasiveInfixTable =
  (Hasht.new 7 : (string, InfixStatus) Hasht.t);

val pervasiveInfixBasis = ref (NILenv : InfixBasis);
val pervasiveStaticT = ref ([]:TyNameSet);
val pervasiveStaticVE   = ref (NILenv : VarEnv);
val pervasiveStaticTE   = ref (NILenv : TyEnv);
val pervasiveStaticME   = ref (NILenv : ModEnv);
val pervasiveStaticFE   = ref (NILenv : FunEnv);
val pervasiveStaticGE   = ref (NILenv : SigEnv);

(* cvr: TODO at the moment there are no pervasive modules functors
   or signatures but there probably should be *) 
fun initPervasiveEnvironments() =
( pervasiveInfixBasis := mk1TopEnv pervasiveInfixTable;
  pervasiveStaticT    := [];
  pervasiveStaticVE   := NILenv;
  pervasiveStaticTE   := NILenv;
  pervasiveStaticME   := NILenv;
  pervasiveStaticFE   := NILenv;
  pervasiveStaticGE   := NILenv;
  List.app
    (fn uname =>
       let val cu = findPervSig uname in
	 pervasiveInfixBasis := bindTopInEnv (!pervasiveInfixBasis) (#uIBas cu);
         pervasiveStaticT :=
            (!pervasiveStaticT) @ (!(#uTyNameSet cu));
         pervasiveStaticVE :=
             bindTopInEnv (!pervasiveStaticVE) (#uVarEnv cu);
         pervasiveStaticTE :=
             bindTopInEnv (!pervasiveStaticTE) (#uTyEnv cu);
         pervasiveStaticME :=
             bindTopInEnv (!pervasiveStaticME) (#uModEnv cu);
         pervasiveStaticFE :=
             bindTopInEnv (!pervasiveStaticFE) (#uFunEnv cu);
         pervasiveStaticGE :=
             bindTopInEnv (!pervasiveStaticGE) (#uSigEnv cu)
       end)
    pervasiveOpenedUnits
);

(* Find and mention a signature *)

fun findAndMentionSig loc uname =
  let val cu = findSig loc uname in
    (case !(#uStamp cu) of
         NONE => ()
       | SOME stamp =>
           let val mentions = #uMentions (!currentSig) in
             ignore(Hasht.find mentions uname)
             handle Subscript => Hasht.insert mentions uname stamp
           end);
    cu
  end;

val initialInfixBasis  = ref (NILenv : InfixBasis);
val initialStaticT   = ref ([]:TyNameSet);
val initialStaticVE   = ref (NILenv : VarEnv);
val initialStaticTE   = ref (NILenv : TyEnv);
val initialStaticME   = ref (NILenv : ModEnv);
val initialStaticFE   = ref (NILenv : FunEnv);
val initialStaticGE   = ref (NILenv : SigEnv);

fun initInitialEnvironments context =
( initialInfixBasis   := !pervasiveInfixBasis;
  initialStaticT    := !pervasiveStaticT;
  initialStaticVE   := !pervasiveStaticVE;
  initialStaticTE   := !pervasiveStaticTE;
  initialStaticME   := !pervasiveStaticME;
  initialStaticFE   := !pervasiveStaticFE;
  initialStaticGE   := !pervasiveStaticGE;
  List.app
    (fn uname =>
       let val cu = findAndMentionSig nilLocation uname in
	 initialInfixBasis   := bindTopInEnv (!initialInfixBasis) (#uIBas cu);
         initialStaticT := (!initialStaticT) @ (!(#uTyNameSet cu));
         initialStaticTE := bindTopInEnv (!initialStaticTE) (#uTyEnv cu);
         initialStaticVE := bindTopInEnv (!initialStaticVE) (#uVarEnv cu);
         initialStaticME := bindTopInEnv (!initialStaticME) (#uModEnv cu);
         initialStaticFE := bindTopInEnv (!initialStaticFE) (#uFunEnv cu);
         initialStaticGE := bindTopInEnv (!initialStaticGE) (#uSigEnv cu)
       end)
    (!preopenedPreloadedUnits);
  List.app
    (fn filename =>
       let val cu = readAndMentionSig filename in
	   case #uMode cu of
	     STRmode => 
	        let val id = !(#uIdent cu)
		    val T = !(tyNameSetOfSig cu) 
		in
		   (case !(strOptOfSig cu) of
			NONE => ()
		      | SOME RS =>
			    (initialStaticGE := 
			     bindInEnv (!initialStaticGE) 
			               id
				       {qualid = {qual = "",id=[id]},
					info = (LAMBDAsig (T,
							   STRmod RS))}));
		    initialStaticT := (!initialStaticT) @ T;
		    initialStaticME :=
               	        bindInEnv (!initialStaticME) 
			          id 
				  {qualid = {qual = #uName cu,id=[]},
				   info = NONrec (STRstr(mk1TopEnv (modEnvOfSig cu),
							 mk1TopEnv(funEnvOfSig cu),
							 mk1TopEnv(sigEnvOfSig cu), (* should be NILenv *)
							 mk1TopEnv(tyEnvOfSig cu),
							 mk1TopEnv(varEnvOfSig cu)))}
		  end
	   | TOPDECmode =>
		 (initialInfixBasis := bindTopInEnv (!initialInfixBasis) (#uIBas cu);
		  initialStaticT := (!initialStaticT) @ (!(#uTyNameSet cu));
		  initialStaticTE := bindTopInEnv (!initialStaticTE) (#uTyEnv cu);
		  initialStaticVE := bindTopInEnv (!initialStaticVE) (#uVarEnv cu);
		  initialStaticME := bindTopInEnv (!initialStaticME) (#uModEnv cu);
		  initialStaticFE := bindTopInEnv (!initialStaticFE) (#uFunEnv cu);
		  initialStaticGE := bindTopInEnv (!initialStaticGE) (#uSigEnv cu))
       end)
    context
);

fun extendInitialSigEnv (SOME ({uMode = STRmode,
				uIdent = ref id, 
				uTyNameSet = ref T,
				uStrOpt = ref (SOME RS),
				...}:CSig)) =
    (initialStaticGE := bindInEnv (!initialStaticGE) 
                                  id
				  {qualid = {qual = "",id=[id]},
				   info = (LAMBDAsig (T,STRmod RS))})
| extendInitialSigEnv _ = ();



(* To put aside the current toplevel unit while compiling another unit. *)

fun protectCurrentUnit fct =
  let
    val saved_currentSigTable = !currentSigTable
    val saved_currentSig = !currentSig
    val saved_currentTypeStamp = !currentTypeStamp
    val saved_currentExcStamp = !currentExcStamp
    val saved_currentValStamp = !currentValStamp
    val saved_currentRenEnv = !currentRenEnv
    val saved_InfixBasis = !initialInfixBasis
    val saved_initialStaticT = !initialStaticT
    val saved_initialStaticVE = !initialStaticVE
    val saved_initialStaticTE = !initialStaticTE
    val saved_initialStaticME = !initialStaticME
    val saved_initialStaticFE = !initialStaticFE
    val saved_initialStaticGE = !initialStaticGE
  in
    (
    fct();
    currentSigTable := saved_currentSigTable;
    currentSig := saved_currentSig;
    currentTypeStamp := saved_currentTypeStamp;
    currentExcStamp := saved_currentExcStamp;
    currentValStamp := saved_currentValStamp;
    currentRenEnv := saved_currentRenEnv;
    initialInfixBasis := saved_InfixBasis;
    initialStaticT := saved_initialStaticT;
    initialStaticVE := saved_initialStaticVE;
    initialStaticTE := saved_initialStaticTE;
    initialStaticME := saved_initialStaticME;
    initialStaticFE := saved_initialStaticFE;
    initialStaticGE := saved_initialStaticGE
    )
    handle x =>
      (
      currentSigTable := saved_currentSigTable;
      currentSig := saved_currentSig;
      currentTypeStamp := saved_currentTypeStamp;
      currentExcStamp := saved_currentExcStamp;
      currentValStamp := saved_currentValStamp;
      currentRenEnv := saved_currentRenEnv;
      initialInfixBasis := saved_InfixBasis;
      initialStaticT := saved_initialStaticT;
      initialStaticVE := saved_initialStaticVE;
      initialStaticTE := saved_initialStaticTE;
      initialStaticME := saved_initialStaticME;
      initialStaticFE := saved_initialStaticFE;
      initialStaticGE := saved_initialStaticGE;
      raise x
      )
  end;

fun currentUnitName() =
  #uName(!currentSig)
;


fun mkGlobalName id =
  { qual = #uName(!currentSig), id = [id] }
;

fun isUnitName {qual, id} = id = [];

fun isGlobalName {qual, id} = not(qual = currentUnitName() orelse qual = "");

fun mkLocalName id = 
  { qual = "", id = [id]}
;
fun mkName onTop = 
  if onTop then mkGlobalName else mkLocalName;
;
fun mkGlobalInfo id info =
  { qualid = mkGlobalName id, info = info }
;

fun mkUniqueGlobalName (id, stamp) =
  ({ qual = #uName(!currentSig), id = [id] }, stamp)
;

fun newTypeStamp() =
(
  incr currentTypeStamp;
  !currentTypeStamp
);

fun newTyNameStamp() =
(
  incr currentTypeStamp;
  (currentUnitName (),!currentTypeStamp)
);

fun newExcStamp() =
(
  incr currentExcStamp;
  !currentExcStamp
);

fun newValStamp() =
(
  incr currentValStamp;
  !currentValStamp
);

(* Additions to the unit being compiled *)

fun add_global_info sel_fct id info =
  let val tbl = sel_fct (!currentSig) in
    Hasht.insert tbl id info
  end
;

(* cvr: modified to set local to global qualifiers *)
fun add_qualified_info sel_fct i (info as {qualid = {qual,id},info = info'}) =
  let val tbl = sel_fct (!currentSig) in
    if qual = "" orelse qual = currentUnitName() 
    then Hasht.insert tbl i {qualid={qual = currentUnitName(), id = id}, info = info'} 
    else Hasht.insert tbl i info 
  end
;


val add_InfixBasis  = add_global_info iBasOfSig
and add_VarEnv   = add_qualified_info varEnvOfSig
and add_TyEnv    = add_global_info tyEnvOfSig
and add_ModEnv    = add_qualified_info modEnvOfSig
and add_FunEnv    = add_qualified_info funEnvOfSig
and add_SigEnv    = add_qualified_info sigEnvOfSig
;

(* Additions to the unit being compiled *)
(* without redefining names that are already bound! *)

(* cvr: these functions should all be redundant now that
   signature elaboration is done properly *)

fun extend_InfixBasis id info =
  let val tbl = iBasOfSig (!currentSig) in
    (ignore (Hasht.find tbl id);
     msgIBlock 0;
     errPrompt "The fixity status of ";
     msgString id; msgString " cannot be redefined in a signature.";
     msgEOL();
     msgEBlock();
     raise Toplevel)
    handle Subscript =>
      (* Hasht.insert tbl id info *)
      add_InfixBasis id info
  end;

fun extend_VarEnv id info =
  let val tbl = varEnvOfSig (!currentSig) in
    (ignore (Hasht.find tbl id);
     msgIBlock 0;
     errPrompt "Value identifier ";
     msgString id; msgString " cannot be redefined in a signature.";
     msgEOL();
     msgEBlock();
     raise Toplevel)
    handle Subscript =>
      (* Hasht.insert tbl id info *)
      add_VarEnv id info
  end;

fun extend_TyEnv id info =
  let val tbl = tyEnvOfSig (!currentSig) in
    (ignore (Hasht.find tbl id);
     msgIBlock 0;
     errPrompt "Type constructor ";
     msgString id; msgString " cannot be redefined in a signature.";
     msgEOL();
     msgEBlock();
     raise Toplevel)
    handle Subscript =>
       Hasht.insert tbl id info
  end;

fun extend_ModEnv id info =
  let val tbl = modEnvOfSig (!currentSig) in
    (ignore (Hasht.find tbl id);
     msgIBlock 0;
     errPrompt "Module identifier ";
     msgString id; msgString " cannot be redefined in a signature.";
     msgEOL();
     msgEBlock();
     raise Toplevel)
    handle Subscript =>
      (* Hasht.insert tbl id info *)
      add_ModEnv id info
  end;

fun extend_FunEnv id info =
  let val tbl = funEnvOfSig (!currentSig) in
    (ignore (Hasht.find tbl id);
     msgIBlock 0;
     errPrompt "Functor identifier ";
     msgString id; msgString " cannot be redefined in a signature.";
     msgEOL();
     msgEBlock();
     raise Toplevel)
    handle Subscript =>
  (*  Hasht.insert tbl id info *)
      add_FunEnv id info
  end;

fun extend_SigEnv id info =
  let val tbl = sigEnvOfSig (!currentSig) in
    (ignore (Hasht.find tbl id);
     msgIBlock 0;
     errPrompt "Signature identifier";
     msgString id; msgString " cannot be redefined in a signature.";
     msgEOL();
     msgEBlock();
     raise Toplevel)
    handle Subscript =>
  (*  Hasht.insert tbl id info *)
      add_SigEnv id info
  end;


(* We have to compare the whole qualids, because in exported *)
(* TyNames all stamps are reset to 0. Therefore, different   *)
(* exported TyNames may have equal stamps. *)

fun isEqTN (tn1 : TyName) (tn2 : TyName) =
 (*  #qualid tn1 = #qualid tn2 andalso*)
  #tnStamp (!(#info tn1)) = #tnStamp (!(#info tn2))
;

fun updateCurrentInfixBasis iBas =
  traverseEnv add_InfixBasis (revEnv iBas)
;


(* cvr: added *)
local
fun updateTyName ({qualid,info}:TyName) = 
  let val { tnStamp, 
	    tnKind, 
	    tnEqu, 
	    tnSort, 
	    tnLevel,
            tnConEnv} = !info in
      info := {tnStamp=tnStamp, 
	       tnKind=tnKind, 
	       tnEqu=tnEqu, 
	       tnSort=tnSort, 
	       tnLevel=0, (* update the level *)
	       tnConEnv = tnConEnv}
  end;    
in
fun updateCurrentStaticT T = 
  (app updateTyName T;
   tyNameSetOfSig (!currentSig) := (!(tyNameSetOfSig (!currentSig))) @ T)
end;

fun extendCurrentStaticS S = 
    let val strOpt = strOptOfSig (!currentSig) 
    in (* cvr: TODO check for duplicate specs? *)
    strOpt := (case !strOpt of
                 NONE => SOME (NONrec S)
	      |  SOME (NONrec S') => SOME (NONrec (SEQstr (S',S)))
	      |  SOME (RECrec _) => fatalError "extendCurrentStaticS")
    end;

fun extendCurrentStaticIBas iBas =
  traverseEnv extend_InfixBasis (revEnv iBas)
;

fun extendCurrentStaticVE VE =
  traverseEnv extend_VarEnv (revEnv VE)
;

fun updateCurrentStaticVE VE =
  traverseEnv add_VarEnv (revEnv VE)
;

fun updateCurrentStaticTE TE =
  traverseEnv add_TyEnv (revEnv TE)
;

fun extendCurrentStaticTE TE =
  traverseEnv extend_TyEnv (revEnv TE)
;

fun extendCurrentStaticME ME =
  traverseEnv extend_ModEnv (revEnv ME)
;

fun updateCurrentStaticME ME =
  traverseEnv add_ModEnv (revEnv ME)
;

fun extendCurrentStaticFE FE =
  traverseEnv extend_FunEnv (revEnv FE)
;

fun updateCurrentStaticFE FE =
  traverseEnv add_FunEnv (revEnv FE)
;

fun extendCurrentStaticGE GE =
  traverseEnv extend_SigEnv (revEnv GE)
;

fun updateCurrentStaticGE GE =
  traverseEnv add_SigEnv (revEnv GE)
;

(*
fun mkGlobalInfixBasis() =
  bindTopInEnv pervasiveInfixBasis (!currentInfixBasis)
;
*)

fun mkGlobalInfixBasis() =
  bindTopInEnv (!initialInfixBasis) (#uIBas (!currentSig));
;

fun mkGlobalT() = (!initialStaticT) @ (!(#uTyNameSet (!currentSig)))

fun mkGlobalVE() =
  bindTopInEnv (!initialStaticVE) (#uVarEnv (!currentSig))
;

fun mkGlobalTE() =
  bindTopInEnv (!initialStaticTE) (#uTyEnv (!currentSig))
;


fun mkGlobalME() =
  bindTopInEnv (!initialStaticME) (#uModEnv (!currentSig))
;
fun mkGlobalFE() =
  bindTopInEnv (!initialStaticFE) (#uFunEnv (!currentSig))
;
fun mkGlobalGE() =
  bindTopInEnv (!initialStaticGE) (#uSigEnv (!currentSig))
;

fun execToplevelOpen loc uname =
  let val cu = findAndMentionSig loc uname in
    updateCurrentInfixBasis (mk1TopEnv (#uIBas cu));
    updateCurrentStaticT  (!(#uTyNameSet cu));
    updateCurrentStaticVE (mk1TopEnv (#uVarEnv cu));
    updateCurrentStaticTE (mk1TopEnv (#uTyEnv cu));
    updateCurrentStaticME (mk1TopEnv (#uModEnv cu));
    updateCurrentStaticFE (mk1TopEnv (#uFunEnv cu));
    updateCurrentStaticGE (mk1TopEnv (#uSigEnv cu))
  end;

fun print_qual "" = () 
  | print_qual qual = (msgString qual; msgString ".");

fun print_id [] = ()
  | print_id [i] = msgString i
  | print_id (i::id) = 
            (print_id id;  msgString "." ; msgString i);

fun printHiddenId id =
  (msgString "?{"; print_id id; msgString "}")
;
(* cvr: revise printVQ to deal with real long ids*)

fun printVQ q =
  let val {qual, id} = q
      fun printHidden() =
            if qual <> #uName(!currentSig) then
              (print_qual qual; 
               print_id id)
            else
              printHiddenId id
  in
      (if #qual(#qualid (lookupEnv (mkGlobalVE()) (hd id))) = qual then 
        print_id id
      else
        printHidden())
    handle Subscript =>
      printHidden()
  end;


(* cvr: TODO see above *)

fun mkInfixBasis() = (Hasht.new 13 : (string, InfixStatus) Hasht.t);
fun mkRenEnv() = (Hasht.new 113 : (string, int) Hasht.t);

fun startCompilingUnit uname uident umode =
(
  currentSigTable := mkSigTable();
  currentSig := newSig uname uident umode;
  currentInfixBasis := mkInfixBasis();
  currentTypeStamp := 0;
  currentExcStamp := 0;
  currentValStamp := 0;
  currentRenEnv := mkRenEnv()
);

fun rectifyVarEnv VE =
  let
    val excRen = ref( [] : (QualifiedIdent * (QualifiedIdent * int)) list )
  in
  (* Hasht.apply (fn id => fn {qualid, info = (sc,status)} => 
          case status of 
            EXNname ei =>
              (case #exconTag(!ei) of
                   NONE => () 
                 | SOME (name, stamp) =>
                     if #qual(qualid) = #uName(!currentSig) then
                       excRen := (qualid, (name, stamp)) :: !excRen
                     else ()
                     )
          | _ => ())
      VE; *)
    (!excRen)
  end;


fun rectifySignature() =
  let 
      val excRenList = rectifyVarEnv (#uVarEnv(!currentSig))
      val valRenList =
        foldEnv (fn id => fn stamp => fn acc => (id,stamp)::acc)
                [] (mk1TopEnv (!currentRenEnv))
  in
    currentRenEnv := dummyRenEnv;
    (excRenList, valRenList)
  end;





















