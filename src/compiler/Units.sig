local
  open Fnlib Mixture Const Globals Location;
in

type CSig =
{
  uName:       string,
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
  uStrOpt:     Str option ref, 
  uStamp:      SigStamp option ref,
                    (* present, if this signature comes from a .ui file *)
  uMentions:   (string, SigStamp) Hasht.t
};

val varEnvOfSig : CSig -> (string, (TypeScheme * ConStatusDesc)global) Hasht.t;
val tyEnvOfSig  : CSig -> (string, (TyFun * ConEnv)) Hasht.t;
val modEnvOfSig : CSig -> (string, Str global) Hasht.t;
val funEnvOfSig : CSig -> (string, GenFun global) Hasht.t;
val sigEnvOfSig : CSig -> (string, Sig global) Hasht.t;
val tyNameSetOfSig : CSig -> TyNameSet ref;
val strOptOfSig  :  CSig -> Str option ref;


type SigTable = (string, CSig) Hasht.t;

val pervSigTable : SigTable;
val currentSigTable : SigTable ref;
val newSig : string -> CSig;
val currentSig : CSig ref;
val currentRenEnv : (string, int) Hasht.t ref;
val hasSpecifiedSignature : bool ref;
val readSig : string -> CSig;
val findSig : Location -> string -> CSig;
val pervasiveInfixTable : (string, InfixStatus) Hasht.t;
val initPervasiveEnvironments : unit -> unit;
val findAndMentionSig : Location -> string -> CSig;
val initInitialEnvironments : unit -> unit;
val protectCurrentUnit : (unit -> 'a) -> unit;
val currentUnitName : unit -> string;
val mkGlobalName : string -> QualifiedIdent;
val isUnitName : QualifiedIdent -> bool;
val isGlobalName : QualifiedIdent -> bool;
val mkLocalName : string -> QualifiedIdent;
val mkName : bool -> string -> QualifiedIdent;

val mkGlobalInfo : string -> 'a -> 'a global;
val mkUniqueGlobalName : string * 'a -> QualifiedIdent * 'a;
val newTypeStamp : unit -> int;

val newTyNameStamp : unit -> (string*int);
val newExcStamp : unit -> int;
val newValStamp : unit -> int;


val isEqTN : TyName -> TyName -> bool;
val updateCurrentInfixBasis : (string, InfixStatus) Env -> unit;


val updateCurrentStaticT : TyNameSet -> unit;

val updateCurrentStaticVE : VarEnv -> unit;
val extendCurrentStaticVE : VarEnv -> unit; 
val updateCurrentStaticTE : TyEnv -> unit;
val extendCurrentStaticTE : TyEnv -> unit;
val updateCurrentStaticME : ModEnv -> unit;
val extendCurrentStaticME : ModEnv -> unit;
val updateCurrentStaticFE : FunEnv -> unit;
val extendCurrentStaticFE : FunEnv -> unit;
val updateCurrentStaticGE : SigEnv -> unit;
val extendCurrentStaticGE : SigEnv -> unit;

val extendCurrentStaticS : Str -> unit;


val mkGlobalInfixBasis : unit -> (string, InfixStatus) Env;

val mkGlobalT : unit -> TyNameSet;
val mkGlobalVE : unit -> VarEnv;
val mkGlobalTE : unit -> TyEnv;
val mkGlobalME : unit -> ModEnv;
val mkGlobalFE : unit -> FunEnv;
val mkGlobalGE : unit -> SigEnv;

val execToplevelOpen : Location -> string -> unit;
val printVQ : QualifiedIdent -> unit;

val startCompilingUnit : string -> unit;
val rectifySignature : unit ->
  (QualifiedIdent * (QualifiedIdent * int)) list * (string * int) list;


end;











