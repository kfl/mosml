local
  open Fnlib Mixture Const Smlprim Globals Location Units;
in

(* cvr: operations on structures *)
val SofRecStr : RecStr -> Str;
val MEofStr : Str -> ModEnv;
val FEofStr : Str -> FunEnv;
val GEofStr : Str -> SigEnv;
val VEofStr : Str -> VarEnv;
val TEofStr : Str -> TyEnv;

val removeGEofStr : Str -> Str;
val removeGEofRecStr : RecStr -> RecStr;

val VEofCE : ConEnv -> VarEnv;

datatype ScopeViolation = 
    TYNAMEsv of TyName
  | TYPEVARsv of TypeVar;

type matchReason;

datatype reason = 
    UnifyCircular | UnifyEquality | UnifyExplicit
  | UnifyTup | UnifyRec of Lab | UnifyOther
  | UnifyMod of matchReason option * matchReason option
  | UnifyScope of TypeVar * ScopeViolation
;

exception MatchError of matchReason;

exception Unify of reason;

val tyname_unit     : TyName;
val tyname_bool     : TyName;
val tyname_int      : TyName;
val tyname_word     : TyName;
val tyname_word8    : TyName;
val tyname_char     : TyName;
val tyname_real     : TyName;
val tyname_string   : TyName;
val tyname_substring: TyName; 
val tyname_syserror : TyName;
val tyname_list     : TyName;
val tyname_vector   : TyName;
val tyname_ref      : TyName;
val tyname_exn      : TyName;
val tyname_option   : TyName;
val tyname_order    : TyName;
val tyname_frag     : TyName;
val tyname_ppstream : TyName;

val mkConInfo : unit -> ConInfo;
val mkExConInfo : unit -> ExConInfo;
val mkPrimInfo : int -> SMLPrim -> PrimInfo;

val normType: Type -> Type;
val normTyApp: TyApp -> TyFun; 
val normTyFun: TyFun -> TyFun; 

val normRecStr : RecStr -> RecStr;
val normStr : Str -> Str;
val normMod : Mod -> Mod;
val normExMod : ExMod -> ExMod;

val savePrState : unit -> (unit -> unit);
val under_binder : ('a -> 'b) -> 'a -> 'b;

val checkpoint_free_typevar_names: unit -> unit;
val rollback_free_typevar_names: unit -> unit;
val commit_free_typevar_names: unit -> unit;
val prTypeVar: TypeVar -> unit;
val prTyName : bool -> TyName -> unit;
val prTyNameSet : TyNameSet -> string -> unit;
val prTyApp : int -> TyApp -> unit;
val prType : Type -> unit;
val prTypeScheme : TypeScheme -> unit
val prMod : Mod -> unit;
val prSig : Sig -> unit;
val prModInfo : string -> RecStr global -> unit;
val prFunInfo : string -> GenFun global -> unit;
val prSigInfo : string -> Sig global -> unit;
val prInfixStatus : string -> InfixStatus -> unit;
val prVarInfo : ((TypeScheme * ConStatusDesc) global -> unit) -> 
    string -> (TypeScheme * ConStatusDesc) global -> unit
val prTyInfo : string -> (TyFun * ConEnv) -> unit;
(* val prGenFun : GenFun -> unit; *)
val prTyFun : TyFun -> unit;

val resetTypePrinter: unit -> unit;
val collectExplicitVars: Type -> unit;
val collectTopVars: ExEnvironment -> unit;
val printNextType: Type -> unit;
val printType: Type -> unit;
val printScheme: TypeScheme -> unit;
val collectSchemeExplicitVars: TypeScheme -> unit;
val printNextScheme: TypeScheme -> unit;



val newExplicitTypeVar: string -> TypeVar;
val mkTypeVar   : bool -> bool -> bool -> int -> TypeVar;
val newTypeVar  : bool -> bool -> bool -> TypeVar;
val newTypeVars : int -> TypeVar list;
val newUnknown  : unit -> Type;
val isExplicit  : TypeVar -> bool;

val TypeOfTypeVar : TypeVar -> Type;
val fresh3DotType : unit -> RowType;
val contentsOfRowType : RowType -> Type Row * bool;
val isTupleType   : Type -> bool; 

val kindTyName : TyName -> Kind; 
val kindTyApp : TyApp -> Kind; 
val kindTyFun : TyFun -> Kind; 

val etaExpandTyApp : TyApp -> TyFun;
val freeVarsTyStr : 
    TyName list -> TypeVar list -> 
    (TyName list * TypeVar list * RowVar list) -> 
    TyStr -> 
    (TyName list * TypeVar list * RowVar list)
val unify: Type -> Type -> unit;
val unifyTyApp: TyApp -> TyApp -> unit; 
val unifyTyFun: TyFun -> TyFun -> unit; 
val equalsTyFunTyName: TyFun -> TyName -> bool; 

val generalization: bool -> Type -> TypeScheme;  

val specialization: TypeScheme -> Type;
val TypeOfScheme : TypeScheme -> Type;
val type_subst: (TypeVar * Type) list -> Type -> Type;
val freshSchemeOfType: Type -> TypeScheme;
val mkScheme: TypeVar list -> Type -> TypeScheme;
val trivial_scheme: Type -> TypeScheme;
val scheme_1u: (Type -> Type) -> TypeScheme;
val scheme_1u_eq: (Type -> Type) -> TypeScheme;
val scheme_1u_imp: (Type -> Type) -> TypeScheme;
val scheme_2u: (Type -> Type -> Type) -> TypeScheme;
val scheme_3u: (Type -> Type -> Type -> Type) -> TypeScheme;

val resetBindingLevel: unit -> unit;
val incrBindingLevel: unit -> unit;
val decrBindingLevel: unit -> unit;
val currentBindingLevel: unit -> int;
val setCurrentBindingLevel: bool -> Type -> unit;

val EqualityOfTyFun : TyFun -> TyNameEqu;

val makeEquality : Type -> unit
val schemeViolatesEquality : TypeScheme -> bool;
val typeIsImperative : Type -> bool;

val sc_bool : TypeScheme;
val sc_bogus : TypeScheme;
val initial_bool_CE : ConEnv;
val initial_list_CE : ConEnv;
val initial_option_CE : ConEnv;
val initial_order_CE : ConEnv;
val initial_frag_CE : ConEnv;

val type_con : Type list -> TyName -> Type;
val type_arrow: Type -> Type -> Type;
val type_rigid_record : Type Row -> Type;
val type_flexible_record: Type Row -> RowType -> Type;
val type_ref: Type -> Type;
val type_pair: Type -> Type -> Type;
val type_product: Type list -> Type;

val type_unit      : Type;
val type_bool      : Type;
val type_int       : Type;
val type_word      : Type;
val type_word8     : Type;
val type_char      : Type;
val type_real      : Type;
val type_string    : Type;
val type_substring : Type; 
val type_syserror  : Type;
val type_list      : Type -> Type;
val type_vector    : Type -> Type;
val type_exn       : Type;
val type_option    : Type -> Type;
val type_order     : Type;
val type_frag      : Type -> Type;
val type_ppstream  : Type;

val unit_General : CSig;

val checkClosedCSig : CSig -> unit; 
val checkClosedExEnvironment : ExEnvironment -> unit; 

val copySig : (TyName * TyApp) list -> (TypeVar * Type) list -> Sig -> Sig; 
val copyMod : (TyName * TyApp) list -> (TypeVar * Type) list -> Mod -> Mod; 
val copyRecStr : (TyName * TyApp) list -> (TypeVar * Type) list -> RecStr -> RecStr; 
val copyStr : (TyName * TyApp) list -> (TypeVar * Type) list -> Str -> Str; 
val copyGenFun : (TyName * TyApp) list -> (TypeVar * Type) list -> GenFun -> GenFun; 

val parameteriseTyNameSet: TyNameSet -> TyNameSet -> (TyNameSet * (TyName * TyApp) list);

val conEnvOfTyApp: TyApp -> ConEnv option;


(* destructively change the kind and update the binding level of type names
*)
val refreshTyName: TnSort -> TyName ->  unit; 
val refreshTyNameSet: TnSort -> TyNameSet  -> unit;

val realizeLongTyCon : QualifiedIdent -> TyStr ->  TyStr -> unit;
val matchMod : Mod -> Mod -> unit; 
val matchCSig : CSig -> CSig -> unit; 
val errMatchReason : string -> string -> matchReason -> unit;
val checkCSig : CSig -> CSig -> unit; 

(* cvr: operations on normed structures and environments to return
   runtime field and  static info *)

val sizeOfStr: Str -> int;

val lookupMEofStr : Str -> string -> (int * (RecStr global))
val lookupFEofStr : Str -> string -> (int * (GenFun global))
val lookupVEofStr : Str -> string -> (int*(TypeScheme * ConStatusDesc) global);

val lookupMEofEnv : Environment -> string -> (int * (RecStr global))
val lookupFEofEnv : Environment -> string -> (int * (GenFun global))
val lookupVEofEnv : Environment -> string -> (int*  (TypeScheme * 
						     ConStatusDesc) global);

end;









