local
  open Fnlib Mixture Const Smlprim;
in

(* Internally, a global is represented by its fully qualified name,
   plus associated information. *)

type 'a global =
{
  info: 'a,               (* Description *)
  qualid: QualifiedIdent  (* Full name *)
};

datatype InfixStatus =
    NONFIXst
  | INFIXst of int
  | INFIXRst of int
;

type InfixBasis = (string, InfixStatus) Env;

type PrimInfo =
{
  primArity: int,
  primOp: SMLPrim
};

datatype OvlType =
    REGULARo				(* Non-overloaded                *)
  | OVL1NNo				(* numtext -> num                *)
  | OVL1NSo				(* numtext -> string             *)
  | OVL2NNBo				(* numtext * numtext -> bool     *)
  | OVL2NNNo				(* num * num -> num              *)
  | OVL1TXXo				(* printVal: pseudopoly 'a -> 'a *)
  | OVL1TPUo				(* installPP: pseudopoly         *)
  | OVL2EEBo                            (* =, <>: ''a * ''a -> bool      *)
;

datatype TyNameEqu = 
    FALSEequ 
  | TRUEequ 
  | REFequ 
  | ARROWequ of TyNameEqu * TyNameEqu;

datatype TyApp = 
    NAMEtyapp of TyName
  | APPtyapp of TyApp * TyFun
and TyFun =
    TYPEtyfun of TypeVar list * Type
  | LAMtyfun of TyName * TyFun
  | APPtyfun of TyApp
and Kind = 
    ARITYkind of int
  | ARROWkind of Kind * Kind
and TnSort =
    REAts of TyFun
  | VARIABLEts
  | PARAMETERts
and Type =
    VARt of TypeVar
  | ARROWt of Type * Type
  | CONt of Type list * TyApp
  | RECt of { fields: (Lab * Type) list, rho: RowType } ref
  | PACKt of ExMod
and TypeVarKind =
    Explicit of string
  | NoLink
  | LinkTo of Type
and RowTypeKind =
    NILrow
  | VARrow of RowVar
  | LINKrow of RowType
  | FIELDrow of Lab * Type * RowType
and TypeScheme = TypeScheme of
{
  tscParameters: TypeVar list,
  tscBody: Type
}
and ConStatusDesc =
    VARname of OvlType
  | PRIMname of PrimInfo
  | CONname of ConInfo
  | EXNname of ExConInfo
  | REFname
and ConEnv = ConEnv of  (ConInfo global list) (* cvr: ConEnv should be renamed for consistency...*)
           | LAMconenv of TyName * ConEnv 
and ExMod = EXISTSexmod of TyName list * Mod
and Mod = 
      STRmod of RecStr
    | FUNmod of TyName list * Mod * ExMod
and Str = STRstr of ModEnv * 
                    (string, (TyName list * Mod * ExMod) global) Env *
		    (*cvr:ugly, but FunEnv needs to be inlined because of the reference to TyName (defined below) *)
		    SigEnv *
		    TyEnv * 
		    VarEnv
    |     SEQstr of Str * Str                      (* hack for matching algorithm *)
and RecStr = RECrec of RecStr * RecStr
           | NONrec of Str 
and Sig = LAMBDAsig of TyName list * Mod
withtype ModEnv = (string, RecStr global) Env
and TyStr = TyFun * ConEnv
and TyEnv = (string, (TyFun * ConEnv)) Env
and VarEnv = (string,(TypeScheme * ConStatusDesc) global) Env    
and SigEnv = (string, Sig global) Env
and TyName = 
{
  tnKind: Kind,
  tnEqu: TyNameEqu,
  tnStamp: (string *int),  (* unit name * stamp *)
  tnSort: TnSort,
  tnLevel: int,
  tnConEnv: (ConEnv option) ref 
} ref global
and ConInfo =
{
  conArity: int,
  conIsGreedy: bool,
  conSpan: int,
  conTag: int,
  conType : TypeScheme
} ref
and ExConInfo =
{
  exconArity: int
} ref
and TypeVar =
{
  tvEqu : bool,
  tvImp : bool,
  tvKind : TypeVarKind,
  tvLevel : int,
  tvOvl : bool
} ref
and RowType = RowTypeKind ref
and RowVar =
{
  rvEqu : bool,
  rvImp : bool,
  rvLevel : int
} ref
;

(* export the type abbreviations local to the datatype *)
(*
type TyName = 
{
  tnKind: Kind,
  tnEqu: TyNameEqu,
  tnStamp: (string*int),
  tnSort: TnSort,
  tnLevel: int,
  tnConEnv: (ConEnv option) ref 
} ref global
and ConInfo =
{
  conArity: int,
  conIsGreedy: bool,
  conSpan: int,
  conTag: int,
  conType : TypeScheme
} ref
and ExConInfo =
{
  exconArity: int,
  exconIsGreedy: bool,
  exconTag : (QualifiedIdent * int) option
} ref
and TypeVar =
{
  tvEqu : bool,
  tvImp : bool,
  tvKind : TypeVarKind,
  tvLevel : int,
  tvOvl : bool
} ref
and RowType = RowTypeKind ref
and RowVar =
{
  rvEqu : bool,
  rvImp : bool,
  rvLevel : int
} ref
;
*)

type TyNameSet = TyName list
type GenFun = TyNameSet * Mod * ExMod

type VarInfo = (TypeScheme * ConStatusDesc) global
and TyInfo = (TyFun * ConEnv) 
and ModInfo = RecStr global
and FunInfo = GenFun global
and SigInfo = Sig global
;

type VarEnv = (string,VarInfo) Env    
and TyEnv = (string, TyInfo) Env
and ModEnv = (string, ModInfo) Env
and FunEnv = (string, FunInfo) Env
and SigEnv = (string, SigInfo) Env
;

datatype 'a Signature =
       LAMBDA of  TyName list * 'a
and 'a Existential = 
       EXISTS of TyName list * 'a;

type Environment = ModEnv * FunEnv * SigEnv * VarEnv * TyEnv;
type ExEnvironment = Environment Existential;

type RecType = { fields: (Lab * Type) list, rho: RowType } ref;



type ConStatus = ConStatusDesc global;





(* Updaters *)


fun setTnStamp r new_stamp =
  let val { tnStamp=stamp, tnKind=kind, tnEqu=equ, tnSort=sort, tnLevel=level,
            tnConEnv=tnConEnv} = !r in
    r := { tnStamp=new_stamp, tnKind=kind, tnEqu=equ, tnSort=sort, tnLevel=level,
            tnConEnv = tnConEnv}
  end;

fun setTnKind r new_kind =
  let val { tnStamp=stamp, tnKind=kind, tnEqu=equ, tnSort=sort, tnLevel=level,
            tnConEnv=tnConEnv} = !r in
    r := { tnStamp=stamp, tnKind=new_kind, tnEqu=equ, tnSort=sort,tnLevel=level,
            tnConEnv = tnConEnv}
  end;

fun setTnEqu r new_equ =
  let val { tnStamp=stamp, tnKind=kind, tnEqu=equ, tnSort=sort, tnLevel=level,
            tnConEnv=tnConEnv} = !r in
    r := { tnStamp=stamp, tnKind=kind, tnEqu=new_equ, tnSort=sort, tnLevel=level,
            tnConEnv=tnConEnv}
  end;

fun setTnSort r new_sort =
  let val { tnStamp=stamp, tnKind=kind, tnEqu=equ, tnSort=sort, tnLevel=level,
            tnConEnv=tnConEnv} = !r in
    r := { tnStamp=stamp, tnKind=kind, tnEqu=equ, tnSort=new_sort, tnLevel=level,
            tnConEnv=tnConEnv}
  end;


fun setTnLevel r new_level =
  let val { tnStamp=stamp, tnKind=kind, tnEqu=equ, tnSort=sort, tnLevel=level,
            tnConEnv=tnConEnv} = !r in
    r := { tnStamp=stamp, tnKind=kind, tnEqu=equ, tnSort=sort, tnLevel=new_level,
            tnConEnv=tnConEnv}
  end;

fun setTnConEnv r new_conenv =
  let val { tnStamp=stamp, tnKind=kind, tnEqu=equ, tnSort=sort, tnLevel=level,
            tnConEnv=tnConEnv} = !r in
    r := { tnStamp=stamp, tnKind=kind, tnEqu=equ, tnSort=sort, tnLevel=level,
            tnConEnv=new_conenv}
  end;


fun setConArity r new_arity =
  let val { conArity=arity, conIsGreedy=isGreedy,
            conTag=tag, conSpan=span, conType=typ }
          = !r
  in r :=
    { conArity=new_arity, conIsGreedy=isGreedy,
      conTag=tag, conSpan=span, conType=typ }
  end;

fun setConIsGreedy r new_isGreedy =
  let val { conArity=arity, conIsGreedy=isGreedy,
            conTag=tag, conSpan=span, conType=typ }
          = !r
  in r :=
    { conArity=arity, conIsGreedy=new_isGreedy,
      conTag=tag, conSpan=span, conType=typ }
  end;

fun setConTag r new_tag =
  let val { conArity=arity, conIsGreedy=isGreedy,
            conTag=tag, conSpan=span, conType=typ }
          = !r
  in r :=
    { conArity=arity, conIsGreedy=isGreedy,
      conTag=new_tag, conSpan=span, conType=typ }
  end;

fun setConSpan r new_span =
  let val { conArity=arity, conIsGreedy=isGreedy,
            conTag=tag, conSpan=span, conType=typ }
          = !r
  in r :=
    { conArity=arity, conIsGreedy=isGreedy,
      conTag=tag, conSpan=new_span, conType=typ }
  end;

fun setConType (r : ConInfo) new_typ =
  let val { conArity=arity, conIsGreedy=isGreedy,
            conTag=tag, conSpan=span, conType=typ }
          = !r
  in r :=
    { conArity=arity, conIsGreedy=isGreedy,
      conTag=tag, conSpan=span, conType=new_typ }
  end;

fun setExConArity r new_arity =
    r := { exconArity=new_arity }

fun setTvKind r new_kind =
  let val { tvKind=kind, tvLevel=level, tvEqu=equ, tvImp=imp, tvOvl=ovl }
          = !r
  in r :=
    { tvKind=new_kind, tvLevel=level, tvEqu=equ, tvImp=imp, tvOvl=ovl }
  end;

fun setTvLevel r new_level =
  let val { tvKind=kind, tvLevel=level, tvEqu=equ, tvImp=imp, tvOvl=ovl }
          = !r
  in r :=
    { tvKind=kind, tvLevel=new_level, tvEqu=equ, tvImp=imp, tvOvl=ovl }
  end;

fun setTvEqu r new_equ =
  let val { tvKind=kind, tvLevel=level, tvEqu=equ, tvImp=imp, tvOvl=ovl }
          = !r
  in r :=
    { tvKind=kind, tvLevel=level, tvEqu=new_equ, tvImp=imp, tvOvl=ovl }
  end;

fun setTvImp r new_imp =
  let val { tvKind=kind, tvLevel=level, tvEqu=equ, tvImp=imp, tvOvl=ovl }
          = !r
  in r :=
    { tvKind=kind, tvLevel=level, tvEqu=equ, tvImp=new_imp, tvOvl=ovl }
  end;

fun setTvOvl r new_ovl =
  let val { tvKind=kind, tvLevel=level, tvEqu=equ, tvImp=imp, tvOvl=ovl }
          = !r
  in r :=
    { tvKind=kind, tvLevel=level, tvEqu=equ, tvImp=imp, tvOvl=new_ovl }
  end;

fun setRtFields r new_fields =
  let val { fields=fields, rho=rho } = !r in
    r := { fields=new_fields, rho=rho }
  end;

fun setRtRho r new_rho =
  let val { fields=fields, rho=rho } = !r in
    r := { fields=fields, rho=new_rho }
  end;

fun setRvEqu r new_equ =
  let val { rvEqu=equ, rvImp=imp, rvLevel=level } = !r in
    r := { rvEqu=new_equ, rvImp=imp, rvLevel=level }
  end;

fun setRvImp r new_imp =
  let val { rvEqu=equ, rvImp=imp, rvLevel=level } = !r in
    r := { rvEqu=equ, rvImp=new_imp, rvLevel=level }
  end;

fun setRvLevel r new_level =
  let val { rvEqu=equ, rvImp=imp, rvLevel=level } = !r in
    r := { rvEqu=equ, rvImp=imp, rvLevel=new_level }
  end;

end;







