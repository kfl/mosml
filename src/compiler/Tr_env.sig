local
  open Mixture Const Lambda Asynt;
in


type RenEnv = (string * int) list;

(* cvr: modified 
val lookupRenEnv : QualifiedIdent -> (QualifiedIdent * int);
*)
val lookupRenEnv : (string -> Const.Id) -> QualifiedIdent -> (QualifiedIdent * int);
val updateCurrentRenEnv: RenEnv -> unit;

val renameId : string -> string * int;

datatype AccessPath =
    Path_rec of int
  | Path_local of int
  | Path_global of (QualifiedIdent * int)
  | Path_son of int * AccessPath
  | Path_virtual_son of int * AccessPath
;


(* cvr: modified
type TranslEnv = (string, AccessPath) Env * int;
val translateLocalAccess : TranslEnv -> string -> Lambda;
val translateAccess : TranslEnv -> QualifiedIdent -> Lambda;
*)
type TranslEnv = (Const.Id, AccessPath) Env * int;
val translateLocalAccess : (string -> Const.Id) -> TranslEnv -> string -> Lambda;
val translateAccess : (string -> Const.Id) -> TranslEnv -> QualifiedIdent -> Lambda;
val translateLongAccess : (string -> Const.Id) -> TranslEnv -> IdInfo ->  Lambda; (* cvr added *)

val translateExName : TranslEnv -> IdInfo -> Lambda;
val mkEnvOfRecPats : int -> Pat list -> TranslEnv;
val mkEnvOfPats : int -> Pat list -> TranslEnv * (Lambda -> Lambda);

end;




