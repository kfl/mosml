local
  open Globals Asynt;
in

val elabStrDec: Dec -> (ModEnv * FunEnv * SigEnv * VarEnv * TyEnv) Existential 

val elabToplevelDec: Dec -> (ModEnv * FunEnv * SigEnv * VarEnv * TyEnv) Existential
val elabToplevelSigExp: SigExp -> RecStr Signature;
val elabToplevelSpec: Spec -> Str Signature;
val elabSigSpec: Spec -> Str Signature;

end
