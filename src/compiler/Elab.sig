local
  open Globals Asynt;
in

val elabToplevelDec: Dec -> (ModEnv * FunEnv * SigEnv * VarEnv * TyEnv) Existential

val elabToplevelSigExp: SigExp -> RecStr Signature;

val elabToplevelSpec: Spec -> Str Signature;

end
