local
  open Globals Asynt;
in

val resolveToplevelSigExp : SigExp -> SigExp;
val resolveToplevelSpec : Spec -> InfixBasis * Spec;
val resolveToplevelDec : Dec -> InfixBasis * Dec;


end;
