local
  open Globals Asynt;
in

val resolveToplevelDec : Dec -> InfixBasis * Dec;
val resolveToplevelSpec : Spec -> Spec;

end;
