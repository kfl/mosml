(* cvr: checks now in Elab.sml
val checkDec: bool -> Asynt.Dec -> unit;
val checkSpec: bool -> Asynt.Spec -> unit;
*)

val compliantStrDec: Asynt.Dec -> unit;
val compliantTopDec: Asynt.Dec -> unit;
val compliantSigExp: Asynt.SigExp -> unit;
val compliantSpec: Asynt.Spec -> unit;
val compliantTopSpec: Asynt.Spec -> unit;


