local
  open Fnlib Mixture Const Globals Location Types Asynt;
in

val mkIdInfo : (Location * QualifiedIdent) -> bool -> IdInfo;
val getConInfo : IdInfo -> ConInfo;
val getExConInfo : IdInfo -> ExConInfo;

val pairExp  : Exp -> Exp -> Exp;
val tupleExp : Location * Exp list -> Exp;
val quoteExp  : Exp -> Exp;
val antiquoteExp  : Exp -> Exp;
val listExp  : Location * Exp list -> Exp;
val seqExp   : Exp list -> Exp;
val hashLabelExp : Location * Lab -> Exp;
val mkLabPatOfId : Location * string -> Ty option -> Pat option
                 -> Lab * Pat;
val pairPat  : Pat -> Pat -> Pat;
val tuplePat : Location * Pat list -> Pat;
val listPat  : Location * Pat list -> Pat;
val tupleTy  : Ty list -> Ty;

val mkValIt : Exp -> Dec;

val domPat : Pat -> string list;
val domPatAcc : Pat -> string list -> string list;
val varsOfPatAcc : Pat -> IdInfo list -> IdInfo list;
val curriedness : Match -> int;

val printExp : Exp -> unit;
val printDec : Dec -> unit;

end;
