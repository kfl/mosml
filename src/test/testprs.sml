datatype token =
    Key of string
  | Name of string
;

exception SynError of string;

fun $a (Key b :: toks) =
      if a=b then (a, toks) else raise SynError a
  | $a _ =
      raise SynError "Symbol expected";

fun id (Name a :: toks) = (a, toks)
  | id toks = raise SynError "Identifier expected";

infix 3 >>;

fun (ph >> f) toks =
      let val (x, toks') = ph toks
      in  (f x, toks') end;

infix 0 ||;

fun (ph1 || ph2) toks =
      ph1 toks handle SynError _ => ph2 toks;

infix 5 ~~;

fun (ph1 ~~ ph2) toks =
      let val (x, toks1) = ph1 toks
          val (y, toks2) = ph2 toks1
      in ((x,y), toks2) end;

fun empty toks = ([], toks);

fun many ph toks =
      ( ph ~~ many ph >> (op ::) || empty ) toks;

fun snd (_, y) = y;

fun many_sep ph sep toks =
      ( ph ~~ ( (sep ~~ many_sep ph sep >> snd) || empty ) >> (op ::) ) toks;

fun parser ph toks =
      (case ph toks
         of (x, [])   => x
          | (_, _::_) => raise SynError "Extra characters in phrase"
      );

datatype typ
  = Con of string * typ list
  | Var of string
;

local
  fun typ toks =
    (   atom ~~ $"->" ~~ typ   >> (fn ((S, _), T) => Con("->", [S,T]))
    ||  atom
    ) toks
  and atom toks =
    (   id                     >> Var
    ||  $"(" ~~ typ ~~ $")"    >> (fn ((_, T), _) => T)
  ) toks;
in
  val parseTypeExp = parser typ;
end;

val t1 = [Name "a", Key "->", Name "b", Key "->", Name "c"];
val t2 = [Key "(", Name "a", Key "->", Name "b", Key ")", Key "->", Name "c"];
parseTypeExp t1;
parseTypeExp t2;
