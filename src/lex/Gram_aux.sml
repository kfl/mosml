(* Auxiliaries for the parser. *)

local
  open List Syntax;
in

fun regexp_for_string s =
  let open CharVector infix 9 sub
      val len = length s
      fun re_string n =
        if n >= len then Epsilon
        else if n+1 = len then Characters([s sub n])
        else Sequence(Characters([s sub n]), re_string (n+1))
  in re_string 0 end
;

fun char_class c1 c2 =
  let fun class n =
        if n > Char.ord c2 then
          []
        else
          (Char.chr n) :: class(n+1)
  in class (Char.ord c1) end
;

val all_chars = char_class #"\001" #"\255";

fun subtract xs [] = xs
  | subtract xs ys =
      List.filter (fn x => not(Fnlib.member x ys)) xs
;

end;
