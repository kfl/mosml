(* Bool -- new basis 1995-03-30 *)

datatype bool = datatype bool

val not = not;

fun toString false = "false"
  | toString true  = "true";

fun getstring str getc source =
    let prim_val sub_ : string -> int -> char = 2 "get_nth_char";
	val len = size str
	fun toLower c = 
	    if #"A" <= c andalso c <= #"Z" then Char.chr (Char.ord c + 32)
	    else c;
	fun h i src = 
	    if i >= len then SOME src 
	    else case getc src of
		NONE          => NONE
	      | SOME(c, rest) => if toLower c = sub_ str i then h (i+1) rest
				 else NONE
    in h 0 source end

fun scan getc source =
    let val src = StringCvt.dropl Char.isSpace getc source 
    in
	case getstring "true" getc src of
	    SOME rest => SOME(true, rest)
	  | NONE  => 
	case getstring "false" getc src of
	    SOME rest => SOME(false, rest)
	  | NONE => NONE
    end

val fromString = StringCvt.scanString scan;
