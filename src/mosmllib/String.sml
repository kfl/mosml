(* String -- 1994-12-10, 1995-11-07, 1999-04-22 *)

local 
    type char = Char.char;
    prim_val sub_      : string -> int -> char         = 2 "get_nth_char";
    prim_val mkstring_ : int -> string                 = 1 "create_string";
    prim_val update_   : string -> int -> char -> unit = 3 "set_nth_char";
    prim_val strcmp_   : string -> string -> int       = 2 "compare_strings";
    prim_val blit_     : string -> int -> string -> int -> int -> unit 
                                                       = 5 "blit_string";

    fun (f o g) x = f (g x);
in

type string = string
val maxSize = Strbase.maxlen
val size = size

fun sub(s, i) = 
    if i<0 orelse i >= size s then raise Subscript
    else sub_ s i;

fun concat strs =
    let fun acc [] len       = len
          | acc (v1::vr) len = acc vr (size v1 + len)
        val len = acc strs 0
        val newstr = if len > maxSize then raise Size else mkstring_ len 
        fun copyall to []       = ()
          | copyall to (v1::vr) = 
            let val len1 = size v1
            in blit_ v1 0 newstr to len1; copyall (to+len1) vr end
    in copyall 0 strs; newstr end;

val op ^ = op ^;

fun str c = 
    let val newstr = mkstring_ 1
    in update_ newstr 0 c; newstr end;

fun implode cs = 
  let val n = List.length cs
      val newstr = if n > maxSize then raise Size else mkstring_ n
      fun init []      i = ()
        | init (c::cr) i = (update_ newstr i c; init cr (i+1))
  in (init cs 0; newstr) end;

fun extract (s, i, slicelen) =
    let val n = case slicelen of NONE => size s - i | SOME n => n
	val newstr = if i<0 orelse n<0 orelse n>size s-i then raise Subscript
		     else mkstring_ n
    in blit_ s i newstr 0 n; newstr end;

fun substring (s, i, n) = extract(s, i, SOME n);

fun explode s =
    let fun h j res = if j<0 then res
		      else h (j-1) (sub_ s j :: res)
    in h (size s - 1) [] end;

fun map (f : char -> char) (a : string) : string = 
    let val stop = size a
	val newstr = mkstring_ stop 
	fun lr j = if j < stop then (update_ newstr j (f(sub_ a j)); 
				     lr (j+1))
		   else ()
    in lr 0; newstr end

fun compare (s1, s2) = 
    (case strcmp_ s1 s2 of
         ~2 => LESS
       | ~1 => LESS
       |  0 => EQUAL
       |  1 => GREATER
       |  2 => GREATER
       |  _ => raise Fail "internal error: String.compare");

fun collate cmp (s1, s2) =
    let val n1 = size s1 
	and n2 = size s2
	val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point s1[0..j-1] = s2[0..j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
		case cmp(sub_ s1 j, sub_ s2 j) of
		    LESS    => LESS
		  | GREATER => GREATER
		  | EQUAL   => h (j+1)
    in h 0 end;

fun translate f s = 
    Strbase.translate f (s, 0, size s);

fun tokens p s = 
    List.map substring (Strbase.tokens p (s, 0, size s));

fun fields p s = 
    List.map substring (Strbase.fields p (s, 0, size s));

fun isPrefix s1 s2 = 
    let val n1 = size s1 
	and n2 = size s2
	val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point s1[0..j-1] = s2[0..j-1] *)
	    j = stop orelse sub_ s1 j = sub_ s2 j andalso h (j+1)
    in n1 <= n2 andalso h 0 end;

fun foldl f e s = 
    let val stop = size s
	fun h j res = if j>=stop then res 
		      else h (j+1) (f (sub_ s j, res))
    in h 0 e end;

fun foldr f e s = 
    let fun h j res = if j<0 then res 
                      else h (j-1) (f (sub_ s j, res))
    in h (size s - 1) e end;

fun find p s = 
    let val stop = size s
	fun h j = if j>=stop then NONE 
		  else if p (sub_ s j) then SOME j
		  else h (j+1) 
    in h 0 end;

fun fromString s =
    let fun getc i = if i < size s then SOME (sub_ s i, i+1) else NONE
	fun h src res = 
	    case getc src of
		NONE              => SOME (implode(List.rev res))
	      | SOME(#"\\", src1) => 
		    (case Strbase.fromMLescape getc src1 of
			 NONE          => NONE
		       | SOME(c, src2) => h src2 (c :: res))
	      | SOME(c, src1)     => h src1 (c :: res)
    in h 0 [] end;
	
fun toString s = Strbase.translate Strbase.toMLescape (s, 0, size s);

fun fromCString s = Strbase.fromCString s

fun toCString s = Strbase.translate Strbase.toCescape (s, 0, size s)

val op <  = op <  : string * string -> bool;
val op <= = op <= : string * string -> bool;
val op >  = op >  : string * string -> bool;
val op >= = op >= : string * string -> bool;
end
