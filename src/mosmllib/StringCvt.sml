(* StringCvt -- new basis 1995-04-06, 1995-10-26, 1996-04-01, 1997-06-03 *)

local
    prim_val sub_      : string -> int -> char  = 2 "get_nth_char";
    prim_val mkstring_ : int -> string          = 1 "create_string";
    prim_val fill_     : string -> int -> int -> char -> unit 
                                                = 4 "fill_string";
    prim_val blit_     : string -> int -> string -> int -> int -> unit 
                                                = 5 "blit_string";
    prim_val set_nth_  : string -> int -> char -> unit 
                                                = 3 "set_nth_char";

    fun sub_string_ s start len =
	let val res = mkstring_ len
	in blit_ s start res 0 len; res end

in

datatype radix = BIN | OCT | DEC | HEX;
datatype realfmt = 
    SCI of int option	(* scientific,  arg = # dec. digits, dflt=6 *)
  | FIX of int option   (* fixed-point, arg = # dec. digits, dflt=6 *)
  | GEN of int option 	(* auto choice of the above,                *)
                        (* arg = # significant digits, dflt=12      *)

type cs = int		(* the state of a string character source   *)

type ('a, 'b) reader = 'b -> ('a * 'b) option

fun scanString scan s =
    let val len = size s
	fun getc i = if i >= len then NONE 
		     else SOME (sub_ s i, i+1)
    in case scan getc 0 of
	NONE          => NONE
      | SOME (res, _) => SOME res
    end;

fun dropl p getc = 
    let fun h src =
	case getc src of
	    NONE          => src
	  | SOME(c, rest) => if p c then h rest else src
    in h end;

(* skipWS getc = dropl Char.isSpace getc; here specialized for efficiency: *)
fun skipWS getc = 
    let fun h src =
	case getc src of
	    NONE          => src
	  | SOME(c, rest) => 
		if c = #" " orelse #"\009" <= c andalso c <= #"\013" 
		    then h rest 
		else src
    in h end;

fun splitl p getc =
    let val max = ref 15
	val tmp = ref (mkstring_ (!max))
	fun realloc () =
	    let val newmax = 2 * !max
		val newtmp = mkstring_ newmax
	    in 
		blit_ (!tmp) 0 newtmp 0 (!max);
		max := newmax;
		tmp := newtmp
	    end
	fun h len src =
	    case getc src of
		NONE          => (sub_string_ (!tmp) 0 len, src)
	      | SOME(c, rest) => 
		    if p c then 
			(if len >= !max then realloc () else ();
			 set_nth_ (!tmp) len c;
			 h (len+1) rest)
		    else
			(sub_string_ (!tmp) 0 len, src)
    in h 0 end;

fun takel p getc src = #1 (splitl p getc src);

fun padLeft c n s = 
    let val ssize = size s
    in if n <= ssize then s
       else let val res = mkstring_ n 
	    in
		fill_ res 0 (n - ssize) c;
		blit_ s 0 res (n - ssize) ssize;
		res
	    end
    end;
	     
fun padRight c n s = 
    let val ssize = size s
    in if n <= ssize then s
       else let val res = mkstring_ n 
	    in
		blit_ s 0 res 0 ssize;
		fill_ res ssize (n - ssize) c;
		res
	    end
    end;

end
