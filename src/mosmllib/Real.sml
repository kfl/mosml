(* Real.sml -- 1995-05-24, 1996-05-16, 1996-07-02 *)

type real = real

exception Div = Div
and Overflow = Overflow;

val floor = floor;
val ceil  = ceil;
val trunc = trunc;
val round = round;

val fromInt = real;

(* The following should be replaced by numerically better conversion
functions; see 

Steele and White : How to print floating-point numbers accurately,
PLDI'90, pages 112-123, and

Clinger: How to read floating-point numbers accurately, PLDI'90, pages
92-101.

D.M. Gay: Correctly rounded binary-decimal and decimal-binary
conversions, AT&T Bell Labs, Numerical Analysis Manuscript 90-10,
November 30, 1990 *)

fun fmt spec = 
    let prim_val to_string       : string -> real -> string 
	                           = 2 "sml_general_string_of_float";
	prim_val plain_to_string : real -> string = 1 "sml_string_of_float";
	prim_val sub_            : string -> int -> char = 2 "get_nth_char";
	prim_val int_to_string   : int -> string = 1 "sml_string_of_int";

	fun mlify s = (* Add ".0" if not "e" or "." in s  *)
	    let val stop = size s
		fun loop i =		(* s[0..i-1] contains no "." or "e" *)
		    if i = stop then s ^ ".0"
		    else if sub_ s i = #"." orelse sub_ s i = #"E" then s
                    else loop (i+1)
	    in loop 0 end

	open StringCvt
	(* Below we check that the requested number of decimal digits 
	 * is reasonable; else sml_general_string_of_float may crash. *)
	val fmtspec = 
	case spec of
	    SCI NONE     => to_string "%e"
	  | SCI (SOME n) => 
		if n < 0 orelse n > 400 then raise Size 
		else to_string ("%." ^ int_to_string n ^ "e")
	  | FIX NONE     => to_string "%f"
	  | FIX (SOME n) =>
		if n < 0 orelse n > 400 then raise Size 
		else to_string ("%." ^ int_to_string n ^ "f")
	  | GEN NONE     => plain_to_string
	  | GEN (SOME n) => 
		if n < 1 orelse n > 400 then raise Size 
		else fn r => mlify (to_string ("%." ^ int_to_string n ^ "g") r)
    in fmtspec end

fun toString r = fmt (StringCvt.GEN NONE) r;

fun scan getc source = 
    let fun decval c = Char.ord c - 48
	fun pow10 0 = 1.0
	  | pow10 n = 
	    if n mod 2 = 0 then 
		let val x = pow10 (n div 2) in x * x end
	    else 10.0 * pow10 (n-1)
	fun pointsym src = 
	    case getc src of
		NONE           => (false, src)
	      | SOME (c, rest) => if c = #"." then (true, rest)
				  else (false, src)
	fun esym src = 
	    case getc src of
		NONE           => (false, src)
	      | SOME (c, rest) => 
		    if c = #"e" orelse c = #"E"  then 
			(true, rest)
		    else (false, src)
	fun scandigs first next final source =
	    let fun digs state src = 
		case getc src of
		    NONE          => (SOME (final state), src)
		  | SOME(c, rest) => 
			if Char.isDigit c then 
			    digs (next(state, decval c)) rest
			else 
			    (SOME (final state), src)
	    in 
		case getc source of
		    NONE          => (NONE, source)
		  | SOME(c, rest) => 
			if Char.isDigit c then digs (first (decval c)) rest
			else (NONE, source)
	    end

	fun ident x = x
	val getint  = 
	    scandigs real (fn (res, cval) => 10.0 * res + real cval) ident
	val getfrac = 
	    scandigs (fn cval => (1, real cval))    
	             (fn ((decs, frac), cval) => (decs+1, 10.0*frac+real cval))
		     (fn (decs, frac) => frac / pow10 decs)
	val getexp = scandigs ident (fn (res, cval) => 10 * res + cval) ident

	fun sign src =
	    case getc src of
		SOME(#"+", rest) => (true,  rest)
	      | SOME(#"-", rest) => (false, rest)
	      | SOME(#"~", rest) => (false, rest)
	      | _                => (true,  src )

	val src = StringCvt.dropl Char.isSpace getc source
	val (manpos, src1) = sign src
	val (intg,   src2) = getint src1
	val (decpt,  src3) = pointsym src2
	val (frac,   src4) = getfrac src3 

	fun mkres v rest = 
	    SOME(if manpos then v else ~v, rest)

        fun expopt manval src = 
	    let val (esym,   src1) = esym src
		val (exppos, src2) = sign src1
		val (expv,   rest) = getexp src2 
	    in 
		case (esym, expv) of
		    (_,     NONE)     => mkres manval src
		  | (true,  SOME exp) => 
			if exppos then mkres (manval * pow10 exp) rest
			else mkres (manval / pow10 exp) rest
		  | _                 => NONE
	    end
    in 
	case (intg,     decpt, frac) of
	    (NONE,      true,  SOME fval) => expopt fval src4
          | (SOME ival, false, SOME _   ) => NONE
          | (SOME ival, true,  NONE     ) => mkres ival src2
          | (SOME ival, false, NONE     ) => expopt ival src2
          | (SOME ival, _    , SOME fval) => expopt (ival+fval) src4
	  | _                             => NONE 
    end;

val fromString = StringCvt.scanString scan;

val ~       : real -> real        = ~;
val op +    : real * real -> real = op +;
val op -    : real * real -> real = op -;
val op *    : real * real -> real = op *;
val op /    : real * real -> real = op /;
val op >    : real * real -> bool = op >;
val op >=   : real * real -> bool = op >=;
val op <    : real * real -> bool = op <;
val op <=   : real * real -> bool = op <=;
val op ==   : real * real -> bool = op =;
val op !=   : real * real -> bool = op <>;
fun ?=(x : real, y : real) : bool = false;
val abs     : real -> real = abs;
fun sign i = if i > 0.0 then 1 else if i < 0.0 then ~1 else 0;
fun compare (x, y: real) = 
    if x<y then LESS else if x>y then GREATER else EQUAL;

fun sameSign (i, j) = sign i = sign j;

fun min (x, y) = if x < y then x else y : real;
fun max (x, y) = if x < y then y else x : real;

fun toDefault   i   = i;
fun fromDefault i   = i;
