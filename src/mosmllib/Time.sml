(* Time -- SML Basis Library 1995-02-25, 1995-05-12, 2000-10-24 *)

local 
    prim_val getrealtime_ : unit -> real = 1 "sml_getrealtime";
    prim_val exp : real -> real = 1 "sml_exp";
    prim_val ln  : real -> real = 1 "sml_ln";

    fun negpow10 p = exp(ln 10.0 * real (~p));
in
    type time = real
    (* Represents the number of microseconds since, or before, 
       UTC 00:00 on 1 Jan 1970. *)

    exception Time

    val zeroTime = 0.0;
    fun now () = getrealtime_ ();

    fun fromSeconds s = 1000000.0 * real s;

    fun fromMilliseconds ms = 1000.0 * real ms;

    fun fromMicroseconds us = real us;

    fun toSeconds r = trunc(r/1000000.0);

    fun toMilliseconds r = trunc(r/1000.0);

    fun toMicroseconds r = trunc r;

    (* Should replace with a more efficient real truncation: *)

    fun fromReal r = 
	let val sec = trunc r
	    val usec = trunc(1000000.0 * (r - real sec))
	in 1000000.0 * real sec + real usec end;

    fun toReal r = r / 1000000.0;

    fun timeToUnits (t, p) = floor(toReal t * negpow10 p + 0.5);

    fun fmt p usec =
	Real.fmt (StringCvt.FIX (SOME (if p > 0 then p else 0))) 
	(usec/1000000.0);

    fun toString t = fmt 3 t;

fun scan getc source =
    let fun skipWSget getc source = 
	    getc (StringCvt.dropl Char.isSpace getc source)
	fun decval c = Char.ord c - 48;
        fun pow10 0 = 1
	  | pow10 n = 10 * pow10 (n-1)
	fun mktime neg intgv decs fracv =
	    let val usecs = (pow10 (7-decs) * fracv + 5) div 10
		val res = intgv * 1000000.0 + real usecs 
	    in
		if neg then ~res else res		
	    end
	fun skipdigs src =
	    case getc src of 
		NONE          => src
	      | SOME(c, rest) => if Char.isDigit c then skipdigs rest 
				 else src
	fun frac neg intgv decs fracv src =
	    if decs >= 7 then SOME(mktime neg intgv decs fracv, skipdigs src)
	    else case getc src of
		NONE          => SOME(mktime neg intgv decs fracv, src)
	      | SOME(c, rest) => 
		    if Char.isDigit c then 
			frac neg intgv (decs+1) (10 * fracv + decval c) rest
		    else 
			SOME(mktime neg intgv decs fracv, src)
	fun intg neg intgv src = 
	    case getc src of
		NONE              => SOME(mktime neg intgv 6 0, src)
	      | SOME (#".", rest) => frac neg intgv 0 0 rest
	      | SOME (c, rest)    => 
		    if Char.isDigit c then 
			intg neg (10.0 * intgv + real(decval c)) rest 
		    else SOME(mktime neg intgv 6 0, src)
	fun nbr neg src =
	    case getc src of
		NONE             => NONE
	      | SOME(#".", rest) => 
		    (case getc rest of
			 NONE          => NONE
		       | SOME(c, rest) => 
			     if Char.isDigit c then 
				 frac neg 0.0 1 (decval c) rest
			     else 
				 NONE)
	      | SOME(c, rest)    => 
		if Char.isDigit c then intg neg (real (decval c)) rest 
		else NONE
	val afterws = StringCvt.dropl Char.isSpace getc source
    in case getc afterws of
	NONE             => NONE
      | SOME(#"+", rest) => nbr false rest
      | SOME(#"~", rest) => nbr true  rest
      | SOME(#"-", rest) => nbr true  rest
      | _                => nbr false afterws
    end;

    fun fromString s = StringCvt.scanString scan s;

    fun compare (x, y: time) = 
	if x<y then LESS else if x>y then GREATER else EQUAL;

    val op + = op + : time * time -> time
    and op - = op - : time * time -> time

    val op <  = op <  : time * time -> bool
    and op <= = op <= : time * time -> bool
    and op >  = op >  : time * time -> bool
    and op >= = op >= : time * time -> bool
end
