(* Int64 -- SML Basis Library *)
(* Stop gap implementation by KFL, 2021-07-05, based on Int.mlp by PS *)

(* This unit relies on two's complement representation *)
structure Int64 :> Int64 =
struct
prim_type int;

val precision = SOME 64;

local
    prim_val equal_     : int -> int -> bool = 2 "boxed_int64_equal";
    prim_val less_      : int -> int -> bool = 2 "boxed_int64_less";
    prim_val fromInt_  : Int.int -> int = 1 "boxed_int64_fromint";
    prim_val toInt_    : int -> Int.int = 1 "boxed_int64_toint";

    prim_val lshift_    : int -> int -> int = 2 "boxed_uint64_lshift";
    prim_val usub_       : int -> int -> int = 2 "boxed_uint64_sub";


    prim_val add_       : int -> int -> int = 2 "boxed_int64_add";
    prim_val sub_       : int -> int -> int = 2 "boxed_int64_sub";
    prim_val mul_       : int -> int -> int = 2 "boxed_int64_mul";
    prim_val div_       : int -> int -> int = 2 "boxed_int64_div";
    prim_val mod_       : int -> int -> int = 2 "boxed_int64_mod";
    prim_val quot_      : int -> int -> int = 2 "boxed_int64_quot";
    prim_val rem_       : int -> int -> int = 2 "boxed_int64_rem";

in
    val fromInt : Int.int -> int = fromInt_

    val ZERO = fromInt 0
    val maxStdInt = fromInt(Misc.valOf Int.maxInt)
    val minStdInt = fromInt(Misc.valOf Int.minInt)

    fun toInt n =
        if less_ n minStdInt orelse less_ maxStdInt n then raise Overflow
        else toInt_ n

    fun toLarge i = i
    fun fromLarge i = i

    val op *    : int * int -> int = fn(x, y) => mul_ x y;
    val op +    : int * int -> int = fn(x, y) => add_ x y;
    val op -    : int * int -> int = fn(x, y) => sub_ x y;
    val op div  : int * int -> int =
     fn(x, y) => if equal_ y ZERO then raise Div else div_ x y;
    val op mod  : int * int -> int =
     fn(x, y) => if equal_ y ZERO then raise Div else mod_ x y;
    val op quot : int * int -> int =
     fn(x, y) => if equal_ y ZERO then raise Div else quot_ x y;
    val op rem  : int * int -> int =
     fn(x, y) => if equal_ y ZERO then raise Div else rem_ x y;

    fun compare (x, y: int) =
        if less_ x y then LESS else if less_ y x then GREATER else EQUAL;

    fun sign i =
        case compare(i, ZERO) of
            LESS    => ~1
          | EQUAL   => 0
          | GREATER => 1

    fun sameSign (i, j) = sign i = sign j;

    val ~ = fn n => ZERO - n

    fun abs n = if less_ n ZERO then ~n else n

    val minInt_ = usub_ ZERO (lshift_ (fromInt 1) (fromInt 63))
    val minInt  = SOME minInt_
    val maxInt  = SOME (~(minInt_ + fromInt 1));

local
    open StringCvt
    (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
    fun decval c = fromInt (Char.ord c) - fromInt 48;
    fun hexval c =
        if #"0" <= c andalso c <= #"9" then
            fromInt (Char.ord c) - fromInt 48
        else
            (fromInt (Char.ord c) - fromInt 55) mod (fromInt 32);

    fun prhex i =
        if toInt i < 10 then Char.chr(toInt (i + fromInt 48))
        else Char.chr(toInt (i + fromInt 55));

    fun skipWSget getc source = getc (dropl Char.isSpace getc source)

    fun conv radix i = 
	    let fun h n res =
                if equal_ n ZERO then res
                else h (n div radix) (prhex (n mod radix) :: res)

	        fun tostr n = h (n div radix) [prhex (n mod radix)]
	    in
	        if less_ i ZERO then
		        let val last  = ~(i mod (~radix))
		            val first = i div (~radix)
		        in
		            String.implode(#"~" :: h first [prhex last])
		        end
	        else
		        String.implode (tostr i)
	    end
in
    fun scan radix getc source =
	let open StringCvt
	    val (isDigit, factor) = 
		case radix of
		    BIN => (fn c => (#"0" <= c andalso c <= #"1"), fromInt  2)
		  | OCT => (fn c => (#"0" <= c andalso c <= #"7"), fromInt  8)
		  | DEC => (Char.isDigit,                          fromInt 10)
		  | HEX => (Char.isHexDigit,                       fromInt 16)
	    fun dig1 sgn NONE             = NONE
	      | dig1 sgn (SOME (c, rest)) = 
		    let val next_val =
		            if sgn = 1 then fn (res, hv) => factor * res + hv
		            else            fn (res, hv) => factor * res - hv
		        fun digr res src =
		            case getc src of
			            NONE           => SOME (res, src)
			          | SOME (c, rest) =>
				        if isDigit c then
				            digr (next_val(res, hexval c)) rest
				        else
				            SOME (res, src)
		    in if isDigit c then digr ((fromInt sgn) * hexval c) rest else NONE end
	    fun getdigs sgn after0 inp = 
		    case dig1 sgn inp of
		        NONE => SOME(ZERO, after0)
		      | res  => res
	    fun hexopt sgn NONE                 = NONE
	      | hexopt sgn (SOME(#"0", after0)) =
		    if radix <> HEX then getdigs sgn after0 (getc after0)
		    else
		        (case getc after0 of
			         NONE             => SOME(ZERO, after0)
		           | SOME(#"x", rest) => getdigs sgn after0 (getc rest)
		           | SOME(#"X", rest) => getdigs sgn after0 (getc rest)
		           | inp              => getdigs sgn after0 inp)
	      | hexopt sgn inp = dig1 sgn inp
	    fun sign NONE                = NONE
	      | sign (SOME (#"~", rest)) = hexopt ~1 (getc rest)
	      | sign (SOME (#"-", rest)) = hexopt ~1 (getc rest)
	      | sign (SOME (#"+", rest)) = hexopt  1 (getc rest)
	      | sign inp                 = hexopt  1 inp
	in sign (skipWSget getc source) end;
	    
    fun fmt BIN = conv (fromInt  2)
      | fmt OCT = conv (fromInt  8)
      | fmt DEC = conv (fromInt 10)
      | fmt HEX = conv (fromInt 16)

    val toString = fmt DEC

    val fromString = scanString (scan DEC)
end



val == : int * int -> bool = fn(x, y) => equal_ x y;
val op <    : int * int -> bool = fn(x, y) => less_ x y;
val op <=   : int * int -> bool = fn(x, y) => less_ x y orelse equal_ x y;
val op >    : int * int -> bool = fn(x, y) => y < x;
val op >=   : int * int -> bool = fn(x, y) => y <= x;
fun min(w1 : int, w2) = if w1 > w2 then w2 else w1;
fun max(w1 : int, w2) = if w1 > w2 then w1 else w2;

end
end
