(* Word8 -- new basis 1994-11-01, 1995-04-12, 1996-09-30 *)

(* This unit relies on two's complement representation *)

type word = word8;
val wordSize = 8;

(* Invariant for values w of type Word8.word: 0 <= toInt w < 256 *)

local
    prim_val orb_       : word -> word -> word      = 2 "or";
    prim_val andb_      : word -> word -> word      = 2 "and";
    prim_val xorb_      : word -> word -> word      = 2 "xor";
    prim_val lshift_    : word -> Word.word -> word = 2 "shift_left";
    prim_val rshiftsig_ : word -> Word.word -> word = 2 "shift_right_signed";
    prim_val rshiftuns_ : word -> Word.word -> word = 2 "shift_right_unsigned";
    prim_val adduns_    : word -> word -> word      = 2 "+intunsig";
    prim_val subuns_    : word -> word -> word      = 2 "-intunsig";
    prim_val muluns_    : word -> word -> word      = 2 "*intunsig";

    prim_val fromInt_ : int -> word = 1 "identity";
    prim_val largeWordToWord_ : Word.word -> word = 1 "identity";
    fun norm w = andb_ w (fromInt_ 255);

    prim_val word2int   : Word.word -> int = 1 "identity";
in
    prim_val toInt : word -> int = 1 "identity";
    fun toIntX w = if toInt w < 128 then (* msbit = 0 *)
	               toInt w
		   else			 (* msbit = 1 *)
		       toInt (orb_ w (fromInt_ ~256))
    fun fromInt w  = norm (fromInt_ w);

    prim_val toLargeInt : word -> int = 1 "identity";
    val toLargeIntX = toIntX
    val fromLargeInt = fromInt

    prim_val toLargeWord   : word -> Word.word = 1 "identity";
    fun toLargeWordX w = if toInt w < 128 then (* msbit = 0 *)
	                     toLargeWord w
			 else		       (* msbit = 1 *)
			     toLargeWord (orb_ w (fromInt_ ~256))
    fun fromLargeWord w = norm(largeWordToWord_ w);

    fun orb (x, y)  = orb_ x y;
    fun andb (x, y) = andb_ x y;
    fun xorb (x, y) = xorb_ x y;
    fun notb x      = norm (xorb_ x (fromInt_ ~1)); 

    fun << (w, k) = 
	if word2int k >= 8 orelse word2int k < 0 then fromInt_ 0
	else norm (lshift_ w k);

    fun >> (w, k) = 
	if word2int k >= 8 orelse word2int k < 0 then fromInt_ 0
	else rshiftuns_ w k;

    fun ~>> (w, k) = 
	if toInt w < 128 then	(* msbit = 0: no sign to extend  *)
	    if word2int k >= 8 orelse word2int k < 0 then fromInt_ 0
	    else rshiftuns_ w k
	else				(* msbit = 1: extend, then shift *)
	    if word2int k >= 8 orelse word2int k < 0 then 
		norm (fromInt_ ~1)
	    else norm (rshiftsig_ (orb_ w (fromInt_ ~256)) k);

    local 
      open StringCvt
      fun skipWSget getc source = getc (skipWS getc source)

      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun decval c = Char.ord c - 48;
      fun hexval c = 
	  if #"0" <= c andalso c <= #"9" then 
	      Char.ord c - 48
	  else 
	      (Char.ord c - 55) mod 32;

      fun prhex i = 
	  if i < 10 then Char.chr(i + 48) else Char.chr(i + 55);

      fun conv radix w = 
	  let fun h n res = 
		  if n = 0 then res
		  else h (n div radix) (prhex (n mod radix) :: res)
	      fun tostr n = h (n div radix) [prhex (n mod radix)]
	  in String.implode (tostr (toInt w)) end

    in
      fun scan radix getc source =
	  let open StringCvt
	      val source = skipWS getc source
	      val (isDigit, factor) = 
		  case radix of
		      BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		    | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		    | DEC => (Char.isDigit,                          10)
		    | HEX => (Char.isHexDigit,                       16)
	      fun return res src = 
		  if res < 256 then SOME (fromInt_ res, src) 
		  else raise Overflow
	      fun dig1 NONE             = NONE
		| dig1 (SOME (c, rest)) = 
		  let 
		      fun digr res src = 
		          case getc src of
			      NONE           => return res src
			    | SOME (c, rest) => 
				  if isDigit c then 
				      digr(factor*res+hexval c) rest
				  else 
				      return res src
		  in 
		      if isDigit c then digr (hexval c) rest else NONE 
		  end
	      fun getdigs after0 src = 
		  case dig1 (getc src) of
		      NONE => return 0 after0
		    | res  => res
	      fun hexprefix after0 src =
		  if radix <> HEX then getdigs after0 src
		  else
		      case getc src of
			  SOME(#"x", rest) => getdigs after0 rest
			| SOME(#"X", rest) => getdigs after0 rest
			| SOME _           => getdigs after0 src
			| NONE => return 0 after0
	  in 
	      case getc source of
		  SOME(#"0", after0) => 
		      (case getc after0 of 
			   SOME(#"w", src2) => hexprefix after0 src2 
			 | SOME _           => hexprefix after0 after0 
			 | NONE             => return 0 after0)
		| SOME _ => dig1 (getc source)
		| NONE   => NONE 
end;

      fun fmt BIN = conv  2
	| fmt OCT = conv  8
	| fmt DEC = conv 10
	| fmt HEX = conv 16
      fun toString w   = conv 16 w
      fun fromString s = scanString (scan HEX) s
    end (* local for string functions *)

    (* Redefining +, -, *, div, and mod is a horrible idea ... *)

    fun w1  +  w2 = norm (adduns_ w1 w2);
    fun w1  -  w2 = norm (subuns_ w1 w2);
    fun w1  *  w2 = norm (muluns_ w1 w2);
    val op div  : word * word -> word = op div;
    val op mod  : word * word -> word = op mod;

    fun min(w1 : word, w2) = if w1 > w2 then w2 else w1;
    fun max(w1 : word, w2) = if w1 > w2 then w1 else w2;
    fun compare (x, y: word) = 
	if x<y then LESS else if x>y then GREATER else EQUAL;
    val op >    : word * word -> bool = op >;
    val op >=   : word * word -> bool = op >=;
    val op <    : word * word -> bool = op <;
    val op <=   : word * word -> bool = op <=;
end
