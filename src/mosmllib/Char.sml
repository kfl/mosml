(* Char -- new basis 1995-05-01, 1995-11-10 *)

type char = char
(* Invariant: for c: char it holds that 0 <= ord c <= maxOrd *)

local 
    prim_val sub_     : string -> int -> char = 2 "get_nth_char"
    prim_val chr_     : int -> char           = 1 "identity";
    prim_val length_  : string -> int         = 1 "string_length";
in
    prim_val ord : char -> int               = 1 "identity";
    val minChar = #"\000"
    val maxChar = #"\255"
    val maxOrd = 255;

    fun chr i = if i<0 orelse i>maxOrd then raise Chr else chr_ i;

    fun succ c = 
	if c < maxChar then chr_(ord c + 1) else raise Chr;
    fun pred c = 
	if c > minChar then chr_(ord c - 1) else raise Chr;

    local
	prim_type array_
	prim_val array_    : int -> array_ = 1 "create_string";
	prim_val update_   : array_ -> char -> int -> unit 
	  		     = 3 "set_nth_char";
	prim_val fill_     : array_ -> int -> int -> int -> unit 
 			     = 4 "fill_string";
	prim_val subarray_ : array_ -> char -> int = 2 "get_nth_char"

        fun mktable s =
	    let val len = length_ s
		val table = array_ 256
		fun init i = 
		    if i < len then (update_ table (sub_ s i) 1; init (i+1))
		    else ()		    
	    in
		fill_ table 0 256 0;
		init 0;
		table
	    end
    in
	fun contains s = 
	    let val table = mktable s 
	    in fn c => subarray_ table c = 1 end

	fun notContains s = 
	    let val table = mktable s 
	    in fn c => subarray_ table c = 0 end
    end

    fun isLower c  = #"a" <= c andalso c <= #"z"
    fun isUpper c  = #"A" <= c andalso c <= #"Z"
    fun isDigit c  = #"0" <= c andalso c <= #"9"
    fun isAlpha c  = isLower c orelse isUpper c
    fun isHexDigit c = #"0" <= c andalso c <= #"9"
	               orelse #"a" <= c andalso c <= #"f"
	               orelse #"A" <= c andalso c <= #"F"
    fun isAlphaNum c = isAlpha c orelse isDigit c
(*    fun isPrint c  = c >= #" " andalso c <> #"\127" andalso c <> #"\255"  *)
    fun isPrint c  = c >= #" " andalso c < #"\127" 
    fun isSpace c  = c = #" " orelse #"\009" <= c andalso c <= #"\013"
    fun isGraph c  = isPrint c andalso not (isSpace c)
    fun isPunct c  = isGraph c andalso not (isAlphaNum c)
    fun isAscii c  = c <= #"\127"
(*    fun isCntrl c  = c < #" " orelse c = #"\127" orelse c = #"\255" *)
    fun isCntrl c  = c < #" " orelse c >= #"\127"

    fun toLower c = 
	if #"A" <= c andalso c <= #"Z" then chr_(ord c + 32)
	else c;
    fun toUpper c = 
	if #"a" <= c andalso c <= #"z" then chr_(ord c - 32)
	else c;

    fun toString c = Strbase.toMLescape c

    fun fromString s = 
	let fun getc i = if i < size s then SOME (sub_ s i, i+1) else NONE
	in 
	    case getc 0 of
		NONE              => NONE
	      | SOME(#"\\", rest) => (case Strbase.fromMLescape getc rest of
					  NONE       => NONE
					| SOME(c, _) => SOME c)
	      | SOME(c, _ )       => SOME c
	end

    fun fromCString s = 
	let fun getc i = if i < size s then SOME (sub_ s i, i+1) else NONE
	in 
	    case getc 0 of
		NONE              => NONE
	      | SOME(#"\\", rest) => (case Strbase.fromCescape getc rest of
					  NONE       => NONE
					| SOME(c, _) => SOME c)
	      | SOME(c, _ )       => SOME c
	end

    fun toCString c = Strbase.toCescape c;	

    fun compare (x, y: char) = 
	if x<y then LESS else if x>y then GREATER else EQUAL;

    val op <  = op < : char * char -> bool;
    val op <= = op <= : char * char -> bool;
    val op >  = op >  : char * char -> bool;
    val op >= = op >= : char * char -> bool;
end
