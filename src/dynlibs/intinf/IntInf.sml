(* LargeInt -- interface to the GNU Multiple-Precision Library GMP *)

exception Domain

prim_type int;
type largeint = int;

(* Interfacing to the C functions that call the GNU GMP library: *)

local 
    (* 1. Load the C dynamic library libmgmp.so, and get a handle to it.
          Assume libmgmp.so is in a directory which is in the LD_LIBRARY_PATH,
	  or that it is in a system library directory and that ldconfig has 
	  been run: *)

    open Dynlib
    val dlh = dlopen { lib = "libmgmp.so",
		       flag = RTLD_LAZY, 
		       global = false }

in

    (* 2. Define SML functions using this handle.  Type ascriptions
          are necessary for SML type safety:                            *)

val largeint_make    : unit -> largeint        
    = app1 (dlsym dlh "largeint_make")
val largeint_make_si : Int.int -> largeint 
    = app1 (dlsym dlh "largeint_make_si")
val largeint_clear   : largeint -> unit
    = app1 (dlsym dlh "largeint_clear")
val largeint_set     : largeint -> largeint -> unit
    = app2 (dlsym dlh "largeint_set")
val largeint_set_si  : largeint -> Int.int -> unit
    = app2 (dlsym dlh "largeint_set_si")
val largeint_to_si   : largeint -> Int.int
    = app1 (dlsym dlh "largeint_to_si")
val largeint_neg     : largeint -> largeint -> unit
    = app2 (dlsym dlh "largeint_neg")
val largeint_add     : largeint -> largeint -> largeint -> unit
    = app3 (dlsym dlh "largeint_add")
val largeint_sub     : largeint -> largeint -> largeint -> unit
    = app3 (dlsym dlh "largeint_sub")
val largeint_mul     : largeint -> largeint -> largeint -> unit
    = app3 (dlsym dlh "largeint_mul")
val largeint_tdiv    : largeint -> largeint -> largeint -> unit
    = app3 (dlsym dlh "largeint_tdiv")
val largeint_tmod    : largeint -> largeint -> largeint -> unit
    = app3 (dlsym dlh "largeint_tmod")
val largeint_fdiv    : largeint -> largeint -> largeint -> unit
    = app3 (dlsym dlh "largeint_fdiv")
val largeint_fmod    : largeint -> largeint -> largeint -> unit
    = app3 (dlsym dlh "largeint_fmod")
val largeint_fdivmod : largeint -> largeint -> largeint -> largeint -> unit
    = app4 (dlsym dlh "largeint_fdivmod")
val largeint_tdivmod : largeint -> largeint -> largeint -> largeint -> unit
    = app4 (dlsym dlh "largeint_tdivmod")
val largeint_cmp     : largeint -> largeint -> Int.int 
    = app2 (dlsym dlh "largeint_cmp")
val largeint_cmp_si  : largeint -> Int.int -> Int.int 
    = app2 (dlsym dlh "largeint_cmp_si")
val largeint_sizeinbase : largeint -> Int.int -> Int.int 
    = app2 (dlsym dlh "largeint_sizeinbase")
val largeint_get_str : largeint -> Int.int -> string
    = app2 (dlsym dlh "largeint_get_str")
val largeint_set_str : largeint -> string -> Int.int -> unit
    = app3 (dlsym dlh "largeint_set_str")
val largeint_pow_ui  : largeint -> largeint -> Int.int -> unit
    = app3 (dlsym dlh "largeint_pow_ui")
end

val onepos = largeint_make_si 1
and zero   = largeint_make_si 0
and oneneg = largeint_make_si ~1;

val precision = NONE
val minInt    = NONE
val maxInt    = NONE

fun fromInt  0 = zero
  | fromInt  1 = onepos
  | fromInt ~1 = oneneg
  | fromInt si = largeint_make_si si;

fun toInt li = 
    case (Int.minInt, Int.maxInt) of
	(SOME  defaultMinint, SOME defaultMaxint) =>
	    if largeint_cmp_si li defaultMinint <> ~1 
		andalso largeint_cmp_si li defaultMaxint <> 1 
	    then
		    largeint_to_si li
	    else 
		raise Overflow
      | _  => raise Fail "LargeInt.from: internal error";

fun fromLarge x = x
fun toLarge x = x

fun sign li = largeint_cmp_si li 0;    

fun sameSign(li1, li2) = sign li1 = sign li2;

fun compare(li1, li2) =
    case largeint_cmp li1 li2 of
	~1 => LESS
      |  0 => EQUAL
      |  1 => GREATER
      |  _ => raise Fail "LargeInt.compare: internal error";

fun ~ li = 
    let val res = largeint_make ()
    in largeint_neg res li; res end;

fun abs li = 
    if sign li = ~1 then ~ li else li;

fun li1 div li2 = 
    if sign li2 = 0 then raise Div 
    else
	let val res = largeint_make ()
	in largeint_fdiv res li1 li2; res end

fun li1 mod li2 = 
    if sign li2 = 0 then raise Div 
    else
	let val res = largeint_make ()
	in 
	    largeint_fmod res li1 li2; 
	    res
	end;

fun quot(li1, li2) = 
    if sign li2 = 0 then raise Div 
    else
	let val res = largeint_make ()
	in largeint_tdiv res li1 li2; res end

fun rem(li1, li2) = 
    if sign li2 = 0 then raise Div 
    else
	let val res = largeint_make ()
	in largeint_tmod res li1 li2; res end

fun divMod(li1, li2) = 
    if sign li2 = 0 then raise Div 
    else
	let val divres = largeint_make ()
	    and modres = largeint_make ()
	in 
	    largeint_fdivmod divres modres li1 li2; 
 	    (divres, modres) 
	end;

fun quotRem(li1, li2) = 
    if sign li2 = 0 then raise Div 
    else
	let val quotres = largeint_make ()
	    and remres  = largeint_make ()
	in largeint_tdivmod quotres remres li1 li2; (quotres, remres) end;

fun log2 li = 
    if Int.<=(sign li, 0) then raise Domain
    else largeint_sizeinbase li 2;

fun pow(li, exp) = 
    if Int.<(exp, 0) then 
	if sign li = 0 then raise Div 
	else if largeint_cmp li onepos = 0 then onepos
        else if largeint_cmp li oneneg = 0 then oneneg
	else zero
    else if exp = 0 then onepos
    else
	let val res = largeint_make () 
	in largeint_pow_ui res li exp; res end;

fun fmt radix li = 
    let open StringCvt
    in case radix of 
	BIN => largeint_get_str li  2
      | OCT => largeint_get_str li  8
      | DEC => largeint_get_str li 10
      | HEX => largeint_get_str li 16
    end;
    
fun toString li = largeint_get_str li 10;

local 
    open StringCvt
    fun skipWSget getc source = getc (skipWS getc source)
    prim_val sub_      : string -> Int.int -> char = 2 "get_nth_char";
    prim_val mkstring_ : Int.int -> string         = 1 "create_string";
    prim_val update_   : string -> Int.int -> char -> unit 
						       = 3 "set_nth_char";
in
    fun scan radix getc source =
	let open StringCvt;

	    val (isDigit, factor) = 
		case radix of
		    BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		  | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		  | DEC => (Char.isDigit,                          10)
		  | HEX => (Char.isHexDigit,                       16)

	    fun revimplode cs = 
		let val n = List.length cs
		    val newstr = if n > String.maxSize then raise Size 
				 else mkstring_ n
		    fun init []      i = ()
		      | init (c::cr) i = (update_ newstr i c; init cr (i-1))
		in (init cs (n-1); newstr) end;

	    fun makelarge digits src = 
		let val str = revimplode digits
		    val res = largeint_make ()
		in 
		    (largeint_set_str res str factor; SOME (res, src))
		    handle Fail _ => NONE
		end

	    fun dig1 digits NONE             = NONE
	      | dig1 digits (SOME (c, rest)) = 
		let fun digr digits src = 
		    case getc src of
			NONE           => makelarge digits src
		      | SOME (c, rest) => 
			    if isDigit c then digr (c :: digits) rest
			    else makelarge digits src
		in 
		    if isDigit c then digr (c :: digits) rest 
		    else NONE 
		end
	    fun sign NONE                = NONE
	      | sign (SOME (#"~", rest)) = dig1  [#"-"] (getc rest)
	      | sign (SOME (#"-", rest)) = dig1  [#"-"] (getc rest)
	      | sign (SOME (#"+", rest)) = dig1  []     (getc rest)
	      | sign inp                 = dig1  []     inp	    
	in sign (skipWSget getc source) end;
	    
    val fromString = scanString (scan DEC)
end

val op <  = fn (li1, li2) => (largeint_cmp li1 li2) <  0;
val op <= = fn (li1, li2) => (largeint_cmp li1 li2) <= 0;
val op >  = fn (li1, li2) => (largeint_cmp li1 li2) >  0;
val op >= = fn (li1, li2) => (largeint_cmp li1 li2) >= 0;

fun eq(li1, li2) = (largeint_cmp li1 li2) =  0;
fun ne(li1, li2) = (largeint_cmp li1 li2) <> 0;

fun min(li1, li2) = if li1 < li2 then li1 else li2;

fun max(li1, li2) = if li1 > li2 then li1 else li2;

fun op+ (li1, li2) = 
    let val res = largeint_make ()
    in largeint_add res li1 li2; res end;

fun op - (li1, li2) = 
    let val res = largeint_make ()
    in largeint_sub res li1 li2; res end;

fun op * (li1, li2) = 
    let val res = largeint_make ()
    in largeint_mul res li1 li2; res end;
