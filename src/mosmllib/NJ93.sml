(* NJ93.sml 1995-02-24, 1995-11-22, 1996-07-12
   Half-way compatibility with those SML/NJ 0.93 basis structures
   which were open in the initial environment. *)

fun print s = TextIO.print s

(* NJ93 Integer *)

fun max (x, y) = if x > y then x else y : int;
fun min (x, y) = if x > y then y else x : int;

(* NJ93 List *)

exception Hd and Tl and Nth and NthTail

fun hd arg      = (List.hd arg) handle Empty => raise Hd;
fun tl arg      = (List.tl arg) handle Empty => raise Tl;
fun nthtail arg = (List.drop arg) handle Subscript => raise NthTail;
fun nth arg     = (List.nth arg) handle Subscript => raise Nth;

fun app f xs =
    let fun h []      = ()
	  | h (x::xr) = (f x; h xr)
    in h xs end;
fun revapp f xs =
    let fun h []      = ()
	  | h (x::xr) = (h xr; f x; ())
    in h xs end;

fun fold f xs e    = List.foldr f e xs;
fun revfold f xs e = List.foldl f e xs;

(* NJ93 Real *)

fun ceiling r  = ceil r
fun truncate r = trunc r

(* NJ93 Ref *)

fun inc r = r := !r+1;
fun dec r = r := !r-1;

(* NJ93 String *)

prim_val chr : int    -> string = 1 "sml_chr";
prim_val ord : string -> int    = 1 "sml_ord";

local 
    prim_val create_string_ : int -> string                = 1 "create_string";
    prim_val nth_char_      : string -> int -> int         = 2 "get_nth_char";
    prim_val set_nth_char_  : string -> int -> int -> unit = 3 "set_nth_char";
    prim_val blit_string_   : string -> int -> string -> int -> int -> unit 
                                                           = 5 "blit_string";
in 
    exception Substring;
    fun ordof(s, i) = 
	if i < 0 orelse i >= size s then raise Ord
	else nth_char_ s i;
    fun substring (s, i, n) = (String.substring (s, i, n)) 
	                      handle Subscript => raise Substring;
    fun explode s =
	let fun loop 0 acc = acc
	      | loop n acc =
		let val n' = n - 1
		    val x = create_string_ 1
		in
		    set_nth_char_ x 0 (nth_char_ s n');
		    loop n' (x::acc)
		end
	in loop (size s) [] end;

    fun implode ss =
	let fun resultSizeAcc [] acc = acc
	      | resultSizeAcc (s::ss) acc = resultSizeAcc ss (acc + size s)
	    val rlen = resultSizeAcc ss 0
	    val r = create_string_ rlen
	    fun loop [] i = ()
	      | loop (s::ss) i =
		let val slen = size s in
		    blit_string_ s 0 r i slen;
		    loop ss (i + slen)
		end
	in loop ss 0; r end;
end;

(* NJ93 top-level math functions *)

prim_val sqrt   : real -> real = 1 "sml_sqrt";
prim_val sin    : real -> real = 1 "sml_sin";
prim_val cos    : real -> real = 1 "sml_cos";
prim_val arctan : real -> real = 1 "atan_float";
prim_val exp    : real -> real = 1 "sml_exp";
prim_val ln     : real -> real = 1 "sml_ln";

(* NJ93 top-level input/output *)

open BasicIO

