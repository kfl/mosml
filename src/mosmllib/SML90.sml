(* Old.sml 1995-02-24, 1996-04-09 -- compatibility with the Definition *)

fun (g o f) x = g (f x);
fun a before (b : unit) = a;

(* The definitions below implement the requirement that units
   Char, String and List are partially opened in the initial environment.
 *)

prim_val chr : int    -> string = 1 "sml_chr";
prim_val ord : string -> int    = 1 "sml_ord";

local 
    prim_val create_string_ : int -> string                = 1 "create_string";
    prim_val nth_char_      : string -> int -> int         = 2 "get_nth_char";
    prim_val set_nth_char_  : string -> int -> int -> unit = 3 "set_nth_char";
    prim_val blit_string_   : string -> int -> string -> int -> int -> unit 
                                                           = 5 "blit_string";

    open String
in 
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

exception Abs   = Overflow
      and Diff  = Overflow
      and Exp   = Overflow
      and Floor = Overflow
      and Neg   = Overflow
      and Prod  = Overflow
      and Sum   = Overflow
      and Quot  = Overflow
      and Mod   = Div;

prim_val sqrt   : real -> real = 1 "sml_sqrt";
prim_val sin    : real -> real = 1 "sml_sin";
prim_val cos    : real -> real = 1 "sml_cos";
prim_val arctan : real -> real = 1 "atan_float";
prim_val exp    : real -> real = 1 "sml_exp";
prim_val ln     : real -> real = 1 "sml_ln";

type instream = BasicIO.instream and outstream = BasicIO.outstream

val std_in = BasicIO.std_in
fun open_in s = BasicIO.open_in s
fun input(is, n) = BasicIO.input (is, n)
fun lookahead is = BasicIO.lookahead is
fun close_in is = BasicIO.close_in is
fun end_of_stream is = BasicIO.end_of_stream is

val std_out = BasicIO.std_out
fun open_out s = BasicIO.open_out s
fun output(os, s) = BasicIO.output(os, s)
fun close_out os = BasicIO.close_out os
