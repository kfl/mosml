(* mosml/src/mosmllib/test/callback/testcallback.sml -- test Callback.

   The SML side of things -- sestoft@dina.kvl.dk 1999-08-09 *)

app load ["Int", "Dynlib", "Callback", "Mosml"];

use "../auxil.sml";

open Callback;

(* Obtain a handle pointing to the library defining the C functions, 
   and make the C side register a number of C functions: *)

val dlh = Dynlib.dlopen { lib = "./libcside.so",
			  flag = Dynlib.RTLD_LAZY, 
			  global = false }

val _ = Dynlib.app1 (Dynlib.dlsym dlh "initialize_callbacktest") ();


(* TESTING THE REGISTRATION AND USE OF C VALUES *)

(* Define SML functions that call the registered C functions *)

(* Passing and returning base types: unit, int, char, real, string, bool: *)

val fu : unit   -> unit   = app1 (getcptr "regcfu");
val fi : int    -> int    = app1 (getcptr "regcfi");
val fc : char   -> char   = app1 (getcptr "regcfc");
val fr : real   -> real   = app1 (getcptr "regcfr");
val fs : string -> string = app1 (getcptr "regcfs");
val fb : bool   -> bool   = app1 (getcptr "regcfb");

(* Passing several curried arguments: *)

val fcur : int -> char -> real -> string -> bool -> int = 
    app5 (getcptr "regcfcur");

(* Passing a tuple: *)

val ftup : int * char * real -> int = 
    app1 (getcptr "regcftup");

(* Passing a record: *)

val frec : { surname : string, givenname : string, age : int } -> bool =
    app1 (getcptr "regcfrec");

(* Passing a constructed value belonging to a datatype: *)

datatype t = 
    Lf 
  | Br of int * t * t
  | Brs of t list

val fdat : t -> int = 
    app1 (getcptr "regcfdat");

(* Passing an ML function (a closure): *)

val ffun : (int -> real) -> int -> string = 
    app2 (getcptr "regcffun");

(* Returning a tuple *)

val frtup : int -> int * bool = 
    app1 (getcptr "regcfrtup");

(* Returning a record *)

val frrec : int -> { half : int, odd : bool } = 
    app1 (getcptr "regcfrrec");

(* Illustration of heap allocation trickiness *)

val fconcat : string -> string -> string = 
    app2 (getcptr "regcfconcat");

(* Exercising the C functions: *)

val test1 = () = fu ();

val test2 = 5667 = fi 5666;

val test3 = #"T" = fc #"t";

val test4 = 56.0 = fr 28.0;

val test5 = "TEST NUMBER +1" = fs "Test number +1";

val test6 = fb false;

val test7 = 85 = fcur 5 #"A" 10.0 "blah" true; 

val test8 = 80 = ftup(5, #"A", 10.0);

val test9 = not (frec {surname = "Jensen", givenname = "Karin", age = 28 });

local 
    val tree = Brs[Lf, 
		   Br(12, Br(10, Lf, Lf), Br(20, Lf, Lf)),
		   Brs[Br(15, Lf, Lf)]]
in
    val test10 = 57 = fdat tree
end

val test11 = ("Just right" = ffun (fn x => real x + 7.0) 100000);

val test12 = (8, true) = frtup 17;

val test13 = {half = 8, odd = true} = frrec 17;

val test14 = "abcdef" = fconcat "abc" "def";


(* TESTING THE REGISTRATION AND USE OF ML VALUES *)

val test15 = app1 (getcptr "getting_notreg") () : bool;

val test16 = (register "unregistered" (fn x => x); 
	      unregister "unregistered";
	      app1 (getcptr "getting_unreg") ()) : bool;

val test17 = (app1 (getcptr "using_notreg") (); false)
             handle Fail _ => true | _ => false : bool;

val test18 = (register "temp1" (fn x => x);
	      app1 (getcptr "using_unreg") ()) : bool;

fun mkfun extra = 
    let fun f r = r + extra
    in f end

(* On a 266 MHz Pentium II notebook this does 1.25 million callbacks/sec: *)

val test19 = 
    let val a = 2.5 
	val b = 1000000
	val c = 10.0
	val _ = (register "extrafun" (mkfun a); register "steps" b)
	val res = Mosml.time (app1 (getcptr "call function")) c : real
	val expected = a * real b + c 
    in abs(expected - res) < 0.0001 end 

(* On a 266 MHz Pentium II notebook this does 1.5 million callbacks/sec: *)

val test20 = 
    let val a = 1
	val b = 1000000
	val c = 17
	val _ = app unregister ["extrafun", "steps"]
	val _ = (register "extrafun" (fn x => x+a); register "steps" b)
	val res = Mosml.time (app1 (getcptr "call function")) c : int
	val expected = a * b + c 
    in expected = res end 

val _ = quit();
