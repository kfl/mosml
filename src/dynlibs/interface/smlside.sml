(* mosml/src/dynlibs/interface/smlside.sml -- foreign function interface.
   How to pass SML values to C functions, and how to pass them back.
   The SML side of things.                 sestoft@dina.kvl.dk 1998-05-05 *)

(* Load the Dynlib and FileSys structures from the Moscow ML library: *)

app load ["Dynlib", "FileSys"];

open Dynlib;

(* Obtain a handle pointing to the library defining the C functions: *)

val dlh = dlopen { lib = Path.concat(FileSys.getDir (), "libcside.so"),
		   flag = RTLD_LAZY, 
		   global = false }

(* Define SML functions using this handle.  The type ascriptions are
   necessary for SML type safety: 
 *)

(* Passing and returning base types: unit, int, char, real, string, bool: *)

val fu : unit   -> unit   = app1 (dlsym dlh "cfu")
val fi : int    -> int    = app1 (dlsym dlh "cfi")
val fc : char   -> char   = app1 (dlsym dlh "cfc")
val fr : real   -> real   = app1 (dlsym dlh "cfr")
val fs : string -> string = app1 (dlsym dlh "cfs")
val fb : bool   -> bool   = app1 (dlsym dlh "cfb")

(* Passing several curried arguments: *)

val fcur : int -> char -> real -> string -> bool -> int = 
    app5 (dlsym dlh "cfcur")

(* Passing a tuple: *)

val ftup : int * char * real -> int = 
    app1 (dlsym dlh "cftup")

(* Passing a record: *)

val frec : { surname : string, givenname : string, age : int } -> bool =
    app1 (dlsym dlh "cfrec")

(* Passing a constructed value belonging to a datatype:                 
   It is advisable to declare the constructors in alphabetical order 
   since Moscow ML 2.00 and later sorts them anyway, and the C side 
   must use the tags (0, 1, ...) of the sorted constructors.
*)

datatype t = 
    Br of int * t * t
  | Brs of t list
  | Lf 

val fdat : t -> int = 
    app1 (dlsym dlh "cfdat")

(* Passing an ML function (a closure): *)

val ffun : (int -> real) -> int -> string = 
    app2 (dlsym dlh "cffun");

(* Returning a tuple *)

val frtup : int -> int * bool = 
    app1 (dlsym dlh "cfrtup");

(* Returning a record *)

val frrec : int -> { half : int, odd : bool } = 
    app1 (dlsym dlh "cfrrec");

(* Illustration of heap allocation trickiness *)

val fconcat : string -> string -> string = 
    app2 (dlsym dlh "cfconcat");

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

val _ = quit();
