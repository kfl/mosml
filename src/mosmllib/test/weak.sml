(* weak.sml -- test weak pointers -- 1998-01-10 *)

(* May be run with         
	camlrunm -v ~/mosml/lib/mosmltop -stdlib ~/mosml/lib -P full weak.sml
   if you want to see how tests interleave with garbage collection.
*)

use "auxil.sml";

val _ = load "Weak";

local

fun mklist 0 = [] | mklist n = n :: mklist (n-1)
fun exercisegc n = ignore (mklist n)
open Weak

in 

val a = array 1000 : int list array;

val _ = print "test1:\n";

val test1 = (sub(a, 7); "WRONG") 
            handle Fail _ => "OK" | _ => "WRONG";

val _ = print "test2:\n";

val test2 = (update(a, 7, mklist 30);
	     exercisegc 100000;
	     if isdead(a, 7) then () else raise Fail "ak";
	     sub(a, 7); 
	     "WRONG") 
            handle Fail _ => "OK" | _ => "WRONG";

val _ = print "test3:\n";

val test3 = 
    check'(fn _ => 
	   let val xs = mklist 30
	   in 
	       update(a, 11, xs);
	       exercisegc 100000;
	       not (isdead(a, 11)) andalso sub(a, 11) = xs
	   end)

val _ = print "test4:\n";
 
val test4 = (exercisegc 100000;
	     if isdead(a, 11) then () else raise Fail "ak";
	     sub(a, 11);
	     "WRONG") 
            handle Fail _ => "OK" | _ => "WRONG";

val _ = print "test5:\n";
 
val b = array 200 : int array

val test5 = check'(fn _ => (update(b, 11, 1000+8);
			    exercisegc 100000;
			    not (isdead(b, 11)) andalso sub(b, 11) = 1008));

val _ = print "test6:\n";

val c = array 200 : string array

val test6 = check'(fn _ => (update(c, 0, Int.toString 117 ^ " kroner");
			    not (isdead(c, 0)) andalso sub(c, 0) = "117 kroner"));
val _ = print "test7:\n";

val test7 = (exercisegc 100000;
	     if isdead(c, 0) then () else raise Fail "ak";
	     sub(c, 0);
	     "WRONG")
            handle Fail _ => "OK" | _ => "WRONG";

val _ = print "test8:\n";

val test8 = 
    check'( fn _ => (update(c, 1, Real.toString Math.pi);
		     while not (isdead(c, 1)) do 
			 (print "/"; exercisegc 5000);
			 print "\n";
			 isdead(c, 1)))
end
