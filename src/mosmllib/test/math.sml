(* test/math.sml 
   PS 1995-02-25, 1996-04-01, 1997-03-07
*)

use "auxil.sml";

local
    open Math
    val MAXDOUBLE = 8.98846567431157E307;
    (* This is too much for Linux/Alpha for some reason: *)
    val MINDOUBLE = 4.94065645841246544E~324
    val PI = 3.14159265358979323846;
    val E = 2.7182818284590452354;

    val eps = 1E~8
    infix 4 ===
    fun x === y = 
	abs (x - y) <= eps orelse abs(x-y) <= eps * (abs x + abs y)

    fun check1 (opr, a, r) = if opr a === r then "OK" else "WRONG"
    fun check2 (opr, a1, a2, r) =
	if opr(a1, a2) === r then "OK" else "WRONG"
in

val test0a = check(PI === pi);
val test0b = check(E === e);

val test1a = check1(sqrt, 64.0, 8.0);
val test1b = check1(sqrt, 0.0, 0.0);
val test1c = (sqrt ~1.0 seq "WRONG") handle Domain => "OK" | _ => "WRONG";

val test2a = check1(sin, 0.0, 0.0);
val test2b = check1(sin, pi/2.0, 1.0);
val test2c = check1(sin, pi, 0.0);
val test2d = check1(sin, 3.0*pi/2.0, ~1.0);

val test3a = check1(cos, 0.0, 1.0);
val test3b = check1(cos, pi/2.0, 0.0);
val test3c = check1(cos, pi, ~1.0);
val test3d = check1(cos, 3.0*pi/2.0, 0.0);

val test4a = check1(tan, 0.0, 0.0);
val test4b = check1(tan, pi/4.0, 1.0);
val test4c = check1(tan, pi, 0.0);
val test4d = check1(tan, 3.0*pi/4.0, ~1.0);
val test4e = check1(tan, ~pi/4.0, ~1.0);
val test4f = check((abs(tan (pi/2.0))  > 1E8) handle _ => true);
val test4g = check((abs(tan (~pi/2.0)) > 1E8) handle _ => true);

val test5a = check1(asin, 0.0, 0.0);
val test5b = check1(asin, 1.0, pi/2.0);
val test5c = check1(asin, ~1.0, ~pi/2.0);
val test5d = (asin 1.1 seq "WRONG") handle Domain => "OK" | _ => "WRONG";
val test5e = (asin ~1.1 seq "WRONG") handle Domain => "OK" | _ => "WRONG";

val test6a = check1(acos, 1.0, 0.0);
val test6b = check1(acos, 0.0, pi/2.0);
val test6c = check1(acos, ~1.0, pi);
val test6d = (acos 1.1 seq "WRONG") handle Domain => "OK" | _ => "WRONG";
val test6e = (acos ~1.1 seq "WRONG") handle Domain => "OK" | _ => "WRONG";

val test7a = check1(atan, 0.0, 0.0);
val test7b = check1(atan, 1.0, pi/4.0);
val test7c = check1(atan, ~1.0, ~pi/4.0);
val test7d = check1(atan, 1E8, pi/2.0);
val test7e = check1(atan, ~1E8, ~pi/2.0);

(* atan2 -- here I am in doubt over the argument order, since the New
Basis document is inconsistent with itself and with atan2 in the C
libraries. *)

val test8a = check2(atan2, 0.0, 0.0, 0.0);
val test8b = check2(atan2, 1.0, 0.0, pi/2.0);
val test8c = check2(atan2, ~1.0, 0.0, ~pi/2.0);
val test8d = check2(atan2, 1.0, 1.0, pi/4.0);
val test8e = check2(atan2, ~1.0, 1.0, ~pi/4.0);
val test8f = check2(atan2, ~1.0, ~1.0, ~3.0*pi/4.0);
val test8g = check2(atan2, 1.0, ~1.0, 3.0*pi/4.0);
val test8h = check2(atan2, 1E8, 1.0, pi/2.0);
val test8i = check2(atan2, ~1E8, 1.0, ~pi/2.0);
val test8j = check2(atan2, 1.0, 1E8, 0.0);
val test8k = check2(atan2, 1.0, ~1E8, pi);
val test8l = check2(atan2, ~1.0, ~1E8, ~pi);

val test9a = check1(exp, 0.0, 1.0);
val test9b = check1(exp, 1.0, e);
val test9c = check1(exp, ~1.0, 1.0/e);

val test10a = check1(ln, 1.0, 0.0);
val test10b = check1(ln, e, 1.0);
val test10c = check1(ln, 1.0/e, ~1.0);
val test10d = (ln 0.0 seq "WRONG") handle Domain => "OK" | _ => "WRONG";
val test10e = (ln ~1.0 seq "WRONG") handle Domain => "OK" | _ => "WRONG";

val test12a = check2(pow, 0.0, 0.0, 1.0); (* arbitrary, might be 0.0 *)
val test12b = check2(pow, 7.0, 0.0, 1.0); 
val test12c = check2(pow, 0.0, 7.0, 0.0); 
val test12d = check2(pow, 64.0, 0.5, 8.0); 
val test12e = check2(pow, ~9.0, 2.0, 81.0); 
val test12f = check2(pow, 10.0, ~2.0, 0.01); 
val test12g = check2(pow, ~10.0, ~2.0, 0.01); 
val test12h = check2(pow, 0.0, 0.5, 0.0); 
val test12i = check2(pow, 0.4, ~2.0, 6.25); 

val test12j = (pow(0.0, ~1.0) seq "WRONG") 
              handle Domain => "OK" | _ => "WRONG";
val test12k = (pow(0.0, ~0.5) seq "WRONG") 
              handle Domain => "OK" | _ => "WRONG";
val test12l = (pow(~1.0, 1.1) seq "WRONG") 
              handle Domain => "OK" | _ => "WRONG";
val test12m = (pow(~1.0, 0.5) seq "WRONG") 
              handle Domain => "OK" | _ => "WRONG";
(* val test12n = (pow(3.0, 1000000.0) seq "WRONG") 
              handle Overflow => "OK" | _ => "WRONG";
*)
val test13a = check1(log10, 1.0, 0.0);
val test13b = check1(log10, 10.0, 1.0);
val test13c = check1(log10, 100.0, 2.0);
val test13d = check1(log10, 0.1, ~1.0);
val test13e = check1(log10, 0.01, ~2.0);

val check14a = check1(sinh, 0.0, 0.0);
val check14b = check1(sinh,  1.0, 1.17520119364);
val check14c = check1(sinh, ~1.0, ~1.17520119364);
val check14d = check1(sinh,  2.0,  3.62686040785);
val check14e = check1(sinh, ~2.0, ~3.62686040785);

val check15a = check1(cosh, 0.0, 1.0);
val check15b = check1(cosh,  1.0, 1.54308063482);
val check15c = check1(cosh, ~1.0, 1.54308063482);
val check15d = check1(cosh,  2.0, 3.76219569108);
val check15e = check1(cosh, ~2.0, 3.76219569108);

val check15a = check1(tanh, 0.0, 0.0);
val check15b = check1(tanh,  1.0,  0.761594155956);
val check15c = check1(tanh, ~1.0, ~0.761594155956);
val check15d = check1(tanh,  2.0,  0.964027580076);
val check15e = check1(tanh, ~2.0, ~0.964027580076);
val check15f = check1(tanh,  100.0,  1.0);
val check15g = check1(tanh, ~100.0, ~1.0);
end
