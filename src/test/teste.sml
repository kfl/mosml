(* Floating-point exceptions.  Works for 64-bit arithmetic on PCs *)

val MAXDOUBLE = 8.98846567431157E307;
val MINDOUBLE = 1e~307;

(* This value is too small for DEC Alpha:
val MINDOUBLE = 4.940656458412465442e~324 *)

val pi = 3.14159265358979323846;
val eps = 1E~14;
infix seq
fun e1 seq e2 = e2;

fun check1 (opr, a, r) =
    let val res = opr a
    in
	if r = 0.0 andalso abs res <= eps orelse abs (res/r - 1.0) <= eps
	then "OK" else "WRONG"
    end;

check1(abs, 1.9E~212, 1.9E~212);
check1(abs, ~1.9E~212, 1.9E~212);
check1(~, 1.9E~212, ~1.9E~212);
check1(~, ~1.9E~212, 1.9E~212);
check1(real, 515, 515.0);
check1(real, ~515, ~515.0);

fun check2 (opr, a1, a2, r) =
    let val res = opr(a1, a2)
    in
	if r = 0.0 andalso abs res <= eps orelse abs (res/r - 1.0) <= eps
	then "OK" else "WRONG"
    end;

check2(op+, 1.6, 2.3, 3.9);
check2(op+, ~1E123, 2E124, 190E122);
check2(op-, 16.0, 28.0, ~12.0);
check2(op-, ~8E23, 4E24, ~480E22);
check2(op*, 1E100, 1.234E8, 1.234E108);
check2(op*, 1E~100, 1.234E~8, 1.234E~108);
check2(op/, 0E300, 1.0, 0.0);
check2(op/, 1.0, ~1E~80, ~1E80);

(1.0/0.0    seq "WRONG") handle Div => "OK";
((~1.0)/0.0 seq "WRONG") handle Div => "OK";
(1.0/(~0.0) seq "WRONG") handle DIv => "OK";

(MAXDOUBLE + 1E300       seq "WRONG") handle Overflow => "OK";
(~MAXDOUBLE - 1E300      seq "WRONG") handle Overflow => "OK";
(MAXDOUBLE * 1.000000001 seq "WRONG") handle Overflow => "OK";
(1E30 / MINDOUBLE        seq "WRONG") handle Overflow => "OK";

if MAXDOUBLE + ~MAXDOUBLE = 0.0 then "OK" else "WRONG";

fun f x = let val x2 = x / 2.0;
	  in MINDOUBLE/x2 + f x2 handle Div => 1.17 end;
