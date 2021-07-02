(* This test works only for 32-bit implementations! *)

val maxint = 1073741823;
val minint = ~maxint -1;
infix seq
fun e1 seq e2 = e2;


val test1 = if Int.precision = SOME 31
            then (~minint    seq "WRONG") handle Overflow => "OK"
            else "TODO";
val test2 = if Int.precision = SOME 31
            then (abs minint seq "WRONG") handle Overflow => "OK"
            else "TODO";
val test3 = if Int.precision = SOME 31
            then (maxint+1   seq "WRONG") handle Overflow => "OK"
            else "TODO";
val test4 = if Int.precision = SOME 31
            then (minint-1   seq "WRONG") handle Overflow => "OK"
            else "TODO";

if maxint =  0x3fffffff then "OK" else "WRONG";
if maxint =  0x3FFFFFFF then "OK" else "WRONG";
if minint = ~0x40000000 then "OK" else "WRONG";

val sum = (op+) : int * int -> int;
val diff = (op-) : int * int -> int;

val test5 = if Int.precision = SOME 31
            then (sum (maxint,1)  seq "WRONG") handle Overflow => "OK"
            else "TODO";
val test6 = if Int.precision = SOME 31
            then (diff (minint,1) seq "WRONG") handle Overflow => "OK"
            else "TODO";
val test7 = if Int.precision = SOME 31
            then (minint * ~1 seq  "WRONG") handle Overflow => "OK"
            else "TODO";

val prod = (op * ) : int * int -> int;
val test8 = if Int.precision = SOME 31
            then (prod (minint,~1) seq "WRONG") handle Overflow => "OK"
            else "TODO";

fun checkDivMod i d =
  let val q = i div d
      val r = i mod d
  in 
      printVal i seq TextIO.output(TextIO.stdOut, " "); 
      printVal d seq TextIO.output(TextIO.stdOut, "   ");
      if (d * q + r = i) andalso
	  ((0 <= r andalso r < d) orelse (d < r andalso r <= 0))
      then "OK" else "WRONG: problems with div, mod"
  end;

checkDivMod 23 10;
checkDivMod ~23 10;
checkDivMod 23 ~10;
checkDivMod ~23 ~10;

checkDivMod 100 10;
checkDivMod ~100 10;
checkDivMod 100 ~10;
checkDivMod ~100 ~10;

checkDivMod 100 1;
checkDivMod 100 ~1;
checkDivMod 0 1;
checkDivMod 0 ~1;

(100 div 0     seq  "WRONG") handle Div => "OK";
(100 mod 0     seq  "WRONG") handle Div => "OK";
val test9 = if Int.precision = SOME 31
            then (minint div ~1 seq  "WRONG") handle Overflow => "OK"
            else "TODO";

val maxri = real maxint;
val minri = real minint;

if floor 3.0 = 3 then "OK" else "WRONG";
if floor 3.14 = 3 then "OK" else "WRONG";
if floor ~3.0 = ~3 then "OK" else "WRONG";
if floor ~3.14 = ~4 then "OK" else "WRONG";
if floor(maxri + 0.9) = maxint then "OK" else "WRONG";
if floor minri = minint then "OK" else "WRONG";
val test9 = if Int.precision = SOME 31
            then (floor (minri - 0.1) seq  "WRONG") handle Overflow => "OK"
            else "TODO";
val test10 = if Int.precision = SOME 31
            then (floor (maxri + 1.0) seq  "WRONG") handle Overflow => "OK"
            else "TODO";
