(* This test works only for 32-bit implementations! *)

val maxint = 1073741823;
val minint = ~maxint -1;
infix seq
fun e1 seq e2 = e2;

(~minint    seq "WRONG") handle Overflow => "OK";
(abs minint seq "WRONG") handle Overflow => "OK";
(maxint+1   seq "WRONG") handle Overflow => "OK";
(minint-1   seq "WRONG") handle Overflow => "OK";

if maxint =  0x3fffffff then "OK" else "WRONG";
if maxint =  0x3FFFFFFF then "OK" else "WRONG";
if minint = ~0x40000000 then "OK" else "WRONG";

val sum = (op+) : int * int -> int;
val diff = (op-) : int * int -> int;
(sum (maxint,1)  seq "WRONG") handle Overflow => "OK";
(diff (minint,1) seq "WRONG") handle Overflow => "OK";

(minint * ~1 seq  "WRONG") handle Overflow => "OK";

val prod = (op * ) : int * int -> int;
(prod (minint,~1) seq "WRONG") handle Overflow => "OK";

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
(minint div ~1 seq  "WRONG") handle Overflow => "OK";

val maxri = real maxint;
val minri = real minint;

if floor 3.0 = 3 then "OK" else "WRONG";
if floor 3.14 = 3 then "OK" else "WRONG";
if floor ~3.0 = ~3 then "OK" else "WRONG";
if floor ~3.14 = ~4 then "OK" else "WRONG";
if floor(maxri + 0.9) = maxint then "OK" else "WRONG";
if floor minri = minint then "OK" else "WRONG";
(floor (minri - 0.1) seq  "WRONG") handle Overflow => "OK";
(floor (maxri + 1.0) seq  "WRONG") handle Overflow => "OK";
