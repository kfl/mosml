(* file "test/largeint.sml" PS 1995-09-05, 1998-04-12 *)

load "IntInf";

use "../../mosmllib/test/auxil.sml";

local 
    open IntInf
    fun divmod1 (i, d, q, r)  = 
	check'(fn () => (toInt (fromInt i div fromInt d) = q 
			 andalso toInt(fromInt i mod fromInt d) = r));
    fun quotrem1 (i, d, q, r) = 
	check'(fn () => (toInt (quot(fromInt i, fromInt d)) = q 
			 andalso toInt (rem(fromInt i, fromInt d)) = r));

    fun divmod2 (i, d, q, r)  = 
	check'(fn () => let val (q', r') = divMod(fromInt i, fromInt d) 
			in toInt q' = q andalso toInt r' = r end);
    fun quotrem2 (i, d, q, r) = 
	check'(fn () => let val (q', r') = quotRem(fromInt i, fromInt d) 
			in toInt q' = q andalso toInt r' = r end);

    fun add1(x, y, sum) = 
	check'(fn () => (toInt (fromInt x + fromInt y) = sum));

    fun sub1(x, y, diff) = 
	check'(fn () => (toInt (fromInt x - fromInt y) = diff));

    fun mul1(x, y, prod) = 
	check'(fn () => (toInt (fromInt x * fromInt y) = prod));
	
in	

val test1a = divmod1(10, 3, 3, 1);
val test1b = divmod1(~10, 3, ~4, 2);
val test1c = divmod1(~10, ~3, 3, ~1);
val test1d = divmod1(10, ~3, ~4, ~2);

val test2a = quotrem1(10, 3, 3, 1);
val test2b = quotrem1(~10, 3, ~3, ~1);
val test2c = quotrem1(~10, ~3, 3, ~1);
val test2d = quotrem1(10, ~3, ~3, 1);

val test3a = divmod2(10, 3, 3, 1);
val test3b = divmod2(~10, 3, ~4, 2);
val test3c = divmod2(~10, ~3, 3, ~1);
val test3d = divmod2(10, ~3, ~4, ~2);

val test4a = quotrem2(10, 3, 3, 1);
val test4b = quotrem2(~10, 3, ~3, ~1);
val test4c = quotrem2(~10, ~3, 3, ~1);
val test4d = quotrem2(10, ~3, ~3, 1);

val test5a = ((fromInt 1 div fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val test5b = ((fromInt 1 mod fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val test5c = (quot(fromInt 1, fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val test5d = (rem(fromInt 1, fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val test5e = (divMod(fromInt 1, fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";
val test5f = (quotRem(fromInt 1, fromInt 0) seq "WRONG")
             handle Div => "OK" | _ => "WRONG";

val test6a = 
    List.map add1 [(12,17,29), (~12,17,5), (12,~17,~5), (~12,~17,~29)];
val test6b = 
    List.map sub1 [(12,17,~5), (~12,17,~29), (12,~17,29), (~12,~17,5)];
val test6c = 
    List.map mul1 [(12,17,204), (~12,17,~204), (12,~17,~204), (~12,~17,204)];

fun chkToString (i, s) = check'(fn _ => toString(fromInt i) = s);

val test12a = 
    List.map chkToString [(0, "0"), 
			  (~1, "~1"), 
			  (12345678, "12345678"),
			  (~12345678, "~12345678")];

fun chk f (s, r) = 
    check'(fn _ => 
	   case f s of
	       SOME res => toInt res = r
	     | NONE     => false)

fun chkScan fmt = chk (StringCvt.scanString (scan fmt))

val test13a = 
    List.map (chk fromString)
             [("10789", 10789),
	      ("+10789", 10789),
	      ("~10789", ~10789),
	      ("-10789", ~10789),
	      (" \n\t10789crap", 10789),
	      (" \n\t+10789crap", 10789),
	      (" \n\t~10789crap", ~10789),
	      (" \n\t-10789crap", ~10789)];

val test13b = 
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1", "ff"];	    

val test14a = 
    List.map (chkScan StringCvt.DEC)
             [("10789", 10789),
	      ("+10789", 10789),
	      ("~10789", ~10789),
	      ("-10789", ~10789),
	      (" \n\t10789crap", 10789),
	      (" \n\t+10789crap", 10789),
	      (" \n\t~10789crap", ~10789),
	      (" \n\t-10789crap", ~10789)];

val test14b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.DEC) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1", "ff"];	    

val test15a = 
    List.map (chkScan StringCvt.BIN)
             [("10010", 18),
	      ("+10010", 18),
	      ("~10010", ~18),
	      ("-10010", ~18),
	      (" \n\t10010crap", 18),
	      (" \n\t+10010crap", 18),
	      (" \n\t~10010crap", ~18),
	      (" \n\t-10010crap", ~18)];

val test15b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.BIN) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1", "2", "8", "ff"];

val test16a = 
    List.map (chkScan StringCvt.OCT)
             [("2071", 1081),
	      ("+2071", 1081),
	      ("~2071", ~1081),
	      ("-2071", ~1081),
	      (" \n\t2071crap", 1081),
	      (" \n\t+2071crap", 1081),
	      (" \n\t~2071crap", ~1081),
	      (" \n\t-2071crap", ~1081)];

val test16b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.OCT) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1", "8", "ff"];

val test17a = 
    List.map (chkScan StringCvt.HEX)
             [("20Af", 8367),
	      ("+20Af", 8367),
	      ("~20Af", ~8367),
	      ("-20Af", ~8367),
	      (" \n\t20AfGrap", 8367),
	      (" \n\t+20AfGrap", 8367),
	      (" \n\t~20AfGrap", ~8367),
	      (" \n\t-20AfGrap", ~8367)];

val test17b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.HEX) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1"];

val test18 =
    check'(fn _ => 
	   toInt(pow(fromInt 12, 3)) = 1728
	   andalso toInt(pow(fromInt  0 ,  1)) = 0
	   andalso toInt(pow(fromInt  1 ,  0)) = 1
	   andalso toInt(pow(fromInt  0 ,  0)) = 1
	   andalso toInt(pow(fromInt  1 , ~1)) = 1
	   andalso toInt(pow(fromInt ~1 , ~1)) = ~1
	   andalso toInt(pow(fromInt  2 , ~1)) = 0
	   andalso toInt(pow(fromInt ~2 , ~1)) = 0)
end;

val _ = quit();


