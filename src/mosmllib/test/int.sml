(* test/int.sml -- here we test only the `exotic' operations
   PS 1995-02-25, 1996-07-02 *)

use "auxil.sml";

local 
    open Int
    infix 7 quot rem
    fun divmod (i, d, q, r)  = check(i div d = q andalso i mod d = r);
    fun quotrem (i, d, q, r) = check(i quot d = q andalso i rem d = r);
in	

val test1a = divmod(10, 3, 3, 1);
val test1b = divmod(~10, 3, ~4, 2);
val test1c = divmod(~10, ~3, 3, ~1);
val test1d = divmod(10, ~3, ~4, ~2);

val test2a = quotrem(10, 3, 3, 1);
val test2b = quotrem(~10, 3, ~3, ~1);
val test2c = quotrem(~10, ~3, 3, ~1);
val test2d = quotrem(10, ~3, ~3, 1);

val test3 = check(max(~5, 2) =  2 andalso max(5, 2) = 5);
val test4 = check(min(~5, 3) = ~5 andalso min(5, 2) = 2);

val test5 = check(sign ~57 = ~1 andalso sign 99 = 1 andalso sign 0 = 0);
val test6 = check(sameSign(~255, ~256) andalso sameSign(255, 256) 
		  andalso sameSign(0, 0));

val test12 = 
    case (minInt, maxInt) of
	(SOME mi, SOME ma) =>
	    check(sign mi = ~1 andalso sign ma = 1 
		  andalso sameSign(mi, ~1) andalso sameSign(ma, 1))
      | (NONE, NONE)       => "OK"
      | _                  => "WRONG";

fun chk f (s, r) = 
    check'(fn _ => 
	   case f s of
	       SOME res => res = r
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
	      (" \n\t-10789crap", ~10789),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x123", 0),
	      ("0X123", 0),
	      ("0wx123", 0),
	      ("0wX123", 0)];

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
	      (" \n\t-10789crap", ~10789),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x123", 0),
	      ("0X123", 0),
	      ("0wx123", 0),
	      ("0wX123", 0)];

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
	      (" \n\t-10010crap", ~18),
	      ("0w101", 0),
	      ("0W101", 0),
	      ("0x101", 0),
	      ("0X101", 0),
	      ("0wx101", 0),
	      ("0wX101", 0)];

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
	      (" \n\t-2071crap", ~1081),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x123", 0),
	      ("0X123", 0),
	      ("0wx123", 0),
	      ("0wX123", 0)];

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
	      (" \n\t-20AfGrap", ~8367),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x", 0),
	      ("0x ", 0),
	      ("0xG", 0),
	      ("0X", 0),
	      ("0XG", 0),
	      ("0x123", 291),
	      ("0X123", 291),
	      ("-0x123", ~291),
	      ("-0X123", ~291),
	      ("~0x123", ~291),
	      ("~0X123", ~291),
	      ("+0x123", 291),
	      ("+0X123", 291),
	      ("0wx123", 0),
	      ("0wX123", 0)];

val test17b = 
    List.map (fn s => case StringCvt.scanString (scan StringCvt.HEX) s 
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+", 
	    "+ 1", "~ 1", "- 1"];


local 
    fun fromToString i = 
	fromString (toString i) = SOME i;

    fun scanFmt radix i = 
	StringCvt.scanString (scan radix) (fmt radix i) = SOME i;

in
val test18 = 
    check'(fn _ => range (~1200, 1200) fromToString);

val test19 = 
    check'(fn _ => range (~1200, 1200) (scanFmt StringCvt.BIN));

val test20 = 
    check'(fn _ => range (~1200, 1200) (scanFmt StringCvt.OCT));

val test21 = 
    check'(fn _ => range (~1200, 1200) (scanFmt StringCvt.DEC));

val test22 = 
    check'(fn _ => range (~1200, 1200) (scanFmt StringCvt.HEX));
end

end
