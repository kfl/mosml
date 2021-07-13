(* test/int64.sml -- some test cases for Int64, appropriate for a two's
   complement machine.

   Based on int.sml by PS and with some inspiration from int64.sml from MLKit by MAEL
   2021-07-13 KFL
 *)

use "auxil.sml";
load "Int64";

local 
    open Int64
    infix 7 quot rem
    infix ==
    fun divmod (ii, dd, qq, rr)  =
        check let val (i, d, q, r) = (fromInt ii, fromInt dd, fromInt qq, fromInt rr)
              in  i div d == q andalso i mod d == r end;
    fun quotrem (ii, dd, qq, rr) =
        check let val (i, d, q, r) = (fromInt ii, fromInt dd, fromInt qq, fromInt rr)
              in  i quot d == q andalso i rem d == r end;

    val i64 = fromInt
in

val test0a = checkrange (~1025, 1025)
                        (fn i => i = toInt(fromInt i));
val test0b = checkrange (~1025, 1025)
                        (fn k => toInt (i64 k div i64 17) = Int.div(k, 17));

val test1a = divmod(10, 3, 3, 1);
val test1b = divmod(~10, 3, ~4, 2);
val test1c = divmod(~10, ~3, 3, ~1);
val test1d = divmod(10, ~3, ~4, ~2);

val test2a = quotrem(10, 3, 3, 1);
val test2b = quotrem(~10, 3, ~3, ~1);
val test2c = quotrem(~10, ~3, 3, ~1);
val test2d = quotrem(10, ~3, ~3, 1);

val test3 = check (max(fromInt ~5, fromInt 2) == fromInt 2
                   andalso max(fromInt 5, fromInt 2) == fromInt 5);
val test4 = check (min(fromInt ~5, fromInt 3) == fromInt ~5
                   andalso min(fromInt 5, fromInt 2) == fromInt 2);
val test5 = check (sign (fromInt ~57) = ~1
                   andalso sign (fromInt 99) = 1
                   andalso sign (fromInt 0) = 0);
val test6 = check (sameSign(fromInt ~255, ~(fromInt 256))
                   andalso sameSign(fromInt 255, fromInt 256)
		           andalso sameSign(fromInt 0, fromInt 0));

val test7 =
    List.map (fn f => check' (fn _ => let val (x, y) = f() in x == y end))
             [ fn _ => (i64 ~42             , ~(i64 42))
             , fn _ => (abs(i64 ~42)        , i64 42)
             , fn _ => (abs(i64 42)         , i64 42)
             , fn _ => (~(~(i64 42))        , i64 42)
             , fn _ => (i64 21 + i64 21     , i64 42)
             , fn _ => (i64 2 * i64 21      , i64 42)
             , fn _ => (i64 ~21 + i64 ~21   , i64 ~42)
             , fn _ => (i64 ~42 - i64 42    , i64 ~84)
             , fn _ => (valOf maxInt + i64 0   , valOf maxInt)
             , fn _ => (valOf minInt + i64 0   , valOf minInt)
             , fn _ => (valOf maxInt - i64 0   , valOf maxInt)
             , fn _ => (valOf minInt - i64 0   , valOf minInt)
             , fn _ => (valOf maxInt * i64 1   , valOf maxInt)
             , fn _ => (valOf minInt * i64 1   , valOf minInt)
             , fn _ => (valOf minInt div i64 1 , valOf minInt)
             , fn _ => (valOf minInt quot i64 1, valOf minInt)
             , fn _ => (valOf minInt mod i64 2 , i64 0)
             , fn _ => (valOf minInt rem i64 2 , i64 0)
             , fn _ => (valOf maxInt mod i64 2 , i64 1)
             , fn _ => (valOf maxInt rem i64 2 , i64 1)
             ];

val test8 =
    List.map (fn f => check ((f(); false) handle Overflow => true))
             [ fn _ => valOf maxInt + i64 1
             , fn _ => valOf minInt - i64 1
             , fn _ => valOf maxInt * i64 2
             , fn _ => i64 4611686018427387903 * i64 2 * i64 2
             , fn _ => ~(valOf minInt)
             , fn _ => valOf minInt div i64 ~1
             , fn _ => valOf minInt quot i64 ~1
             , fn _ => valOf minInt + valOf minInt
             ];

val test12 =
    (case (minInt, maxInt) of
		 (SOME mi, SOME ma) => check(sign mi = ~1
                                     andalso sign ma = 1
						             andalso sameSign(mi, fromInt ~1)
                                     andalso sameSign(ma, fromInt 1))
	   | (NONE, NONE)       => "OK"
	   | _                  => "WRONG");


fun chk f (s, r) =
    check'(fn _ =>
	          case f s of
	              SOME res => res == fromInt r
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
	    case fromString(toString(fromInt i)) of
            SOME n => fromInt i == n
          | NONE => false;

    fun scanFmt radix i =
        let val i = fromInt i
	        val s = fmt radix i
	    in case StringCvt.scanString (scan radix) s of
               SOME n => n == i
             | NONE => false end;

    fun scanFmt2 radix i =
        let val s = fmt radix i
	    in case StringCvt.scanString (scan radix) s of
               SOME n => n == i
             | NONE => false end;

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

val test23a = check'(fn _ => scanFmt2 StringCvt.HEX (valOf Int64.maxInt));
val test23b = check'(fn _ => scanFmt2 StringCvt.DEC (valOf Int64.maxInt));
val test23c = check'(fn _ => scanFmt2 StringCvt.OCT (valOf Int64.maxInt));
val test23d = check'(fn _ => scanFmt2 StringCvt.BIN (valOf Int64.maxInt));

val test24a = check'(fn _ => scanFmt2 StringCvt.HEX (valOf Int64.minInt));
val test24b = check'(fn _ => scanFmt2 StringCvt.DEC (valOf Int64.minInt));
val test24c = check'(fn _ => scanFmt2 StringCvt.OCT (valOf Int64.minInt));
val test24d = check'(fn _ => scanFmt2 StringCvt.BIN (valOf Int64.minInt));

val test25a = check'(fn _ => scanFmt2 StringCvt.HEX (valOf Int64.minInt + fromInt 10));
val test25b = check'(fn _ => scanFmt2 StringCvt.DEC (valOf Int64.minInt + fromInt 10));
val test25c = check'(fn _ => scanFmt2 StringCvt.OCT (valOf Int64.minInt + fromInt 10));
val test25d = check'(fn _ => scanFmt2 StringCvt.BIN (valOf Int64.minInt + fromInt 10));

fun chk' f s =
    check' (fn _ => ((f s; false) handle Overflow => true))
fun chkScanOvf fmt = chk' (StringCvt.scanString (scan fmt))

val test26a = chkScanOvf StringCvt.HEX "~8000000000000001"
val test26b = chkScanOvf StringCvt.DEC "~9223372036854775809"
val test26c = chkScanOvf StringCvt.OCT "~1000000000000000000001"
val test26d = chkScanOvf StringCvt.BIN "~1000000000000000000000000000000000000000000000000000000000000001"

val test27a = chkScanOvf StringCvt.HEX "10000000000000000"
val test27b = chkScanOvf StringCvt.DEC "9223372036854775808"
val test27c = chkScanOvf StringCvt.OCT "1000000000000000000000"
val test27d = chkScanOvf StringCvt.BIN "1000000000000000000000000000000000000000000000000000000000000000"

val test28a = check'(fn () => toString (valOf maxInt) = "9223372036854775807")
val test28b = check'(fn () => toString (valOf minInt) = "~9223372036854775808")

val test29a = check'(fn () => case fromString "9223372036854775807" of
                                           SOME n => n == valOf maxInt
                                         | _ => false)
val test29b = check'(fn () => case fromString "-9223372036854775808" of
                                           SOME n => n == valOf minInt
                                         | _ => false)

end

end
