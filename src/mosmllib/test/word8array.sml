(* test/word8array.sml -- some test cases for Word8Array 
   PS 1994-12-21, 1995-05-11, 2000-10-24 *)

use "auxil.sml";

local
    open Word8Array 
    infix 9 sub;
    val array0 = fromList [];
in

val i2w = Word8.fromInt;

val w127 = i2w 127;

val a = fromList (map i2w [0,1,2,3,4,5,6]);
val b = fromList (map i2w [44,55,66]);
val c = fromList (map i2w [0,1,2,3,4,5,6]);

val test1 = 
    check'(fn () => a<>c);
val test2 = 
    check'(fn () => 
	   array(0, w127) <> array0
	   andalso array(0, w127) <> tabulate(0, fn _ => w127)
	   andalso tabulate(0, fn _ => w127) <> fromList []
	   andalso array(0, w127) <> array(0, w127)
	   andalso tabulate(0, fn _ => w127) <> tabulate(0, fn _ => w127)
	   andalso fromList [] <> fromList []);

val d = tabulate(100, fn i => i2w (i mod 7));

val test3 = check' (fn () => d sub 27 = i2w 6);

val test4a = (tabulate(maxLen+1, i2w) seq "WRONG")
            handle Size => "OK" | _ => "WRONG";

val test4b = (tabulate(~1, i2w)       seq "WRONG")
            handle Size => "OK" | _ => "WRONG";

val test4c = 
    check'(fn () => length (tabulate(0, fn i => i2w (i div 0))) = 0);

val test5a = check'(fn () => length (fromList []) = 0 andalso length a = 7);
val test5b = check'(fn () => length array0 = 0);

val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6c = check'(fn () => c sub 0 = i2w 0);

val e = array(203, i2w 0);
val _ = (copy{src=d, dst=e, di=0}; 
	 copy{src=b, dst=e, di=length d};
	 copy{src=d, dst=e, di=length d + length b});
	 
fun a2v a = vector a;
val ev = Word8Vector.concat [a2v d, a2v b, a2v d];

val test7 = check'(fn () => length e = 203);

val test8a = (update(e, ~1, w127); "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test8b = (update(e, length e, w127); "WRONG")
             handle Subscript => "OK" | _ => "WRONG";

val f = Word8ArraySlice.vector(Word8ArraySlice.slice(e, 100, SOME 3));

val test9 = check'(fn () => f = a2v b);

val test9a = 
    check'(fn () => ev = vector e);
val test9b = 
    check'(fn () => Word8Vector.fromList [] = vector array0);


val _ = copy{src=e, dst=e, di=0};
val g = array(203, w127);
val _ = copy{src=e, dst=g, di=0};

val test10a = check'(fn () => ev = vector g);

val test10b = 
    check'(fn () => (copy{src=array0, dst=array0, di=0}; 
		     array0 <> array(0, 0w99)));
val test10c = 
    check'(fn () => (copy{src=array0, dst=g, di=0}; 
		     ev = vector g));
val test10d = 
    check'(fn () => (copy{src=array0, dst=g, di=203}; 
		     ev = vector g));
val test10e = 
    check'(fn () => (copy{src=array0, dst=g, di=1}; 
		     ev = vector g));

val test11a = (copy{src=g, dst=g, di=1}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11b = (copy{src=g, dst=g, di= 202}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11c = (copy{src=b, dst=g, di = ~1}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11d = (copy{src=b, dst=g, di=203}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11e = check'(fn () => ev = vector g);

val test12 = 
    check'(fn _ =>
	   let fun invcompare (c1, c2) = Word8.compare(c2, c1)
	       val fromString = 
		   fromList o List.map (Word8.fromInt o ord) o explode 
	       fun coll s1 s2 = 
		   collate invcompare (fromString s1, fromString s2)
	   in 
	       coll "" "" = EQUAL
	       andalso coll "" " " = LESS
	       andalso coll " " "" = GREATER
	       andalso coll "ABCD" "ABCD" = EQUAL
	       andalso coll "ABCD" "ABCD " = LESS
	       andalso coll "ABCD " "ABCD" = GREATER
	       andalso coll "B" "ABCD" = LESS
	       andalso coll "ABCD" "B" = GREATER
	       andalso coll "CCCB" "CCCABCD" = LESS
	       andalso coll "CCCABCD" "CCCB" = GREATER
	       andalso coll "CCCB" "CCCA" = LESS
	       andalso coll "CCCA" "CCCB" = GREATER
	   end)

val test13 = 
    check'(fn _ => 
	   NONE = find (fn i => i > 0w7) a
	   andalso SOME 0w5 = find (fn i => i > 0w4) a
	   andalso NONE = find (fn _ => true) (fromList []));

val test14 = 
    check'(fn _ => 
	   not (exists (fn i => i > 0w7) a)
	   andalso exists (fn i => i > 0w4) a
	   andalso not (exists (fn _ => true) (fromList [])));

val test15 = 
    check'(fn _ => 
	   not (all (fn i => i < 0w6) a)
	   andalso all (fn i => i < 0w7) a
	   andalso all (fn _ => false) (fromList []));
end;
