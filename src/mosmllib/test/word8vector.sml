(* test/vector.sml -- some test cases for Vector 
   PS 1994-12-10, 1995-06-14 *)

use "auxil.sml";

local
    open Word8Vector;
    val i2w = Word8.fromInt;
    infix 9 sub;
in

val a = fromList (List.map i2w [0,1,2,3,4,5,6]);
val b = fromList (List.map i2w [44,55,66]);
val c = fromList (List.map i2w [0,1,2,3,4,5,6]);

val test1 = check'(fn _ => a<>b);
val test2 = check'(fn _ => a=c);

val d = tabulate(100, fn i => i2w (i mod 7));

val test3 = check'(fn _ => d sub 27 = i2w 6);

val test4a = (tabulate(maxLen+1, i2w) seq "WRONG")
             handle Size => "OK" | _ => "WRONG";

val test4b = (tabulate(~1, i2w)       seq "WRONG")
             handle Size => "OK" | _ => "WRONG";

val test4c = check'(fn _ => length (tabulate(0, fn i => i2w (i div 0))) = 0);

val test5 = check'(fn _ => length (fromList []) = 0 andalso length a = 7);

val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6c = check'(fn _ => c sub 0 = i2w 0);

val e = concat [d, b, d];

val test7 = check'(fn _ => length e = 203);

val test8 = check'(fn _ => length (concat []) = 0);

val f = extract (e, 100, SOME 3);

val test9 = check'(fn _ => f = b);

val test9a = check'(fn _ => e = extract(e, 0, SOME (length e)) 
		    andalso e = extract(e, 0, NONE));
val test9b = check'(fn _ => fromList [] = extract(e, 100, SOME 0));
val test9c = (extract(e, ~1, SOME (length e))  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9d = (extract(e, length e + 1, SOME 0)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9e = (extract(e, 0, SOME (length e+1)) seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9f = (extract(e, 20, SOME ~1)        seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9g = (extract(e, ~1, NONE)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9h = (extract(e, length e + 1, NONE)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9i = check'(fn _ => fromList [] = extract(e, length e, SOME 0)
		    andalso fromList [] = extract(e, length e, NONE));

fun chkiter iter f vec reslast =
    check'(fn _ =>
	   let val last = ref 0w255
	       val res = iter (fn x => (last := x; f x)) vec
	   in (res, !last) = reslast end)

fun chkiteri iter f vec reslast =
    check'(fn _ =>
	   let val last = ref ~1
	       val res = iter (fn (i, x) => (last := i; f x)) vec
	   in (res, !last) = reslast end)

val test10a = 
    chkiter map (fn x => 0w2*x) b (fromList [0w88,0w110,0w132], 0w66)

val test11a = 
    chkiteri mapi (fn x => 0w2*x) (b, 0, NONE) (fromList [0w88,0w110,0w132], 2)
val test11b = 
    chkiteri mapi (fn x => 0w2*x) (b, 1, NONE) (fromList [0w110,0w132], 2)
val test11c = 
    chkiteri mapi (fn x => 0w2*x) (b, 1, SOME 0) (fromList [], ~1)
val test11d = 
    chkiteri mapi (fn x => 0w2*x) (b, 1, SOME 1) (fromList [0w110], 1)
val test11e = 
    chkiteri mapi (fn x => 0w2*x) (b, 3, NONE) (fromList [], ~1)

val test11f =
    (mapi #2 (b, 0, SOME 4) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val test11g =
    (mapi #2 (b, 3, SOME 1) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val test11h =
    (mapi #2 (b, 4, SOME 0) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val test11i =
    (mapi #2 (b, 4, NONE) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
end;
