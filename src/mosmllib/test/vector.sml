(* test/vector.sml -- some test cases for Vector 
   PS 1994-12-10, 1995-06-14, 1997-03-07, 2000-10-17 *)

use "auxil.sml";

local
    open Vector;
    infix 9 sub;
in

val a = fromList [0,1,2,3,4,5,6];
val b = fromList [44,55,66];
val c = fromList [0,1,2,3,4,5,6];
val vec0 = fromList [];

val test1 = check'(fn _ => a<>b);
val test2 = check'(fn _ => a=c);

val d = tabulate(100, fn i => i mod 7);

val test3 = check'(fn _ => d sub 27 = 6);

val test4a = (tabulate(maxLen+1, fn i => i) seq "WRONG")
             handle Size => "OK" | _ => "WRONG";

val test4b = (tabulate(~1, fn i => i)       seq "WRONG")
             handle Size => "OK" | _ => "WRONG";

val test4c = check'(fn _ => length (tabulate(0, fn i => i div 0)) = 0);

val test5 = check'(fn _ => length (fromList []) = 0 andalso length a = 7);

val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6c = check'(fn _ => c sub 0 = 0);

val e = concat [d, b, d];

val test7 = check'(fn _ => length e = 203);

val test8 = check'(fn _ => length (concat []) = 0);

val f = VectorSlice.vector(VectorSlice.slice(e, 100, SOME 3));

val test9 = check'(fn _ => f = b);

fun chkiter iter f vec reslast =
    check'(fn _ =>
	   let val last = ref ~1
	       val res = iter (fn x => (last := x; f x)) vec
	   in (res, !last) = reslast end)

fun chkiteri iter f vec reslast =
    check'(fn _ =>
	   let val last = ref ~1
	       val res = iter (fn (i, x) => (last := i; f x)) vec
	   in (res, !last) = reslast end)

val test10a = 
    chkiter map (fn x => 2*x) b (fromList [88,110,132], 66)

val test11a = 
    chkiteri mapi (fn x => 2*x) b (fromList [88,110,132], 2)

val test12a = 
    check'(fn _ => 
	   a = update(a, 0, 0) 
	   andalso a = update(a, 6, 6)
	   andalso #[78,1,2,3,4,5,6] = update(a, 0, 78)
	   andalso #[0,1,2,333,4,5,6] = update(a, 3, 333))
val test12b =
    (update(b, ~1, 17) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val test12c =
    (update(b, 7, 17) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";
val test12d =
    (update(#[], 0, 17) seq "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";

val test13 = 
    check'(fn _ =>
	   let fun invcompare (c1, c2) = Char.compare (c2, c1) 
	       fun coll s1 s2 = 
		   collate invcompare (fromList (explode s1), 
				       fromList (explode s2))
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

val test14 = 
    check'(fn _ => 
	   NONE = find (fn i => i>7) a
	   andalso SOME 5 = find (fn i => i>4) a
	   andalso NONE = find (fn _ => true) #[]);

val test15 = 
    check'(fn _ => 
	   not (exists (fn i => i>7) a)
	   andalso exists (fn i => i>4) a
	   andalso not (exists (fn _ => true) #[]));

val test16 = 
    check'(fn _ => 
	   not (all (fn i => i<6) a)
	   andalso all (fn i => i<7) a
	   andalso all (fn _ => false) #[]);
end;

