(* test/list.sml 
   PS 1994-12-10 *)

use "auxil.sml";

local 
    open List
in
val v123 = [1,2,3];
fun even i = i mod 2 = 0;

val test1 = check (null [] andalso not (null [[]]));

val test2 = check (1 = hd v123 andalso [2,3] = tl v123 andalso 3 = last v123);

val test3 = (hd []   seq "WRONG") handle Empty => "OK" | _ => "WRONG";
val test4 = (tl []   seq "WRONG") handle Empty => "OK" | _ => "WRONG";
val test5 = (last [] seq "WRONG") handle Empty => "OK" | _ => "WRONG";

val test6 = check(1 = nth(v123,0) andalso 3 = nth(v123,2));

val test7 = (nth(v123,~1) seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test8 = (nth(v123,3)  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";

val test9 = check (3 = length v123);

val test10 = check ([3,2,1] = rev [1,2,3]);

val v16 = v123 @ [4,5,6];

val test11 = check([1,2,3,4,5,6] = v16);

val test12 = check(concat [] = [] andalso concat [v16] = v16 
		   andalso concat [v123, [4,5,6]] = v16);

val test13 = check(rev v16 = revAppend([4,5,6], [3,2,1]));

local 
    val v = ref 0
    fun h [] r = r | h (x::xr) r = h xr (r+r+x): int;
    val isum = h v16 0
in 
    fun reset () = v := 0;
    fun incrv i = v := 2 * !v + i;
    fun checkv () = check(!v = isum);
end;

val test14 = (reset (); app incrv v16; checkv);

val test15 = check([2,4,6,8,10,12] = map (fn i=>i*2) v16);

val test16 = 
    check([3,9,15] = 
	  mapPartial (fn i => if even i then NONE else SOME (3*i)) v16);

val test17 = check(NONE = find (fn i => i>7) v16);

val test18 = check(SOME 5 = find (fn i => i>4) v16);

val test19 = check(NONE = find (fn _ => true) []);

val test20 = check([2,4,6] = filter even v16);

val test21 = (reset (); filter (fn i => (incrv i; true)) v16 seq checkv());

val test22 = check(([2,4,6], [1,3,5]) = partition even v16);

val test23 = (reset (); partition (fn i => (incrv i; true)) v16 seq checkv());

val test24 = check(v16 = foldr op:: [] v16);
val test25 = check(rev v16 = foldl op:: [] v16);

val test26 = (reset(); foldr (fn (i,r) => incrv i) () (rev v16); checkv());
val test27 = (reset(); foldl (fn (i,r) => incrv i) () v16; checkv());

val test28 = check(21 = foldr op+ 0 v16 andalso 21 = foldl op+ 0 v16);

val test29 = check(all (fn _ => false) [] 
		   andalso not (exists (fn _ => true) [])); 

val test30 = check(exists even [1,1,1,1,1,1,2,1] 
		   andalso all even [6,6,6,6,6,6,6,6]);

val test31 = check(v16 = tabulate (6, fn i => i+1));

val test32 = (reset(); tabulate (6, fn i => (incrv (i+1); 127)) seq checkv());

val test33 = check([] = tabulate (0, fn i => 1 div i));

val test34 = (tabulate(~1, fn _ => raise Div) seq "WRONG") 
             handle Size => "OK" | _ => "WRONG";

val test35a = check(drop([], 0) = [] 
		   andalso drop(v123, 0) = v123 
		   andalso drop(v123, 3) = []);
val test35b = (drop(v123, ~1) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test35c = (drop(v123, 4) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";

val test36a = check(take([], 0) = [] 
		   andalso take(v123, 3) = v123
		   andalso take(v123, 0) = []);
val test36b = (take(v123, ~1) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test36c = (take(v123, 4) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";

val test37a = 
    check'(fn _ => getItem [] = NONE
	   andalso getItem [#"A"] = SOME(#"A", [])
	   andalso getItem [#"B", #"C"] = SOME(#"B", [#"C"]));
end;
