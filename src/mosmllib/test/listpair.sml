(* test/listpair.sml
   PS 1995-02-25, 1997-03-07, 2000-10-19
*)

use "auxil.sml";

local
    open ListPair
    val a = [1, 2, 3, 4, 5, 6]
    val b = [10, 40, 50, 50]
    val b6 = [10, 40, 50, 50, 60, 70]
    val ab = [(1, 10), (2, 40), (3, 50), (4, 50)]
    val ab6 = [(1, 10), (2, 40), (3, 50), (4, 50), (5, 60), (6, 70)]
    fun take 0 xs        = []
      | take n []        = []
      | take n (x :: xr) = x :: take (n-1) xr
in

val test1 = check(zip([], []) = [] 
		  andalso zip ([], a) = [] 
		  andalso zip(a, []) = []
		  andalso zip(a, b) = ab
		  andalso zip(b, a) = List.map (fn (x,y) => (y,x)) ab);

val test2a = check(([], []) = unzip []
		   andalso (a, a) = unzip(zip(a,a))
		   andalso (take (length b) a, b) = unzip(zip(a, b))
		   andalso (b, take (length b) a) = unzip(zip(b, a)));
val test2b = check(ab = zip(unzip ab));

local 
    val v = ref 0
    fun h [] r = r | h (x::xr) r = h xr (r+r+x): int;
    val isum = h (take (length b) a) 0
    val isum6 = h (take (length b6) a) 0
in 
    fun reset () = v := 0;
    fun incrv i = v := 2 * !v + i;
    fun checkv () = check(!v = isum);
    fun checkv6 () = check(!v = isum6);
    fun checkv0 () = check(!v = 0);
end;

val test3a = check(map (fn (x, y) => x + y) (a, b) = 
		  List.map (fn (x,y) => x + y) (zip(a, b)));

val test3b = (reset (); map (incrv o #1) (a, b) seq (); checkv());

val test4 = (reset (); app (incrv o #1) (a, b); checkv());

val test5a = check(all (fn _ => false) (a, [])
		   andalso not (exists (fn _ => true) ([], b))); 

val test5b = check(exists (fn (x, y) => x = 3) (a, b) 
		   andalso all (fn (x, y) => y <= 50) (a, b));

val test5c = check(not (exists (fn (x, y) => x = 5) (a, b))
		   andalso not (exists (fn (x, y) => y = 5) (b, a))
		   andalso all (fn (x, y) => x <> 6) (a, b)
		   andalso all (fn (x, y) => y <> 6) (b, a));

val test5d = (reset(); all (fn (x,y) => (incrv x; true)) (a, b) seq (); 
	      checkv());
val test5e = (reset(); exists (fn (x,y) => (incrv x; false)) (a, b) seq (); 
	      checkv());

local 
    fun foldrchk f e xs ys = 
	foldr f e (xs, ys) = 
	List.foldr (fn ((x, y), r) => f(x, y, r)) e (zip(xs, ys))
    fun foldlchk f e xs ys = 
	foldl f e (xs, ys) = 
	List.foldl (fn ((x, y), r) => f(x, y, r)) e (zip(xs, ys))
in
val test6 = check'(fn _ => 
    foldrchk (fn (x, y, (r1, r2)) => (x-r1, y div r2)) (0, 10) a b
    andalso foldrchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) [] b
    andalso foldrchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) a  []
    andalso foldrchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) [] []);

val test7 = check'(fn _ => 
    foldlchk (fn (x, y, (r1, r2)) => (x-r1, y div r2)) (0, 10) a b
    andalso foldlchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) [] b
    andalso foldlchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) a  []
    andalso foldlchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) [] []);
end

val test8a = (zipEq (a, b); "WRONG")
             handle UnequalLengths => "OK" | _ => "WRONG";
val test8b = (zipEq (b, a); "WRONG")
             handle UnequalLengths => "OK" | _ => "WRONG";

val test8b = 
    check'(fn _ => zipEq (a, b6) = ab6
	   andalso zipEq (b6, a) = List.map (fn (x,y) => (y,x)) ab6);

val test9a = (mapEq op- (a, b); "WRONG")
             handle UnequalLengths => "OK" | _ => "WRONG";
val test9b = (mapEq op- (b, a); "WRONG")
             handle UnequalLengths => "OK" | _ => "WRONG";
val test9c = 
    check'(fn _ => 
	   mapEq op- (a, b6) = List.map op- (zipEq(a, b6))
	   andalso mapEq op- (b6, a) = List.map op- (zipEq(b6, a))
	   andalso mapEq op- (a, b6) = List.map op- (zip(a, b6))
	   andalso mapEq op- (b6, a) = List.map op- (zip(b6, a)))
val test9d = (reset (); mapEq (incrv o #1) (a, b6) seq (); checkv6());
val test9e = (reset (); 
	      (mapEq (incrv o #1) (a, b); "WRONG")
	      handle UnequalLengths => checkv() | _ => "WRONG");

val test10a = (appEq ignore (a, b); "WRONG")
              handle UnequalLengths => "OK" | _ => "WRONG";
val test10b = (appEq ignore (b, a); "WRONG")
              handle UnequalLengths => "OK" | _ => "WRONG";
val test10c =
    (reset (); 
     (appEq (incrv o #1) (a, b); "WRONG")
     handle UnequalLengths => checkv() | _ => "WRONG");	 
val test10d =
    (reset (); appEq (incrv o #1) (a, b6); checkv6());

val test11a = 
    check'(fn _ =>
	   allEq (fn _ => false) ([], [])
	   andalso not (allEq (fn _ => true) (a, []))
	   andalso not (allEq (fn _ => true) ([], a))
	   andalso allEq (fn _ => true) (a, b6)
	   andalso allEq (fn _ => true) (b6, a)
	   andalso not (allEq (fn (x, y) => x <> 77) (a, b))
	   andalso allEq (fn (x, y) => x <> 77) (a, b6)
	   andalso allEq (fn (x, y) => x <> 77) (b6, a));
val test11b = 
    (reset(); 
     allEq (fn (x,y) => (incrv x; true)) (a, b);
     checkv());
val test11c = 
    (reset(); 
     allEq (fn (x,y) => (incrv x; true)) (a, b6); 
     checkv6());

local 
    fun foldrEqchk f e xs ys = 
	foldrEq f e (xs, ys) = 
	List.foldr (fn ((x, y), r) => f(x, y, r)) e (zipEq(xs, ys))
    fun foldlEqchk f e xs ys = 
	foldlEq f e (xs, ys) = 
	List.foldl (fn ((x, y), r) => f(x, y, r)) e (zipEq(xs, ys))
in
val test12a = check'(fn _ => 
    foldrEqchk (fn (x, y, (r1, r2)) => (x-r1, y div r2)) (0, 10) a b6
    andalso 
    foldrEqchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (2, 3) b6 a
    andalso 
    foldrEqchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) [] []);
val test12b = 
    (reset();
     foldrEq (fn (x, _, _) => incrv x) () (rev a, b6); 
     checkv6());

val test13a = check'(fn _ => 
    foldlEqchk (fn (x, y, (r1, r2)) => (x-r1, y div r2)) (0, 10) a b6
    andalso 
    foldlEqchk (fn (x, y, (r1, r2)) => (x div r1, y-r2)) (10, 0) b6 a
    andalso 
    foldlEqchk (fn (x, y, (r1, r2)) => (x div r1, y div r2)) (0, 0) [] []);
val test13b = 
    (reset();
     (foldlEq (fn (x, _, _) => incrv x) () (a, b); "WRONG") 
     handle UnequalLengths => "OK" | _ => "WRONG";
     checkv());
val test13c = 
    (reset();
     foldlEq (fn (x, _, _) => incrv x) () (a, b6); 
     checkv6());
end
end;

