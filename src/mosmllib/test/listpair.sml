(* test/listpair.sml
   PS 1995-02-25, 1997-03-07
*)

use "auxil.sml";

local 
    open ListPair
    val a = [1, 2, 3, 4, 5, 6]
    val b = [10, 40, 50, 50]
    val ab = [(1, 10), (2, 40), (3, 50), (4, 50)]
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

val test3a = check(map (fn (x, y) => x + y) (a, b) = 
		  List.map (fn (x,y) => x + y) (zip(a, b)));

local 
    val v = ref 0
    fun h [] r = r | h (x::xr) r = h xr (r+r+x): int;
    val isum = h (take (length b) a) 0
in 
    fun reset () = v := 0;
    fun incrv i = v := 2 * !v + i;
    fun checkv () = check(!v = isum);
end;

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

end;

