(* test/rbset.sml 2001-10-28 *)

app load ["Rbset", "Random", "Listsort"];

use "auxil.sml";

local 
    open Rbset
    val s0 = empty Int.compare;
    val s1  = addList(empty Int.compare, [43,25,13,14]);
    val s2a = addList(empty Int.compare, [43,1,2,3]);
    val s2b = addList(empty Int.compare, [43,2,1,3,43,43]);
    val s3  = addList(empty Int.compare, [1,3]);
    val s4  = addList(empty Int.compare, [117]);

    val nonemptysets = [(s1, [13,14,25,43]),
			(s2a, [1,2,3,43]), 
			(s2b, [1,2,3,43]) ,
			(s3 , [1,3]),
			(s4 , [117])];

    (* Make enough small-range random int sets so that they will have
       duplicates and some will have a non-empty intersection *)

    open Random 
	    
    val rnd = newgenseed 188.0	(* Use this for debugging *)
    val rnd = newgen ()  

    fun mkrandomset maxsize = 
	let val size = range (0, maxsize) rnd
	    val ys = rangelist(0, 1000) (size, rnd)      (* all >= 0 *)
	    val xs = Listsort.sort Int.compare ys
	    fun dropdups last [] = []
	      | dropdups last (x1 :: xr) =
		if last = x1 then dropdups last xr
		else x1 :: dropdups x1 xr
	in 
	    (addList(empty Int.compare, ys), dropdups ~1 xs)
	end

    fun mkrandomsets maxsize count = 
	List.tabulate(count, fn _ => mkrandomset maxsize)

    val allsets = (s0, []) :: nonemptysets @ mkrandomsets 500 20;

    (* Merge without duplicates *)

    fun mergeUniq ordr ([],     ys) = ys
      | mergeUniq ordr (x1::xr, ys) = 
	let fun take x1 xr []       = x1 :: xr
	      | take x1 xr (y1::yr) = 
	        (case ordr(x1, y1) of 
		     LESS    => x1 :: take y1 yr xr
		   | GREATER => y1 :: take x1 xr yr
		   | EQUAL   => take x1 xr yr)
	in take x1 xr ys end

    fun swap (x, y) = (y, x)

    val minusOne = singleton Int.compare ~1;	 (* not in any set *)
    val oneMega = singleton Int.compare 1000000; (* not in any set *)

in
    (* equal *)

    val test1 = 
	check'(fn _ => 
	       equal(s0, s0)
	       andalso equal(s1, s1)
	       andalso equal(s2a, s2b)
	       andalso not (equal(s0, s1))
	       andalso not (equal(s1, s0))
	       andalso not (equal(s1, s2a))
	       andalso not (equal(s1, s2a))
	       andalso not (equal(s2a, s3))
	       andalso equal(s4, singleton Int.compare 117)
	       andalso equal(singleton Int.compare 117, 
			     singleton Int.compare 117));

    (* listItems, isEmpty, numItems, member *)

    fun chkmems(s, xs) =
	listItems s = xs
	andalso isEmpty s = (xs = [])
	andalso isEmpty s = (numItems s = 0)
	andalso List.all (fn x => member(s, x)) xs
	andalso List.all (fn x => numItems s = List.length xs) allsets
	andalso not (member(s, ~999999));


    val test2 = 
	check'(fn _ => List.all chkmems allsets);

    (* isSubset *)

    val test3 = 
	check'(fn _ => 
	       List.all (fn (s, xs) => isSubset(s0, s)) allsets
 	       andalso List.all (fn (s, xs) => isSubset(s, s)) allsets
 	       andalso not (isSubset(s1, s0))
 	       andalso isSubset(s3, s2a)
	       andalso isSubset(s3, s2b)
	       andalso isSubset(s2a, s2b)
	       andalso isSubset(s2b, s2a));

    (* min, max *)

    val test4 = 
	check'(fn _ => 
	       min s0 = NONE
	       andalso max s0 = NONE
	       andalso List.all (fn (s, xs) => 
				 min s = SOME (List.hd xs)) allsets
	       andalso List.all (fn (s, xs) => 
				 max s = SOME (List.last xs)) allsets)

    (* hash *)

    fun inthash i = 0w2 * Word.fromInt i

    val test4 = 
	check'(fn _ => 
	       hash inthash s0 = 0w0
	       andalso List.all (fn (s, xs) => hash inthash s = 
				 List.foldl (op+) 0w0 (List.map inthash xs))
	                        allsets)
	
    (* delete *)

    val test5 =
	check'(fn _ => 
	       List.all (fn (s, xs) => 
			 isEmpty(List.foldl (delete o swap) s xs)) allsets
	       andalso 
	       List.all (fn (s, xs) => 
			 isEmpty(List.foldr (delete o swap) s xs)) allsets);

    (* union *)

    val test6 =
	check'(fn _ => 
	       List.all (fn (s, xs) => equal(union(s, s), s)) allsets
	       andalso 
	       List.all (fn (s, xs) => equal(union(s0, s), s)) allsets
	       andalso 
	       List.all (fn (s, xs) => equal(union(s, s0), s)) allsets
	       andalso 
	       List.all (fn (s, xs) => listItems(union(s, minusOne)) 
			 = ~1 :: xs) allsets
	       andalso 
	       List.all (fn (s, xs) => listItems(union(minusOne, s)) 
			 = ~1 :: xs) allsets 
	       andalso 
	       List.all (fn (s, xs) => listItems(union(s, oneMega)) 
			 = xs @ [1000000]) allsets
	       andalso 
	       List.all (fn (s, xs) => listItems(union(oneMega, s)) 
			 = xs @ [1000000]) allsets)

    (* intersection *)

    val test7 =
	check'(fn _ => 
	       List.all (fn (s, xs) => equal(intersection(s, s), s)) allsets
	       andalso 
	       List.all (fn (s, xs) => equal(intersection(s0, s), s0)) allsets
	       andalso 
	       List.all (fn (s, xs) => equal(intersection(s, s0), s0)) allsets
	       andalso 
	       List.all (fn (s, xs) => isEmpty(intersection(s, minusOne)))
			allsets
	       andalso 
	       List.all (fn (s, xs) => isEmpty(intersection(minusOne, s)))
	                allsets 
	       andalso 
	       List.all (fn (s, xs) => isEmpty(intersection(s, oneMega)))
	                allsets
	       andalso 
	       List.all (fn (s, xs) => isEmpty(intersection(oneMega, s))) 
	                allsets)

    (* difference *)

    val test8 =
	check'(fn _ => 
	       List.all (fn (s, xs) => equal(difference(s, s), s0)) allsets
	       andalso 
	       List.all (fn (s, xs) => equal(difference(s0, s), s0)) allsets
	       andalso 
	       List.all (fn (s, xs) => equal(difference(s, s0), s)) allsets
	       andalso 
	       List.all (fn (s, xs) => equal(difference(s, minusOne), s))
			allsets
	       andalso 
	       List.all (fn (s, xs) => equal(difference(minusOne, s), 
					     minusOne))
	                allsets 
	       andalso 
	       List.all (fn (s, xs) => equal(difference(s, oneMega), s))
	                allsets
	       andalso 
	       List.all (fn (s, xs) => equal(difference(oneMega, s), oneMega))
	                allsets)

    (* union, intersection, difference *)

    (* Check: card(s1 U s2) + card(s1 n s2) = card(s1) + card(s2) *)

    fun chkcard s1 s2 =
	  numItems(union(s1, s2)) + numItems(intersection(s1, s2))
	= numItems s1 + numItems s2

    (* Check: (s1 \ s2) u (s2 \ s1) = (s1 u s2) \ (s1 n s2) *)

    fun chkdiff s1 s2 =
	  equal(union(difference(s1, s2), difference(s2, s1)),
		difference(union(s1, s2), intersection(s1, s2)));
		 
    fun chkunion s1 s2 = 
	  mergeUniq Int.compare (listItems s1, listItems s2) 
	= listItems(union(s1,s2))

    val test9 = 
	check'(fn _ => 
	List.all (fn (s1, xs1) => 
	List.all (fn (s2, xs2) => chkcard s1 s2
		                  andalso chkdiff s1 s2
				  andalso chkunion s1 s2)
		  allsets)
	       allsets);

    (* compare *)

    (* app, revapp *)

    (* foldl, foldr *)

    (* map, mapMono *)

    (* find *)

    (* subset, subList *)

    val i5000 = List.tabulate(5000, fn i => i);

    val i4000 = List.rev(List.tabulate(1000, fn i => i+4000));
	
    val set1 = addList(empty Int.compare, i5000);

    val _ = print (Int.toString (depth set1) ^ "\n");

    val set2 = List.foldr (delete o swap) set1 i4000;

    val _ = print (Int.toString (depth set2) ^ "\n");
end; 
