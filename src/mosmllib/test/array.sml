(* test/array.sml -- some test cases for Array 
   PS 1994-12-10, 1995-06-14, 1995-11-07, 2000-10-19 *)

use "auxil.sml";

load "ArraySlice";

local 
    open Array 
    infix 9 sub
    val array0 = fromList []
in

val a = fromList [1,11,21,31,41,51,61];
val b = fromList [441,551,661];
val c = fromList [1,11,21,31,41,51,61];

val test1 = check'(fn () => a<>c);
val test2 = 
    check'(fn () => 
	   array(0, 11) <> array0
	   andalso array(0,()) <> tabulate(0, fn _ => ())
	   andalso tabulate(0, fn _ => ()) <> fromList [] 
	   andalso fromList [] <> fromList [] 
	   andalso array(0, ()) <> array(0, ())
	   andalso tabulate(0, fn _ => ()) <> tabulate(0, fn _ => ()));

val d = tabulate(100, fn i => i mod 7 * 10 + 1);

val test3 = 
    check'(fn () => d sub 27 = 61);

val test4a = (tabulate(maxLen+1, fn i => i) seq "WRONG")
            handle Size => "OK" | _ => "WRONG";

val test4b = (tabulate(~1, fn i => i) seq "WRONG")
            handle Size => "OK" | _ => "WRONG";

val test4c = 
    check'(fn () => length (tabulate(0, fn i => i div 0)) = 0);

val test5a = 
    check'(fn () => length (fromList []) = 0 andalso length a = 7);
val test5b = 
    check'(fn () => length array0 = 0);

val test6a = (c sub ~1 seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6b = (c sub 7  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test6c = check'(fn () => c sub 0 = 1);

val e = array(203, 0);
val _ = (copy{src=d, dst=e, di=0}; 
	 copy{src=b, dst=e, di=length d};
	 copy{src=d, dst=e, di=length d + length b});
	 
fun a2v a = vector a
val ev = Vector.concat [a2v d, a2v b, a2v d]; (* length e = 203 *)

val test7 = check'(fn () => length e = 203);

val test8a = (update(e, ~1, 99) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test8b = (update(e, length e, 99) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";

val f = ArraySlice.vector(ArraySlice.slice (e, 100, SOME 3));

val test9 = check'(fn () => f = a2v b);

val test9a = 
    check'(fn () => ev = vector e);
val test9b = 
    check'(fn () => a2v (fromList []) = vector array0);

val _ = copy{src=e, dst=e, di=0};
val g = array(203, 9999999);
val _ = copy{src=e, dst=g, di=0};

val test10a = check'(fn () => ev = vector g);

val test10b = 
    check'(fn () => (copy{src=array0, dst=array0, di=0}; 
		     array0 <> array(0, 999999)));
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
val test11b = (copy{src=g, dst=g, di=202}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11c = (copy{src=b, dst=g, di = ~1}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11d = (copy{src=b, dst=g, di=203}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11e = check'(fn () => ev = vector g);

local 
    val v = ref 0
    fun setv c = v := c;
    fun addv c = v := c + !v;
    fun setvi (i, c) = v := c + i;
    fun addvi (i, c) = v := c + i + !v;
    fun cons (x,r) = x ::  r
    fun consi (i,x,r) = (i,x) ::  r
    val inplist = [7,9,13];
    val inp = fromList inplist
    val pni = fromList (rev inplist)
    fun copyinp a = copy{src=inp, dst=a, di=0}
in 

val array0 = fromList [] : int array;

val test12a =
    check'(fn _ =>
	           foldl cons [1,2] array0 = [1,2]
	   andalso foldl cons [1,2] inp = [13,9,7,1,2]
	   andalso (foldl (fn (x, _) => setv x) () inp; !v = 13));

val test12b =
    check'(fn _ =>
	           foldr cons [1,2] array0 = [1,2]
	   andalso foldr cons [1,2] inp = [7,9,13,1,2]
	   andalso (foldr (fn (x, _) => setv x) () inp; !v = 7));

val test12c =
    check'(fn _ =>
	           find (fn _ => true) array0 = NONE
	   andalso find (fn _ => false) inp = NONE
	   andalso find (fn x => x=7) inp = SOME 7
	   andalso find (fn x => x=9) inp = SOME 9
	   andalso (setv 0; find (fn x => (addv x; x=9)) inp; !v = 7+9));

val test12d = 
    check'(fn _ =>
           (setv 117; app setv array0; !v = 117)
	   andalso (setv 0; app addv inp; !v = 7+9+13)
	   andalso (app setv inp; !v = 13));
val test12e = 
    let val a = array(length inp, inp sub 0)
    in 
	check'(fn _ =>
           (modify (~ : int -> int) array0; true)
	   andalso (copyinp a; modify ~ a; foldr (op::) [] a = map ~ inplist)
	   andalso (setv 117; modify (fn x => (setv x; 37)) a; !v = ~13))
    end
val test12f = 
    check'(fn _ => 
	   not (exists (fn i => i>61) a)
	   andalso exists (fn i => i>41) a
	   andalso not (exists (fn _ => true) array0));
val test12g = 
    check'(fn _ => 
	   (setv 117; exists (fn x => (setv x; false)) array0; !v = 117)
	   andalso (setv 0; exists (fn x => (addv x; false)) inp; !v = 7+9+13)
	   andalso (exists (fn x => (setv x; false)) inp; !v = 13));
val test12h = 
    check'(fn _ => 
	   not (all (fn i => i<61) a)
	   andalso all (fn i => i<62) a
	   andalso all (fn _ => false) array0);
val test12i = 
    check'(fn _ => 
	   (setv 117; all (fn x => (setv x; true)) array0; !v = 117)
	   andalso (setv 0; all (fn x => (addv x; true)) inp; !v = 7+9+13)
	   andalso (all (fn x => (setv x; true)) inp; !v = 13));

val test13a =
    check'(fn _ =>
	           foldli consi [] array0 = []
	   andalso foldri consi [] array0 = []
	   andalso (setv 117; foldli (fn (_, x, _) => setv x) () array0; 
		    !v = 117)
	   andalso (setv 117; foldri (fn (_, x, _) => setv x) () array0; 
		    !v = 117));
val test13b =
    check'(fn _ =>
	           foldli consi [] array0 = []
	   andalso foldri consi [] array0 = []
	   andalso foldli consi [] inp = [(2,13),(1,9),(0,7)]
	   andalso foldri consi [] inp = [(0,7),(1,9),(2,13)]
	   andalso (foldli (fn (_, x, _) => setv x) () inp; !v = 13)
	   andalso (foldri (fn (_, x, _) => setv x) () inp; !v = 7));

val test14a =
    check'(fn _ =>
	   findi (fn _ => true) array0 = NONE
   andalso findi (fn _ => false) inp = NONE
   andalso findi (fn (i, x) => x=9 orelse 117 div (2-i) = 0) inp = SOME (1,9));

val test14b =
    check'(fn _ =>
	   (setvi (0,117); 
	    findi (fn arg => (setvi arg; false)) array0;
	    !v = 117));
val test14c =
    check'(fn _ =>
	   (setvi (0,0); 
	    findi (fn arg => (addvi arg; false)) inp;
	    !v = 0+7+1+9+2+13));

val test15a = 
    check'(fn _ =>
           (setvi (0,117); appi setvi array0; !v = 117)
	   andalso (setvi (0,0); appi addvi inp; !v = 0+7+1+9+2+13)
	   andalso (appi setvi inp; !v = 2+13));

val test16a = 
    let val a = array(length inp, inp sub 0)
    in 
	check'(fn _ =>
           (modifyi (op +) array0; true)
	   andalso (copyinp a; modifyi (op -) a; 
		    foldr (op::) [] a = [~7,~8,~11]))
    end

end

val test17 = 
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
end
