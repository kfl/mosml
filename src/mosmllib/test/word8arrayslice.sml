(* test/word8arrayslice.sml -- some test cases for Word8ArraySlice 
   sestoft@dina.kvl.dk 2000-10-25 *)

use "auxil.sml";

local 
    open Word8Array Word8ArraySlice 
    infix 9 sub
    val array0 = fromList []
    fun cons (x,r) = x ::  r
    fun consi (i,x,r) = (i,x) ::  r
    val i2w = Word8.fromInt;
    fun l2a xs = Word8Array.fromList (List.map i2w xs)
    fun l2v xs = Word8Vector.fromList (List.map i2w xs)
    fun cl2a xs = Word8Array.fromList (List.map (i2w o Char.ord) xs)
in

val a = l2a [1,11,21,31,41,51,61];
val b = l2a [44,55,66];
val c = l2a [1,11,21,31,41,51,61];

val slice00 = slice(array0, 0, NONE)
val slice01 = slice(array0, 0, SOME 0)
val slice02 = slice(a, 0, SOME 0)
val slice03 = slice(a, 7, NONE)
val slice04 = slice(a, 7, SOME 0)
val slice05 = slice(a, 4, SOME 0)

val slicea07 = full a
val slicea02 = slice(a, 0, SOME 2); 
val slicea23 = slice(a, 2, SOME 3); 
val slicea25 = slice(a, 2, SOME 5); 

val slice06 = subslice(slicea23, 0, SOME 0)
val slice07 = subslice(slicea23, 1, SOME 0)
val slice08 = subslice(slicea23, 3, NONE)
val slice09 = subslice(slicea23, 3, SOME 0)

val slice0s = [slice00, slice01, slice02, slice03, slice04, slice05,
	       slice06, slice07, slice08, slice09];

val sliceas = [slicea07, slicea02, slicea23, slicea25];

val test1a = 
    check'(fn _ => List.all 
	   (fn sli => vector sli = l2v [] 
	    andalso length sli = 0
	    andalso isEmpty sli
	    andalso vector (subslice(sli, 0, NONE)) = l2v []
	    andalso vector (subslice(sli, 0, SOME 0)) = l2v []
	    andalso all (fn _ => false) sli
	    andalso not (exists (fn _ => true) sli)
	    andalso NONE = find (fn _ => true) sli
	    andalso NONE = findi (fn _ => true) sli
	    andalso not (Option.isSome (getItem sli))
	    andalso (copy{src=sli, dst=array0, di=0}; true)
	    andalso (app (fn _ => raise Fail "1a app") sli; true)
	    andalso (appi (fn _ => raise Fail "1a appi") sli; true)
	    andalso foldl cons [0w1,0w2] sli = [0w1,0w2]
	    andalso foldli consi [] sli = []
	    andalso foldr cons [0w1,0w2] sli = [0w1,0w2]
	    andalso foldri consi [] sli = []
	    andalso (modify Word8.~ sli; vector sli = l2v [])
	    andalso (modifyi (fn (_, x) => Word8.~ x) sli; vector sli = l2v [])
	    andalso collate Word8.compare (sli, slice00) = EQUAL)
	   slice0s);

val test1b = 
    check'(fn _ => 
	   vector slicea02 = l2v[1, 11]
	   andalso vector slicea23 = l2v[21,31,41]
	   andalso vector slicea25 = l2v[21,31,41,51,61]
	   andalso vector slicea07 = l2v[1,11,21,31,41,51,61]
	   andalso base slicea02 = (a, 0, 2)
	   andalso base slicea23 = (a, 2, 3)
	   andalso base slicea25 = (a, 2, 5)
	   andalso base slicea07 = (a, 0, 7)
	   andalso length slicea02 = 2
	   andalso length slicea23 = 3
	   andalso length slicea25 = 5
	   andalso length slicea07 = 7);

val test2a = 
    check'(fn _ => 
	   slicea07 sub 0 = 0w1 
	   andalso slicea07 sub 6 = 0w61 
	   andalso slicea23 sub 0 = 0w21 
	   andalso slicea23 sub 2 = 0w41);

val test2b = 
    (slicea07 sub ~1; "WRONG") handle Subscript => "OK" | _ => "WRONG";

val test2c = 
    (slicea07 sub 7; "WRONG") handle Subscript => "OK" | _ => "WRONG";

val test2c = 
    (slicea23 sub ~1; "WRONG") handle Subscript => "OK" | _ => "WRONG";

val test2d = 
    (slicea23 sub 3; "WRONG") handle Subscript => "OK" | _ => "WRONG";

val test2e = 
    check'(fn _ =>
	   List.all (fn sli => ((sli sub 0; false) 
				handle Subscript => true)) slice0s);

val test3a = 
    check'(fn _ => List.all (not o isEmpty) sliceas)

val test4a =
    check'(fn _ => vector (subslice(slicea23, 0, SOME 0)) = l2v[]
	   andalso vector (subslice(slicea23, 0, NONE)) = l2v[21,31,41]
	   andalso vector (subslice(slicea23, 0, SOME 1)) = l2v[21]
	   andalso vector (subslice(slicea23, 0, SOME 2)) = l2v[21,31]
	   andalso vector (subslice(slicea23, 1, SOME 2)) = l2v[31,41]
	   andalso vector (subslice(slicea23, 3, SOME 0)) = l2v[]);

val test4b =
    (subslice(slicea23, 3, SOME 1); "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";    

val test4c =
    (subslice(slicea23, ~1, NONE); "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";    

val test4d =
    (subslice(slicea23, ~1, SOME 2); "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";    

val test4e =
    (subslice(slicea23, 4, NONE); "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";    

val test4f =
    (subslice(slicea23, 4, SOME ~2); "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";    

val test4g =
    (subslice(slicea23, 2, SOME 2); "WRONG") 
    handle Subscript => "OK" | _ => "WRONG";    

val test5 = 
    check'(fn _ => let val (i1, r1) = Option.valOf (getItem slicea23)
		       val (i2, r2) = Option.valOf (getItem r1)
		       val (i3, r3) = Option.valOf (getItem r2)
		   in 
		       i1 = 0w21 andalso i2 = 0w31 andalso i3 = 0w41 
		       andalso not (Option.isSome (getItem r3))
		   end);

val test6a = (update(slicea23, ~1, 0w99) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test6b = (update(slicea23, 3, 0w99) seq "WRONG")
             handle Subscript => "OK" | _ => "WRONG";

val test6c = 
    check'(fn _ => 
	   (update(slicea23, 0, 0w99); Word8Array.sub(a, 2) = 0w99)
	   andalso (update(slicea23, 2, 0w199); Word8Array.sub(a, 4) = 0w199)
	   andalso (update(slicea23, 0, 0w21);  Word8Array.sub(a, 2) = 0w21)
	   andalso (update(slicea23, 2, 0w41);  Word8Array.sub(a, 4) = 0w41));

val sliced = full (tabulate(100, fn i => i2w(i mod 7 * 10 + 1)));
val sliceb = full b;

val e = array(203, 0w0);
val _ = (copy{src=sliced, dst=e, di=0}; 
	 copy{src=sliceb, dst=e, di=length sliced};
	 copy{src=sliced, dst=e, di=length sliced + length sliceb});

val ev = Word8Vector.concat [vector sliced, vector sliceb, vector sliced]; 
(* length e = 203 *)

val slicee = full e

val test9a = 
    check'(fn () => vector(subslice(slicee, 100, SOME 3)) = vector sliceb);
val test9b = 
    check'(fn () => 
	   ev = vector (subslice(slicee, 0, SOME (length slicee)))
	   andalso ev = vector (subslice(slicee, 0, NONE)));

val _ = copy{src=slicee, dst=e, di=0};
val g = array(203, 0w99);
val _ = copy{src=slicee, dst=g, di=0};

val sliceg = full g;

val test10a = 
	   check'(fn () => ev = Word8Array.vector e
		  andalso ev = Word8Array.vector g);

val sliceg0 = slice(g, 0, SOME (Word8Array.length g - 1));
val _ = copy{src=sliceg0, dst=g, di=1};
val test10b = check'(fn () => vector sliceb = vector (slice(g, 101, SOME 3)));

val sliceg1 = slice(g, 1, SOME (Word8Array.length g - 1));
val _ = copy{src=sliceg1, dst=g, di=0};
val test10c = check'(fn () => vector sliceb = vector (slice(g, 100, SOME 3)));

val sliceg202 = slice(g, 202, SOME 1);
val _ = copy{src=sliceg202, dst=g, di=202};
val test10d = 
    check'(fn () => sliceg sub 202 = i2w(10 * (202-1-103) mod 7 + 1));

val test11a = (copy{src=sliceg, dst=g, di= ~1}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11b = (copy{src=sliceg1, dst=g, di=0}; "OK") 
              handle _ => "WRONG"
val test11c = (copy{src=sliceg, dst=g, di=1}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"

local 
    val v = ref 0w0
    fun setv c = v := c;
    fun addv c = v := c + !v;
    fun setvi (i, c) = v := c + i2w i;
    fun setvif (i, c, _) = v := c + i2w i;
    fun addvi (i, c) = v := c + i2w i + !v;
    fun cons (x,r) = x ::  r
    fun consi (i,x,r) = (i,x) ::  r
    val inplist = [1,2,3,4,7,9,13,4,5,6,8,0];
    val inpa = l2a inplist
    val inp = slice(inpa, 4, SOME 3)
    val pnia = l2a (rev inplist)
    val pni = slice(pnia, 5, SOME 3)
    fun resetinp () = copy{src=full(l2a inplist), dst=inpa, di=0}
in 

val test12a =
    check'(fn _ =>
	           foldl cons [0w1,0w2] inp = [0w13,0w9,0w7,0w1,0w2]
	   andalso (foldl (fn (x, _) => setv x) () inp; !v = 0w13));

val test12b =
    check'(fn _ =>
	           foldr cons [0w1,0w2] inp = [0w7,0w9,0w13,0w1,0w2]
	   andalso (foldr (fn (x, _) => setv x) () inp; !v = 0w7));

val test12c =
    check'(fn _ =>
	   find (fn _ => false) inp = NONE
	   andalso find (fn x => x=0w7) inp = SOME 0w7
	   andalso find (fn x => x=0w9) inp = SOME 0w9
	   andalso (setv 0w0; find (fn x => (addv x; x=0w9)) inp; !v = 0w7+0w9));

val test12d = 
    check'(fn _ =>
           ((setv 0w0; app addv inp; !v = 0w7+0w9+0w13)
	    andalso (app setv inp; !v = 0w13)));

val test12e = 
    check'(fn _ =>
	   (resetinp(); modify Word8.~ inp; 
	    foldr (op::) [] inp = [Word8.~ 0w7, Word8.~ 0w9, Word8.~ 0w13])
	   andalso (resetinp(); setv 0w117; 
		    modify (fn x => (setv x; 0w37)) inp; !v = 0w13))

val _ = resetinp(); 

val test12f = 
    check'(fn _ => 
	   not (exists (fn i => i>0w13) inp)
	   andalso exists (fn i => i>0w12) inp);
val test12g = 
    check'(fn _ => 
	   (setv 0w117; exists (fn x => (setv x; false)) slice05; !v = 0w117)
	   andalso (setv 0w0; exists (fn x => (addv x; false)) inp; 
		    !v = 0w7+0w9+0w13)
	   andalso (exists (fn x => (setv x; false)) inp; !v = 0w13));
val test12h = 
    check'(fn _ => 
	   not (all (fn i => i<0w13) inp)
	   andalso all (fn i => i<0w14) inp);
val test12i = 
    check'(fn _ => 
	   (setv 0w117; all (fn x => (setv x; true)) slice05; !v = 0w117)
	   andalso (setv 0w0; all (fn x => (addv x; true)) inp; 
		    !v = 0w7+0w9+0w13)
	   andalso (all (fn x => (setv x; true)) inp; !v = 0w13));

val _ = resetinp();

val test13 =
    check'(fn _ =>
	   foldli consi [] inp = [(6,0w13),(5,0w9),(4,0w7)]
	   andalso foldri consi [] inp = [(4,0w7),(5,0w9),(6,0w13)]
	   andalso (resetinp(); setv 0w117; 
		    foldli setvif () inp; !v = 0w6+0w13)
	   andalso (resetinp(); setv 0w117; 
		    foldri setvif () inp; !v = 0w4+0w7));

val _ = resetinp();

val test14a =
    check'(fn _ =>
	   findi (fn _ => false) inp = NONE
	   andalso findi (fn (i,x) => x=0w9) inp = SOME (5,0w9)
	   andalso findi (fn (i,x) => i=6) inp = SOME (6,0w13));

val test14b =
    check'(fn _ =>	   
	   List.all (fn sli => NONE=findi (fn (j, x) => i2w(j*10+1)<>x) sli)
	            sliceas)

val test15 = 
    check'(fn _ =>
           ((setvi (0,0w0); appi addvi inp; !v = 0w4+0w7+0w5+0w9+0w6+0w13)
	   andalso (appi setvi inp; !v = 0w6+0w13)));

val test16 = 
    check'(fn _ =>
	   let fun iwsub (i, w) = i2w i - w
	   in 
	       (resetinp(); modifyi iwsub inp; 
		vector inp = l2v[~3,~4,~7])
	       andalso (resetinp(); setv 0w117; 
			modifyi (fn x => (setvi x; 0w37)) inp; !v = 0w6+0w13)
	   end);
end

val test17a = 
    check'(fn _ =>
	   let fun invcompare (c1, c2) = Word8.compare (c2, c1) 
	       fun coll s1 s2 = 
		   collate invcompare (full (cl2a (explode s1)),
				       full (cl2a (explode s2)))
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

val test17b = 
    check'(fn _ =>
  let val sa = cl2a (explode "AAAAaAbAABBBB");
                           (* 0123456789012 *)
      fun invcompare (c1, c2) = Word8.compare (c2, c1) 
      fun coll s1 s2 = collate invcompare (s1, s2)
  in 
      coll (full sa) (slice(sa, 0, SOME 13)) = EQUAL
      andalso coll (slice(sa, 0, SOME 0)) (slice(sa, 13, SOME 0)) = EQUAL
      andalso coll (slice(sa, 0, SOME 0)) (slice(sa, 0, SOME 13)) = LESS
      andalso coll (slice(sa, 0, SOME 13)) (slice(sa, 0, SOME 0)) = GREATER
      andalso coll (slice(sa, 0, SOME 3)) (slice(sa, 1, SOME 3)) = EQUAL
      andalso coll (slice(sa, 0, SOME 4)) (slice(sa, 1, SOME 4)) = GREATER
      andalso coll (slice(sa, 1, SOME 4)) (slice(sa, 0, SOME 4)) = LESS
  end)
end
