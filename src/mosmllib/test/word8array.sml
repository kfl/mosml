(* test/word8array.sml -- some test cases for Word8Array 
   PS 1994-12-21, 1995-05-11 *)

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
val _ = (copy{src=d, si=0, dst=e, di=0,        len=NONE}; 
	 copy{src=b, si=0, dst=e, di=length d, len=NONE};
	 copy{src=d, si=0, dst=e, di=length d + length b, len=NONE});
	 
fun a2v a = extract(a, 0, NONE);
val ev = Word8Vector.concat [a2v d, a2v b, a2v d];

val test7 = check'(fn () => length e = 203);

val test8a = (update(e, ~1, w127); "WRONG")
             handle Subscript => "OK" | _ => "WRONG";
val test8b = (update(e, length e, w127); "WRONG")
             handle Subscript => "OK" | _ => "WRONG";

val f = extract (e, 100, SOME 3);

val test9 = check'(fn () => f = a2v b);

val test9a = check'(fn () => ev = extract(e, 0, NONE)
		    andalso ev = extract(e, 0, SOME (length e)));
val test9b = 
    check'(fn () => Word8Vector.fromList [] = extract(e, 100, SOME 0));
val test9c = (extract(e, ~1, SOME (length e))  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9d = (extract(e, length e+1, SOME 0) seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9e = (extract(e, 0, SOME (length e+1)) seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9f = (extract(e, 20, SOME ~1)        seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9g = (extract(e, ~1, NONE)  seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9h = (extract(e, length e+1, NONE) seq "WRONG") 
             handle Subscript => "OK" | _ => "WRONG"
val test9i = 
    check'(fn () => a2v (fromList []) = extract(e, length e, SOME 0)
	   andalso a2v (fromList []) = extract(e, length e, NONE));

val _ = copy{src=e, si=0, dst=e, di=0, len=NONE};
val g = array(203, w127);
val _ = copy{src=e, si=0, dst=g, di=0, len=NONE};

val test10a = check'(fn () => ev = extract(e, 0, NONE)
		      andalso ev = extract(e, 0, SOME (length e)));
val test10b = check'(fn () => ev = extract(g, 0, NONE)
		     andalso ev = extract(g, 0, SOME (length g)));

val _ = copy{src=g, si=203, dst=g, di=0, len=SOME 0};
val test10c = check'(fn () => ev = extract(g, 0, NONE));

val _ = copy{src=g, si=0, dst=g, di=203, len=SOME 0};
val test10d = check'(fn () => ev = extract(g, 0, NONE));

val _ = copy{src=g, si=0, dst=g, di=1, len=SOME (length g-1)};
val test10e = check'(fn () => a2v b = extract(g, 101, SOME 3));

val _ = copy{src=g, si=1, dst=g, di=0, len=SOME(length g-1)};
val test10f = check'(fn () => a2v b = extract(g, 100, SOME 3));

val _ = copy{src=g, si=202, dst=g, di=202, len=SOME 1};
val test10g = check'(fn () => g sub 202 = i2w ((202-1-103) mod 7));
val test10h = check'(fn () =>
		     (copy{src=array0, si=0, dst=array0, di=0, len=NONE}; 
		      array0 <> array(0, w127)));
val test10i = check'(fn () =>
		     (copy{src=array0, si=0, dst=array0, di=0, len=SOME 0}; 
		      array0 <> array(0, w127)));

val test11a = (copy{src=g, si= ~1, dst=g, di=0, len=NONE}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11b = (copy{src=g, si=0, dst=g, di= ~1, len=NONE}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11c = (copy{src=g, si=1, dst=g, di=0, len=NONE}; "OK") 
              handle _ => "WRONG"
val test11d = (copy{src=g, si=0, dst=g, di=1, len=NONE}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11e = (copy{src=g, si=203, dst=g, di=0, len=NONE}; "OK") 
              handle _ => "WRONG"

val test11f = (copy{src=g, si= ~1, dst=g, di=0, len=SOME (length g)}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11g = (copy{src=g, si=0, dst=g, di= ~1, len=SOME (length g)}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11h = (copy{src=g, si=1, dst=g, di=0, len=SOME (length g)}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11i = (copy{src=g, si=0, dst=g, di=1, len=SOME (length g)}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11j = (copy{src=g, si=0, dst=g, di=0, len=SOME (length g+1)}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"
val test11k = (copy{src=g, si=203, dst=g, di=0, len=SOME 1}; "WRONG") 
              handle Subscript => "OK" | _ => "WRONG"

end;
