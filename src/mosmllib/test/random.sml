(* test/random.sml -- PS 2007-05-04 *)

use "auxil.sml";
load "Random";

local 
    open Random
in	
val rmax = range (valOf Int.minInt, valOf Int.maxInt);
val r42 =  range (42, 43);

val test1a = 
    check'(fn () => r42 (newgen()) = 42);
val test2a = 
    check'(fn () => (range (42, 42) (newgen()); false) 
	   handle Fail s => true | _ => false);
val test3a = check'(fn () => (rmax (newgen()); true));

val rlmax = rangelist (valOf Int.minInt, valOf Int.maxInt);
val rl42 =  rangelist (42, 43);

val test4a = 
    check'(fn () => List.all (fn x => x=42) (rl42 (1000, newgen())));
val test5a = 
    check'(fn () => (rangelist (42, 42) (10, newgen()); false) 
	   handle Fail s => true | _ => false);

val test6a = check'(fn () => (rlmax (1000, newgen()); true));

val rl8 = rangelist (~2, 6);
val test7a = 
    check'(fn () => (List.foldl Int.max ~2 (rl8 (1000, newgen())) = 5
		     andalso
		     List.foldl Int.min 6 (rl8 (1000, newgen())) = ~2));
end
