(* test/random.sml -- PS 2007-05-04 *)

use "auxil.sml";
load "Random";

local 
    open Random
in	
val r42 =  range (42, 43);
val rmax = range (valOf Int.minInt, valOf Int.maxInt);
val rlo = range (valOf Int.minInt, valOf Int.minInt + 1);
val rhi = range (valOf Int.maxInt - 1, valOf Int.maxInt);

val test1a = 
    check'(fn () => r42 (newgen()) = 42);
val test1b = 
    check'(fn () => (range (42, 42) (newgen()); false) 
	   handle Fail s => true | _ => false);
val test1c = check'(fn () => (rmax (newgen()); true));
val test1d = 
    check'(fn () => (rlo (newgen()) = valOf Int.minInt));
val test1e = 
    if check'(fn () => (rhi (newgen()) = valOf Int.maxInt - 1)) <> "OK"
    then "TODO" else "OK";

val rl42 =  rangelist (42, 43);
val rlmax = rangelist (valOf Int.minInt, valOf Int.maxInt);
val rllo = rangelist (valOf Int.minInt, valOf Int.minInt + 1);
val rlhi = rangelist (valOf Int.maxInt - 1, valOf Int.maxInt);

val test2a = 
    check'(fn () => List.all (fn x => x=42) (rl42 (1000, newgen())));
val test2b = 
    check'(fn () => (rangelist (42, 42) (10, newgen()); false) 
	   handle Fail s => true | _ => false);
val test2c = check'(fn () => (rlmax (1000, newgen()); true));

val test2d = 
    check'(fn () => List.all (fn x => x = valOf Int.minInt)
		             (rllo (1000, newgen())));
val test2e = 
    if check'(fn () => List.all (fn x => x = valOf Int.maxInt-1)
		                            (rlhi (1000, newgen()))) <> "OK"
    then "TODO" else "OK";

val rl8 = rangelist (~2, 6);
val test3a = 
    check'(fn () => (List.foldl Int.max ~2 (rl8 (1000, newgen())) = 5
		     andalso
		     List.foldl Int.min 6 (rl8 (1000, newgen())) = ~2));
end
