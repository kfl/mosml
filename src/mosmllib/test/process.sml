(* test/process.sml
   PS 2000-11-01
*)

use "auxil.sml";

local 
    open Process
in

val test1 = 
    check'(fn _ => isSuccess success andalso not (isSuccess failure));

val test2 = 
    check'(fn _ => 
	   isSuccess (system "mosmlc")
	   andalso not (isSuccess (system "nonsuchprogramexists")));

val test3a = 
    check'(fn _ =>
	   let open Time 
	       val ms = fromMilliseconds 1
	       val t1 = now()
	   in sleep zeroTime; t1 + ms > now() end);

val test3b = 
    check'(fn _ =>
	   let open Time 
	       val ms = fromMilliseconds 1
	       val t1 = now()
	   in sleep (zeroTime - now()); t1 + ms > now() end);

val test3c = 
    check'(fn _ =>
	   let open Time 
	       val ms500  = fromMilliseconds 500
	       val ms1500 = fromMilliseconds 1500
	       val t1 = now()
	       val _ = sleep (fromSeconds 1)
	       val t2 = now()
	   in t1 + ms500 < t2 andalso t2 < t1 + ms1500 end);

val test4 = 
    check'(fn _ => Option.isSome(getEnv "HOME")
	   andalso not (Option.isSome (getEnv "nonsuchvariableexists")));
end
