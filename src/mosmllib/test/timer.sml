(* test/timer.sml
   PS 1995-03-20
*)

use "auxil.sml";

local 
    fun fib n = if n<2 then 1 else fib(n-1) + fib(n-2);

    open Time Timer
    val totalRealTime = startRealTimer ()
    val totalCPUTime  = startCPUTimer ()

in

val test1 = check(checkRealTimer totalRealTime <= checkRealTimer totalRealTime
		  andalso (checkRealTimer totalRealTime before fib 25 seq ())
  		           < checkRealTimer totalRealTime);

local
    val rtmr = startRealTimer ();
in
val test2 = check(checkRealTimer rtmr <= checkRealTimer rtmr
		  andalso (checkRealTimer rtmr before fib 25 seq ())
  		           < checkRealTimer rtmr);
end

local
    val op <= = fn ({usr=usr1, sys=sys1, gc=gc1}, {usr=usr2, sys=sys2, gc=gc2})
	=> usr1 <= usr2 andalso sys1 <= sys2 andalso gc1 <= gc1;
    fun cput1 < cput2 = (cput1 <= cput2) andalso (cput1 <> cput2);
in
val test3 = check(checkCPUTimer totalCPUTime <= checkCPUTimer totalCPUTime
		  andalso (checkCPUTimer totalCPUTime before fib 25 seq ())
  		           < checkCPUTimer totalCPUTime);
val ctmr = startCPUTimer ();
val test4 = check(checkCPUTimer ctmr <= checkCPUTimer ctmr
		  andalso (checkCPUTimer ctmr before fib 25 seq ())
  		           < checkCPUTimer ctmr);
end;

val _ = 
let
    fun time f arg =
	let open Timer
	    val cputimer  = startCPUTimer ()
	    val realtimer = startRealTimer ()
	    val res = f arg
	    val {usr, sys, gc} = checkCPUTimer cputimer;
	    val rea = checkRealTimer realtimer;
	    fun format t = Time.toString t
	in 
	    print("User: " ^ format usr ^
		"  System: " ^ format sys ^ 
		"  Gc: " ^ format gc ^ 
		"  Real: " ^ format rea ^ "\n");
	    res
	end;

    val _ = print "\nEach line below should show roughly \
                   \the same User, System, and Gc times:\n";
in
    map (time fib) [28, 28, 28, 28, 28, 28, 28, 28] seq () 
end 

end;
