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

val test1 = check(checkRealTime totalRealTime <= checkRealTime totalRealTime
		  andalso (checkRealTime totalRealTime before fib 25 seq ())
  		           < checkRealTime totalRealTime);

local
    val rtmr = startRealTimer ();
in
val test2 = check(checkRealTime rtmr <= checkRealTime rtmr
		  andalso (checkRealTime rtmr before fib 25 seq ())
  		           < checkRealTime rtmr);
end

local
    val op <= = fn ({usr=usr1, sys=sys1}, {usr=usr2, sys=sys2})
	=> usr1 <= usr2 andalso sys1 <= sys2;
    fun cput1 < cput2 = (cput1 <= cput2) andalso (cput1 <> cput2);
in
val test3 = check(checkCPUTime totalCPUTime <= checkCPUTime totalCPUTime
		  andalso (checkCPUTime totalCPUTime before fib 25 seq ())
  		           < checkCPUTime totalCPUTime);
val ctmr = startCPUTimer ();
val test4 = check(checkCPUTime ctmr <= checkCPUTime ctmr
		  andalso (checkCPUTime ctmr before fib 25 seq ())
  		           < checkCPUTime ctmr);
end;

val _ = 
let
    fun time f arg =
	let open Timer
	    val cputimer  = startCPUTimer ()
	    val realtimer = startRealTimer ()
	    val res = f arg
	    val {usr, sys} = checkCPUTime cputimer;
	    val gc = checkGCTime cputimer;
	    val rea = checkRealTime realtimer;
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
