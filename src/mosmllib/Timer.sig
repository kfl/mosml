(* Timer -- SML Basis Library *)

local 
    type time = Time.time
in
    type cpu_timer
    type real_timer

    val startCPUTimer : unit -> cpu_timer
    val totalCPUTimer : unit -> cpu_timer
    val checkCPUTimer : cpu_timer -> {usr : time, sys : time, gc : time}

    val startRealTimer : unit -> real_timer
    val totalRealTimer : unit -> real_timer
    val checkRealTimer : real_timer -> time
end

(* A [cpu_timer] measures the CPU time consumed.

   A [real_timer] measures the real time that has passed.

   [startCPUTimer ()] returns a cpu_timer started at the moment of 
   the call.

   [totalCPUTimer ()] returns a cpu_timer started at the moment the 
   library was loaded.

   [checkCPUTimer tmr] returns {usr, sys, gc} where usr is the amount
   of user CPU time consumed since tmr was started, gc is the amount
   of user CPU time spent on garbage collection, and sys is the
   amount of system CPU time consumed since tmr was started.  Note
   that gc time is included in the usr time.  Under MS DOS, usr time
   and gc time are measured in real time.

   [startRealTimer ()] returns a real_timer started at the moment of 
   the call.

   [totalRealTimer ()] returns a real_timer started at the moment the 
   library was loaded.

   [checkRealTimer tmr] returns the amount of real time that has passed
   since tmr was started.
*)
