(* Timer -- SML Basis Library *)

type cpu_timer
type real_timer

val startCPUTimer  : unit -> cpu_timer
val totalCPUTimer  : unit -> cpu_timer
val checkCPUTimer  : cpu_timer -> 
                     { usr : Time.time, sys : Time.time, gc : Time.time }

val startRealTimer : unit -> real_timer
val totalRealTimer : unit -> real_timer
val checkRealTimer : real_timer -> Time.time

(* 
   [cpu_timer] is the type of timers for measuring CPU time consumption
   (user time, garbage collection time, and system time).

   [real_timer] is the type of timers for measuring the passing of
   real time (wall-clock time).

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
