(* Timer -- SML Basis Library *)

type cpu_timer
type real_timer

val startCPUTimer  : unit -> cpu_timer
val totalCPUTimer  : unit -> cpu_timer
val checkCPUTimer  : cpu_timer -> { usr : Time.time, sys : Time.time }
val checkGCTime    : cpu_timer -> Time.time
val checkCPUTimes  : cpu_timer -> { nongc : {usr : Time.time, sys : Time.time},
                                    gc    : {usr : Time.time, sys : Time.time}
                                  }

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

   [checkCPUTimer tmr] returns {usr, sys} where usr is the amount of
   user CPU time consumed since tmr was started and sys is the amount
   of system CPU time consumed since tmr was started.  Note that
   garbage collection time is included in the usr time.  Under MS DOS
   and MS Windows, usr time is measured as real time.

   [checkGCTime tmr] returns the amount of user CPU time spent on
   garbage collection since tmr was started.  Under MS DOS and MS
   Windows, gc time is measured in real time.

   [checkCPUTimes tmr] returns the amount of CPU time consumed since
   tmr was started spilt into time spend in the program (nongc) and on
   garbage collecttion (gc). For both nongc and gc a record {usr, sys}
   is returned where usr is the amount of user CPU time consumed since
   tmr was started and sys is the amount of system CPU time consumed
   since tmr was started.  Note that Moscow ML will allways attribute
   all the system CPU time to the program (nongc).  That is,
   #sys(#gc(checkCPUTimes tmr)) is always 0.

   [startRealTimer ()] returns a real_timer started at the moment of 
   the call.

   [totalRealTimer ()] returns a real_timer started at the moment the 
   library was loaded.

   [checkRealTimer tmr] returns the amount of real time that has passed
   since tmr was started.
*)
