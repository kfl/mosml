(* Time -- SML Basis Library *)

eqtype time

exception Time

val zeroTime : time
val now      : unit -> time

val toSeconds        : time -> int
val toMilliseconds   : time -> int
val toMicroseconds   : time -> int
val fromSeconds      : int -> time
val fromMilliseconds : int -> time
val fromMicroseconds : int -> time

val fromReal : real -> time
val toReal   : time -> real

val toString   : time -> string	(* rounded to millisecond precision *)
val fmt        : int -> time -> string
val fromString : string -> time option
val scan       : (char, 'a) StringCvt.reader -> (time, 'a) StringCvt.reader

val +  : time * time -> time
val -  : time * time -> time
val <  : time * time -> bool
val <= : time * time -> bool
val >  : time * time -> bool
val >= : time * time -> bool

val compare : time * time -> order

(* Values of type [time] represent durations as well as absolute points 
   in time (which can be thought of as durations since some time zero).

   [zeroTime] represents the 0-second duration, and the origin of time, 
   so zeroTime + t = t + zeroTime = t for all t.

   [now ()] returns the point in time at which the application occurs.

   [fromSeconds s] returns the time value corresponding to s seconds.  
   Raises Time if s < 0.

   [fromMilliseconds ms] returns the time value corresponding to ms
   milliseconds.  Raises Time if ms < 0.

   [fromMicroseconds us] returns the time value corresponding to us
   microseconds.  Raises Time if us < 0.

   [toSeconds t] returns the number of seconds represented by t,
   truncated.  Raises Overflow if that number is not representable as
   an int.

   [toMilliseconds t] returns the number of milliseconds
   represented by t, truncated.  Raises Overflow if that number is not
   representable as an int.

   [toMicroseconds t] returns the number of microseconds
   represented by t, truncated.  Raises Overflow if t that number is
   not representable as an int.

   [realToTime r] converts a real to a time value representing that
   many seconds.  Raises Time if r < 0 or if r is not representable
   as a time value.  It holds that realToTime 0.0 = zeroTime.  

   [timeToReal t] converts a time the number of seconds it represents;
   hence realToTime and timeToReal are inverses of each other when 
   defined.  Raises Overflow if t is not representable as a real.

   [fmt n t] returns as a string the number of seconds represented by
   t, rounded to n decimal digits.  If n <= 0, then no decimal digits
   are reported. 

   [toString t] returns as a string the number of seconds represented
   by t, rounded to 3 decimal digits.  Equivalent to (fmt 3 t).  

   [fromString s] returns SOME t where t is the time value represented
   by the string s of form [\n\t ]*([0-9]+(\.[0-9]+)?)|(\.[0-9]+); 
   or returns NONE if s cannot be parsed as a time value.

   [scan getc src], where getc is a character accessor, returns SOME
   (t, rest) where t is a time and rest is rest of the input, or NONE
   if s cannot be parsed as a time value.

   [t1 + t2] is the sum of the times t1 and t2.  For reals r1, r2 >= 0.0, 
   realToTime r1 + realToTime r2 = realToTime(Real.+(r1,r2)).  Raises 
   Overflow if the result is not representable as a time value.

   [t1 - t2] is the t1 minus t2, that is, the duration from t2 to t1.
   Raises Time if t1 < t2 or if the result is not representable as a
   time value.  It holds that t - zeroTime = t.

   [t1 < t2] asserts that t1 is strictly before t2.  Similarly for
   <=, >, >=.  It holds for reals r1, r2 >= 0.0 that
       realToTime r1 < realToTime r2  iff  Real.<(r1, r2) 

   [compare(t1, t2)] returns LESS, EQUAL, or GREATER, according 
   as t1 precedes, equals, or follows t2 in time.
*)
