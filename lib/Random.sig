(* Random -- Random number generator -- 1995-04-23 *)

type generator

val newgenseed : real -> generator
val newgen     : unit -> generator
val random     : generator -> real
val randomlist : int * generator -> real list
val range      : int * int -> generator -> int
val rangelist  : int * int -> int * generator -> int list

(* Type generator is the abstract type of random number generators,
   producing uniformly distributed pseudo-random numbers.

   [newgenseed seed] returns a random number generator with the given seed.

   [newgen ()] returns a random number generator, taking the seed from
   the system clock.

   [random gen] returns a random number in the interval [0..1).

   [randomlist (n, gen)] returns a list of n random numbers in the
   interval [0,1).

   [range (min, max) gen] returns an integral random number in the
   range [min, max).  Raises Fail if min > max.

   [rangelist (min, max) (n, gen)] returns a list of n integral random
   numbers in the range [min, max).  Raises Fail if min > max.  
*)
