(* Arraysort -- Quicksort for arrays, from SML/NJ library *)

val sort   : ('a * 'a -> order) -> 'a Array.array -> unit
val sorted : ('a * 'a -> order) -> 'a Array.array -> bool

(* 
   [sort ordr arr] sorts array arr in-place, using ordering relation ordr.

   [sorted ordr arr] returns true if the elements of array arr is
   appear in (weakly) increasing order, according to ordering ordr.
*)
