(* CharVector -- SML Basis Library *)

type vector = string
type elem = Char.char

val maxLen   : int

val fromList : elem list -> vector
val tabulate : int * (int -> elem) -> vector

val length   : vector -> int
val sub      : vector * int -> elem
val extract  : vector * int * int option -> vector
val concat   : vector list -> vector

val app      : (elem -> unit) -> vector -> unit
val map      : (elem -> elem) -> vector -> vector
val foldl    : (elem * 'b -> 'b) -> 'b -> vector -> 'b
val foldr    : (elem * 'b -> 'b) -> 'b -> vector -> 'b

val appi     : (int * elem -> unit) -> vector * int * int option -> unit
val mapi     : (int * elem -> elem) -> vector * int * int option -> vector
val foldli   : (int * elem * 'b -> 'b) -> 'b -> vector*int*int option -> 'b
val foldri   : (int * elem * 'b -> 'b) -> 'b -> vector*int*int option -> 'b

(* 
   [vector] is the type of one-dimensional, immutable, zero-based
   constant-time-access vectors with elements of type Char.char, that
   is, characters.  Type vector admits equality, and vectors v1 and v2
   are equal if they have the same length and their elements are
   equal.  The type vector is the same as String.string.

   All operations are as for Vector.vector.
*)
