(* CharArray -- SML Basis Library *)

eqtype array
type elem   = Char.char
type vector = CharVector.vector

val maxLen   : int

val array    : int * elem -> array
val tabulate : int * (int -> elem) -> array
val fromList : elem list -> array

val length   : array -> int
val sub      : array * int -> elem
val update   : array * int * elem -> unit
val extract  : array * int * int option -> vector

val copy     : {src: array,  si: int, len: int option, 
                dst: array, di: int} -> unit
val copyVec  : {src: vector, si: int, len: int option, 
                dst: array, di: int} -> unit

val app      : (elem -> unit) -> array -> unit
val foldl    : (elem * 'b -> 'b) -> 'b -> array -> 'b
val foldr    : (elem * 'b -> 'b) -> 'b -> array -> 'b
val modify   : (elem -> elem) -> array -> unit

val appi     : (int * elem -> unit) -> array * int * int option -> unit
val foldli   : (int * elem * 'b -> 'b) -> 'b -> array * int * int option -> 'b
val foldri   : (int * elem * 'b -> 'b) -> 'b -> array * int * int option -> 'b
val modifyi  : (int * elem -> elem) -> array * int * int option -> unit

(* 
   [array] is the type of one-dimensional, mutable, zero-based
   constant-time-access arrays with elements of type Char.char, that
   is, characters.  Arrays a1 and a2 are equal if both were created by
   the same call to a primitive, or if both are empty.

   All operations are as for Array.array.
*)
