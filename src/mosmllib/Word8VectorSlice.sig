(* Word8VectorSlice -- SML Basis Library *)

type elem = Word8.word
type vector = Word8Vector.vector

type slice

val length   : slice -> int
val sub      : slice * int -> elem
val slice    : vector * int * int option -> slice
val full     : vector -> slice
val subslice : slice * int * int option -> slice
val base     : slice -> vector * int * int
val vector   : slice -> vector
val concat   : slice list -> vector
val isEmpty  : slice -> bool
val getItem  : slice -> (elem * slice) option

val find     : (elem -> bool) -> slice -> elem option
val exists   : (elem -> bool) -> slice -> bool
val all      : (elem -> bool) -> slice -> bool

val app      : (elem -> unit) -> slice -> unit
val map      : (elem -> elem) -> slice -> vector
val foldl    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
val foldr    : (elem * 'b -> 'b) -> 'b -> slice -> 'b

val findi    : (int * elem -> bool) -> slice -> (int * elem) option
val appi     : (int * elem -> unit) -> slice -> unit
val mapi     : (int * elem -> elem) -> slice -> vector
val foldli   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
val foldri   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b

val collate  : (elem * elem -> order) -> slice * slice -> order

(* 
   [slice] is the type of Word8Vector slices, that is, sub-vectors of
   Word8Vector.vector values.
   The slice (a,i,n) is valid if 0 <= i <= i+n <= size s, 
                or equivalently, 0 <= i and 0 <= n and i+n <= size s.  
   A valid slice sli = (a,i,n) represents the sub-vector a[i...i+n-1],
   so the elements of sli are a[i], a[i+1], ..., a[i+n-1], and n is
   the length of the slice.  Only valid slices can be constructed by
   these functions.

   All operations are as for VectorSlice.slice.
*)
