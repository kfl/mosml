(* Word8ArraySlice -- SML Basis Library *)

type elem = Word8.word
type array = Word8Array.array
type vector = Word8Vector.vector
type vector_slice = Word8VectorSlice.slice

type slice

val length   : slice -> int
val sub      : slice * int -> elem
val update   : slice * int * elem  -> unit
val slice    : array * int * int option -> slice
val full     : array -> slice
val subslice : slice * int * int option -> slice
val base     : slice -> array * int * int
val vector   : slice -> vector
val copy     : {src: slice, dst: array, di: int} -> unit
val copyVec  : {src: vector_slice, dst: array, di: int} -> unit 
val isEmpty  : slice -> bool
val getItem  : slice -> (elem * slice) option

val find     : (elem -> bool) -> slice -> elem option
val exists   : (elem -> bool) -> slice -> bool
val all      : (elem -> bool) -> slice -> bool

val app      : (elem -> unit) -> slice -> unit
val foldl    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
val foldr    : (elem * 'b -> 'b) -> 'b -> slice -> 'b
val modify   : (elem -> elem) -> slice -> unit

val findi    : (int * elem -> bool) -> slice -> (int * elem) option
val appi     : (int * elem -> unit) -> slice -> unit
val foldli   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
val foldri   : (int * elem * 'b -> 'b) -> 'b -> slice -> 'b
val modifyi  : (int * elem -> elem) -> slice -> unit

val collate  : (elem * elem -> order) -> slice * slice -> order

(* 
   [slice] is the type of Word8Array slices, that is, sub-arrays of
   Word8Array.array values.
   The slice (a,i,n) is valid if 0 <= i <= i+n <= size s, 
                or equivalently, 0 <= i and 0 <= n and i+n <= size s.  
   A valid slice sli = (a,i,n) represents the sub-array a[i...i+n-1],
   so the elements of sli are a[i], a[i+1], ..., a[i+n-1], and n is
   the length of the slice.  Only valid slices can be constructed by
   the functions below.

   All operations are as for ArraySlice.slice.
*)
