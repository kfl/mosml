(* LargeInt -- arbitrary-precision integers 1995-09-04, 1998-04-12     *)
(* This module requires Dynlib and the GNU GMP package to be installed *)

type int

val precision : int option
val minInt    : int option
val maxInt    : int option

val ~    : int -> int
val +    : int * int -> int
val -    : int * int -> int
val *    : int * int -> int
val div  : int * int -> int
val mod  : int * int -> int
val quot : int * int -> int
val rem  : int * int -> int
val <    : int * int -> bool
val >    : int * int -> bool
val <=   : int * int -> bool
val >=   : int * int -> bool
val eq   : int * int -> bool
val ne   : int * int -> bool
val abs  : int -> int
val min  : int * int -> int
val max  : int * int -> int

val divMod   : int * int -> int * int
val quotRem  : int * int -> int * int
val pow      : int * Int.int -> int
val log2     : int -> Int.int

val sign     : int -> Int.int
val sameSign : int * int -> bool
val compare  : int * int -> order

val fromInt    : Int.int -> int
val toInt      : int -> Int.int		(* Overflow *)
val toLarge    : int -> int
val fromLarge  : int -> int

val fromString : string -> int option
val toString   : int -> string

val scan : StringCvt.radix
           -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
val fmt  : StringCvt.radix -> int -> string
