(* Misc -- top-level types and functions that ought to be in General *)

type 'a array  = 'a Array.array

val o         : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c
val before    : 'a * 'b -> 'a

val chr       : int -> char
val ord       : char -> int

val explode   : string -> char list
val implode   : char list -> string
val str       : char -> string
val concat    : string list -> string
val substring : string * int * int -> string


val vector    : 'a list -> 'a vector

val getOpt    : 'a option * 'a -> 'a
val isSome    : 'a option -> bool
val valOf     : 'a option -> 'a      

exception Empty

val @         : 'a list * 'a list -> 'a list
val app       : ('a -> unit) -> 'a list -> unit
val foldl     : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val foldr     : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
val hd        : 'a list -> 'a                     (* Empty     *)
val length    : 'a list -> int 
val map       : ('a -> 'b) -> 'a list -> 'b list
val null      : 'a list -> bool
val rev       : 'a list -> 'a list 
val tl        : 'a list -> 'a list                (* Empty     *)

val print     : string -> unit
