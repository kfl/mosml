(* SML90 -- part of the initial basis of the 1990 Definition *)

(* Math *)

val sqrt    : real -> real
val sin     : real -> real
val cos     : real -> real
val arctan  : real -> real
val exp     : real -> real
val ln      : real -> real

(* Strings *)

val chr     : int -> string
val ord     : string -> int

val explode : string -> string list
val implode : string list -> string

exception Abs  
      and Diff 
      and Exp  
      and Floor
      and Neg  
      and Prod 
      and Sum  
      and Mod  
      and Quot

(* Input/output *)

type instream and outstream

val std_in        : instream
val open_in       : string -> instream
val input         : instream * int -> string
val lookahead     : instream -> string
val close_in      : instream -> unit
val end_of_stream : instream -> bool

val std_out       : outstream
val open_out      : string -> outstream
val output        : outstream * string -> unit
val close_out     : outstream -> unit
