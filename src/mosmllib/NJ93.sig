(* NJ93 -- compatibility SML/NJ 0.93 top-level environment *)

val print     : string -> unit

(* NJ93 Integer *)

val max       : int * int -> int
val min       : int * int -> int

(* NJ93 List *)

exception Hd and Tl and Nth and NthTail

val hd        : 'a list -> 'a             (* Hd *)
val tl        : 'a list -> 'a list        (* Tl *)
val nth       : 'a list * int -> 'a       (* Nth *)
val nthtail   : 'a list * int -> 'a list  (* NthTail *)
val app       : ('a -> 'b) -> 'a list -> unit
val revapp    : ('a -> 'b) -> 'a list -> unit
val fold      : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b
val revfold   : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b

(* NJ93 Real *)

val ceiling   : real -> int
val truncate  : real -> int 

(* NJ93 Ref *)

val inc       : int ref -> unit
val dec       : int ref -> unit

(* NJ93 String *)

exception Substring

val ordof     : string * int -> int
val ord       : string -> int                   (* Ord *)
val chr       : int -> string                   (* Chr *)
val substring : string * int * int -> string    (* Substring *)
val explode   : string -> string list
val implode   : string list -> string

(* NJ93 top-level math functions *)

val sqrt      : real -> real
val sin       : real -> real
val cos       : real -> real
val arctan    : real -> real
val exp       : real -> real
val ln        : real -> real

(* NJ93 top-level input/output, standard *)

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

(* NJ93 top-level input/output, non-standard *)

val open_in_bin   : string -> instream
val open_out_bin  : string -> outstream
val inputc        : instream -> int -> string
val std_err       : outstream
val outputc       : outstream -> string -> unit
val flush_out     : outstream -> unit
val input_line    : instream -> string
val can_input     : instream * int -> bool
val open_append   : string -> outstream
