(* BasicIO -- non-standard input-output; use BinIO and TextIO instead *)

type instream
type outstream

val std_in        : instream;
val open_in       : string -> instream;
val open_in_bin   : string -> instream;
val input         : instream * int -> string;
val inputc        : instream -> int -> string;
val lookahead     : instream -> string;
val close_in      : instream -> unit;
val end_of_stream : instream -> bool;

val std_out       : outstream;
val std_err       : outstream;
val open_out      : string -> outstream;
val open_out_bin  : string -> outstream;
val output        : outstream * string -> unit;
val outputc       : outstream -> string -> unit;
val close_out     : outstream -> unit;
val flush_out     : outstream -> unit;
val input_line    : instream -> string;
val can_input     : instream * int -> bool;
val open_append   : string -> outstream;

val exit          : int -> 'a
val print         : string -> unit
