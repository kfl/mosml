(* Nonstdio -- non-standard I/O -- use BinIO and TextIO instead *)

local open BasicIO in

val open_in_bin       : string -> instream
val buff_input        : instream -> CharArray.array -> int -> int -> int
val input_char        : instream -> char		(* Raises Size *)
val input_binary_int  : instream -> int
val input_value       : instream -> 'a
val seek_in           : instream -> int -> unit
val pos_in            : instream -> int
val in_stream_length  : instream -> int
val fast_really_input : instream -> string -> int ->  int -> unit

val open_out_bin      : string -> outstream
val open_out_exe      : string -> outstream
val output_char       : outstream -> Char.char -> unit
val output_byte       : outstream -> int -> unit
val buff_output       : outstream -> CharArray.array -> int -> int -> unit
val output_binary_int : outstream -> int -> unit
val output_value      : outstream -> 'a -> unit
val seek_out          : outstream -> int -> unit
val pos_out           : outstream -> int

val file_exists       : string -> bool

end
