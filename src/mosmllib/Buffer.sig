signature Buffer =
sig
    type buf
    val new      : int -> buf
    val contents : buf -> string
    val size     : buf -> int
    val clear    : buf -> unit
    val reset    : buf -> unit

    val addChar      : buf -> char -> unit
    val addString    : buf -> string -> unit
    val addSubString : buf -> substring -> unit
end

(* [buf] is the type of string buffers that allows efficient
   concatenation at the end and automatically expand as necessary.  It
   provides accumulative concatenation of strings in quasi-linear time
   (instead of quadratic time when strings are concatenated pairwise).

   [new hint] creates a new empty buffer.  Raises Size if hint <= 0 or
   hint > String.maxSize.
   The argument hint is used as the initial size of the internal
   string that holds the buffer contents.  The internal string is
   automatically reallocated as sontents is stored in the buffer.  For
   best performance, hint should be of the same order of magnitude as
   the number of characters that are expected to be stored in the
   buffer (for instance, 80 for a buffer that holds one output line).
   Nothing bad will happen if the buffer grows beyond that limit,
   however.  In doubt, take hint = 16 for instance. 

   [contents buf] returns the contents of buf.

   [size buf] returns the size of the contents of buf.

   [clear buf] emptys buf.

   [reset buf] emptys buf and shrink the internal string to the
   initial hint.

   [addChar buf c] appends c at the end of buf.

   [addString buf s] appends s at the end of buf.

   [addSubString buf ss] appends ss at the end of buf.

*)

