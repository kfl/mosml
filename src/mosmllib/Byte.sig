(* Byte -- SML Basis Library *)

val byteToChar      : Word8.word -> Char.char
val charToByte      : Char.char -> Word8.word
val bytesToString   : Word8Vector.vector -> String.string
val stringToBytes   : String.string -> Word8Vector.vector

val unpackStringVec : Word8Vector.vector * int * int option -> string
val unpackString    : Word8Array.array * int * int option -> string
val packString      : Word8Array.array * int * Substring.substring -> unit

(* 
   Conversions between bytes and characters, and between byte vectors 
   and strings (character vectors).  

   [byteToChar w] is the character corresponding to the byte w.

   [charToByte c] is the byte corresponding to character c.

   [bytesToString v] is the string whose character codes are the bytes 
   from vector v.

   [stringToBytes s] is the byte vector of character codes of the string s.

   In Moscow ML, all the above operations take constant time.  That
   is, no copying is done.

   [unpackStringVec (v, i, NONE)] is the string whose character codes are
   the bytes of v[i..length v-1].  Raises Subscript if i<0 or i>length v.
   
   [unpackStringVec (v, i, SOME n)] is the string whose character codes are
   the bytes of v[i..i+n-1].  Raises Subscript if i<0 or n<0 or i+n>length v.

   [unpackString (a, i, NONE)] is the string whose character codes are
   the bytes of a[i..length a-1].  Raises Subscript if i<0 or i>length a.
   
   [unpackString (a, i, SOME n)] is the string whose character codes are
   the bytes of a[i..i+n-1].  Raises Subscript if i<0 or n<0 or i+n>length a.

   [packString (a, i, ss)] copies the character codes of substring ss into
   the subarray a[i..i+n-1] where n = Substring.size ss.  Raises Subscript 
   if i<0 or i+n > length a.
*)
