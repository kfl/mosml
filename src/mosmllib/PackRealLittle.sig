signature PackRealLittle =
sig
    type real = real
    val bytesPerElem : int
    val isBigEndian : bool
    val toBytes   : real -> Word8Vector.vector
    val fromBytes : Word8Vector.vector -> real
    val subVec : Word8Vector.vector * int -> real
    val subArr : Word8Array.array * int -> real
    val update : Word8Array.array * int * real -> unit
end

(*
   [bytesPerElem] is the number of bytes per element, sufficient to
   store a value of type real.

   [isBigEndian] is true if the structure implements a big-endian
   (most significant byte first) view of the data.

   [toBytes r] returns an bytesPerElem-element vector of Word8.word, which
   contains the real number in the IEEE 754 floating-point `double
   format' bit layout stored in the byte order specified by isBigEndian.

   [fromBytes v] accepts an vector of Word8.word, and returns the real
   number obtained by taking v to be an IEEE 754 floating-point
   `double format' number stored in the byte order specified by
   isBigEndian. Raises the Subscript exception if the argument vector
   does not have length at least bytesPerElem; otherwise the first
   bytesPerElem bytes are used.

   [subVec (vec, i)] extract the slice
         slice(vec, bytesPerElem*i, SOME bytesPerElem)
   and convert it into a real value according to the endianness of the
   structure. Raise the Subscript exception if i < 0 or if vec does
   not have enough elements.

   [subArr (arr, i)] extract the
         slice(arr, bytesPerElem*i, SOME bytesPerElem)
   and convert it into a real value according to the endianness of the
   structure. Raise the Subscript exception if i < 0 or if arr does
   not have enough elements.


   [update (arr, i, r)] stores r into the bytes bytesPerElem*i through
   bytesPerElem*(i+1)-1 of the array arr, according to the structure's
   endianness. Raises the Subscript exception if i < 0 or if
   length arr < bytesPerElem * (i + 1).
*)
