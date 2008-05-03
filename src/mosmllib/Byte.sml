(* Byte -- 1995-11-08, 1996-04-09, 2000-10-26 *)

prim_val byteToChar    : Word8.word -> Char.char = 1 "identity"
prim_val charToByte    : Char.char -> Word8.word = 1 "identity"
prim_val bytesToString : Word8Vector.vector -> String.string = 1 "identity"
prim_val stringToBytes : String.string -> Word8Vector.vector = 1 "identity"

fun unpackStringVec arg = 
    bytesToString (Word8VectorSlice.vector arg)

fun unpackString arg = 
    bytesToString (Word8ArraySlice.vector arg)

fun packString (a, i, ss) =
    let val (s, si, n) = Substring.base ss
	val src = Word8VectorSlice.slice(stringToBytes s, si, SOME n)
    in  
        Word8ArraySlice.copyVec {src = src, dst = a, di = i} 
    end
