(* Byte -- 1995-11-08, 1996-04-09 *)

prim_val byteToChar    : Word8.word -> Char.char = 1 "identity"
prim_val charToByte    : Char.char -> Word8.word = 1 "identity"
prim_val bytesToString : Word8Vector.vector -> String.string = 1 "identity"
prim_val stringToBytes : String.string -> Word8Vector.vector = 1 "identity"

fun unpackStringVec arg = bytesToString (Word8Vector.extract arg)
fun unpackString arg = bytesToString (Word8Array.extract arg)

fun packString (ss, a, i) =
    let val (s, si, n) = Substring.base ss
    in  
        Word8Array.copyVec {src = stringToBytes s, si = si, 
			    len = SOME n, dst = a, di = i} 
    end
