structure PackRealBig :> PackRealBig =
struct

(* Copied from Mosml.mlp, these make big-endian encodings *)
prim_val doubleVec : real -> Word8Vector.vector = 1 "doubletow8vec"
prim_val vecDouble_ : Word8Vector.vector -> real = 1 "w8vectodouble"


type real = real
val bytesPerElem = 8
val isBigEndian = true
val toBytes = doubleVec

fun fromBytes vec =
    if Word8Vector.length vec = 8 then
	      vecDouble_ vec
    else
        (* Raises Subscript if bytesPerElem > Vector.length vec *)
        vecDouble_ (Word8VectorSlice.vector((Word8VectorSlice.slice(vec, 0, SOME bytesPerElem))))

fun subVec (vec, i) =
    fromBytes (Word8VectorSlice.vector((Word8VectorSlice.slice(vec, i*bytesPerElem, SOME bytesPerElem))))

fun subArr (arr, i) =
    fromBytes (Word8ArraySlice.vector((Word8ArraySlice.slice(arr, i*bytesPerElem, SOME bytesPerElem))))

fun update (arr, i, r) =
    Word8Array.copyVec {src=toBytes r, dst=arr, di=i*bytesPerElem}

end
