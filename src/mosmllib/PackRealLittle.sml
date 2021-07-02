structure PackRealLittle :> PackRealLittle =
struct

type real = real
val bytesPerElem = 8
val isBigEndian = false

fun reverse_gen length sub vec =
    let val len = length vec
    in  Word8Vector.tabulate(len, fn i => sub(vec, len - 1 - i))
    end

fun reverse vec = reverse_gen Word8Vector.length Word8Vector.sub vec

structure Big = PackRealBig

fun toBytes r = reverse(Big.toBytes r)
fun fromBytes vec = Big.fromBytes(reverse vec)
fun subVec (vec, i) =
    Big.fromBytes(
        reverse_gen Word8VectorSlice.length Word8VectorSlice.sub (
            Word8VectorSlice.slice(vec, i*bytesPerElem, SOME bytesPerElem)))

fun subArr(vec, i) =
    Big.fromBytes(
        reverse_gen Word8ArraySlice.length Word8ArraySlice.sub (
            Word8ArraySlice.slice(vec, i*bytesPerElem, SOME bytesPerElem)))

fun update(arr, i, r) =
    Word8Array.copyVec {src=toBytes r, dst=arr, di=i*bytesPerElem}

end
