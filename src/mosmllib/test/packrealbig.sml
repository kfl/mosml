use "auxil.sml";
load "PackRealBig";
local
    structure P = PackRealBig
    fun hex w = StringCvt.padLeft #"0" 2 (Word8.fmt StringCvt.HEX w)
    fun pow n i =
        let fun loop 0 acc = acc
              | loop i acc = loop (i-1) (n * acc)
        in  loop i 1
        end
in

fun roundtrip r =
    let val packed = P.toBytes r
        val endian = if P.isBigEndian then "(big endian)\n"
                     else "(little endian)\n"
    in  app print ["  ", Real.fmt (StringCvt.FIX NONE) r, " is enconded as "]
      ; Word8Vector.app (fn w => app print [hex w, " "]) packed
      ; print endian
      ; Real.== (r, P.fromBytes packed)
    end

val test_roundtrip =
    check'(fn _ =>
              List.all roundtrip
                       [~420.0,
                        ~1.1,
                        ~0.123456789,
                        ~0.0,
                        0.0,
                        1.0,
                        1.1,
                        2.0,
                        420E6,
                        real (valOf Int.maxInt),
                        real (pow 2 53 - 1),
                        real (pow 2 53),
                        real (pow 2 53 + 1),
                        Math.pi,
                        Math.e]);

val encoded =
    [0wx40, 0wx09, 0wx21, 0wxFB, 0wx54, 0wx44, 0wx2D, 0wx18,
     0wx40, 0wx09, 0wx21, 0wxFB, 0wx54, 0wx44, 0wx2D, 0wx18]

val test_subvec =
    let val encoded = Word8Vector.fromList encoded
    in  check'(fn _ => Real.== (P.subVec(encoded, 0),
                                P.subVec(encoded, 1)))
    end;

val test_subarr =
    let val encoded = Word8Array.fromList encoded
    in  check'(fn _ => Real.== (P.subArr(encoded, 0),
                                P.subArr(encoded, 1)))
    end;

val test_update0 =
    let val encoded = Word8Array.fromList encoded
    in  check'(fn _ =>
                  ( P.update(encoded, 0, 0.0)
                  ; Real.== (P.subArr(encoded, 0),
                             0.0)
                  ))
    end;

val test_update1 =
    let val encoded = Word8Array.fromList encoded
    in  check'(fn _ =>
                  ( P.update(encoded, 1, 1.0)
                  ; Real.== (P.subArr(encoded, 1),
                             1.0)
                  ))
    end;
end
