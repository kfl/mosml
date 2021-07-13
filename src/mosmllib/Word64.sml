(* Word64 -- SML Basis Library *)
(* Stop gap implementation by KFL, 2021-07-05, based on Word.mlp by PS *)

(* This unit relies on two's complement representation *)
structure Word64 :> Word64 =
struct
prim_type word;

val wordSize = 64;

local
    prim_val equal_     : word -> word -> bool = 2 "boxed_uint64_equal";
    prim_val less_      : word -> word -> bool = 2 "boxed_uint64_less";
    prim_val fromWord_  : Word.word -> word = 1 "boxed_uint64_fromword";
    prim_val toWord_    : word -> Word.word = 1 "boxed_uint64_toword";

    prim_val add_       : word -> word -> word = 2 "boxed_uint64_add";
    prim_val sub_       : word -> word -> word = 2 "boxed_uint64_sub";
    prim_val mul_       : word -> word -> word = 2 "boxed_uint64_mul";
    prim_val div_       : word -> word -> word = 2 "boxed_uint64_div";
    prim_val mod_       : word -> word -> word = 2 "boxed_uint64_mod";
    prim_val orb_       : word -> word -> word = 2 "boxed_uint64_orb";
    prim_val andb_      : word -> word -> word = 2 "boxed_uint64_andb";
    prim_val xorb_      : word -> word -> word = 2 "boxed_uint64_xorb";
    prim_val lshift_    : word -> word -> word = 2 "boxed_uint64_lshift";
    prim_val rshiftsig_ : word -> word -> word = 2 "boxed_uint64_rshift_signed";
    prim_val rshiftuns_ : word -> word -> word = 2 "boxed_uint64_rshift_unsigned";

in
    (* NOT the SML BL conversion funs *)
    prim_val toInt   : word -> int = 1 "boxed_uint64_toword";
    prim_val toIntX  : word -> int = 1 "boxed_uint64_toword";
    prim_val fromInt : int -> word = 1 "boxed_uint64_fromword";

    prim_val toLargeInt   : word -> int = 1 "boxed_uint64_toword";
    prim_val toLargeIntX  : word -> int = 1 "boxed_uint64_toword";
    prim_val fromLargeInt : int -> word = 1 "boxed_uint64_fromword";

    prim_val toLargeWord   : word -> Word.word = 1 "boxed_uint64_toword";
    prim_val toLargeWordX  : word -> Word.word = 1 "boxed_uint64_toword";
    prim_val fromLargeWord : Word.word -> word = 1 "boxed_uint64_fromword";

    prim_val toLarge   : word -> Word.word = 1 "boxed_uint64_toword"
    prim_val toLargeX  : word -> Word.word = 1 "boxed_uint64_toword"
    prim_val fromLarge : Word.word -> word = 1 "boxed_uint64_fromword";

    val ZERO = fromInt 0

    fun orb (x, y)  = orb_ x y;
    fun andb (x, y) = andb_ x y;
    fun xorb (x, y) = xorb_ x y;
    fun notb x      = xorb_ x (fromInt ~1);

    fun << (w, k) =
        if toInt k >= 64 orelse toInt k < 0 then ZERO
        else lshift_ w k;

    fun >> (w, k) =
        if toInt k >= 64 orelse toInt k < 0 then ZERO
        else rshiftuns_ w k;

    fun ~>> (w, k) =
        if toInt k >= 64 orelse toInt k < 0 then
            if toInt w >= 0 then  (* msbit = 0 *)
                ZERO
            else      (* msbit = 1 *)
                fromInt ~1
        else
            rshiftsig_ w k;

    val op *    : word * word -> word = fn(x, y) => mul_ x y;
    val op +    : word * word -> word = fn(x, y) => add_ x y;
    val op -    : word * word -> word = fn(x, y) => sub_ x y;
    val op div  : word * word -> word =
     fn(x, y) => if equal_ y ZERO then raise Div else div_ x y;
    val op mod  : word * word -> word =
     fn(x, y) => if equal_ y ZERO then raise Div else mod_ x y;

    val ~ = fn w => ZERO - w

    local
      open StringCvt
      fun skipWSget getc source = getc (dropl Char.isSpace getc source)

      (* Below, 48 = Char.ord #"0" and 55 = Char.ord #"A" - 10. *)
      fun decval c = fromInt (Char.ord c) - fromInt 48;
      fun hexval c =
          if #"0" <= c andalso c <= #"9" then
              fromInt (Char.ord c) - fromInt 48
          else
              (fromInt (Char.ord c) - fromInt 55) mod (fromInt 32);

      fun prhex i =
          if toInt i < 10 then Char.chr(toInt (i + fromInt 48))
          else Char.chr(toInt (i + fromInt 55));

      fun conv radix i =
          let fun h n res =
                  if equal_ n ZERO then res
                  else h (n div radix) (prhex (n mod radix) :: res)
              fun tostr n = h (n div radix) [prhex (n mod radix)]
          in String.implode (tostr i) end

    in
    fun scan radix getc source =
        let open StringCvt
            val source = skipWS getc source
            val (isDigit, factor) =
                case radix of
                    BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
                  | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
                  | DEC => (Char.isDigit,                          10)
                  | HEX => (Char.isHexDigit,                       16)
            fun dig1 NONE              = NONE
              | dig1 (SOME (c1, src1)) =
                let fun digr res src =
                        case getc src of
                            NONE           => SOME (res, src)
                          | SOME (c, rest) =>
                            if isDigit c then
                                digr (fromInt factor * res + hexval c)
                                     rest
                            else SOME (res, src)
                in
                    if isDigit c1 then digr (hexval c1) src1
                    else NONE
                end
            fun getdigs after0 src =
                case dig1 (getc src) of
                    NONE => SOME(ZERO, after0)
                  | res  => res
            fun hexprefix after0 src =
                if radix <> HEX then getdigs after0 src
                else
                    case getc src of
                        SOME(#"x", rest) => getdigs after0 rest
                      | SOME(#"X", rest) => getdigs after0 rest
                      | SOME _           => getdigs after0 src
                      | NONE => SOME(ZERO, after0)
        in
            case getc source of
                SOME(#"0", after0) =>
                (case getc after0 of
                     SOME(#"w", src2) => hexprefix after0 src2
                   | SOME _           => hexprefix after0 after0
                   | NONE             => SOME(ZERO, after0))
              | SOME _ => dig1 (getc source)
              | NONE   => NONE
        end;

    fun fmt BIN = conv (fromInt  2)
      | fmt OCT = conv (fromInt  8)
      | fmt DEC = conv (fromInt 10)
      | fmt HEX = conv (fromInt 16)

    fun toString w   = conv (fromInt 16) w
    fun fromString s = scanString (scan HEX) s
    end (* local for string functions *)

    val == : word * word -> bool = fn(x, y) => equal_ x y;
    val op <    : word * word -> bool = fn(x, y) => less_ x y;
    val op <=   : word * word -> bool = fn(x, y) => less_ x y orelse equal_ x y;
    val op >    : word * word -> bool = fn(x, y) => y < x;
    val op >=   : word * word -> bool = fn(x, y) => y <= x;
    fun min(w1 : word, w2) = if w1 > w2 then w2 else w1;
    fun max(w1 : word, w2) = if w1 > w2 then w1 else w2;
    fun compare (x, y: word) =
        if less_ x y then LESS else if less_ y x then GREATER else EQUAL;

    fun toInt w =
        if w > fromWord_ 0wx3FFFFFFFFFFFFFFF then raise Overflow
        else toIntX w

end
end
