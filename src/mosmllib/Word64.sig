(* Word64 -- SML Basis Library *)
signature Word64 =
sig
type word

val wordSize   : int

val orb        : word * word -> word
val andb       : word * word -> word
val xorb       : word * word -> word
val notb       : word -> word
val ~          : word -> word

val <<         : word * word -> word
val >>         : word * word -> word
val ~>>        : word * word -> word

val +          : word * word -> word
val -          : word * word -> word
val *          : word * word -> word
val div        : word * word -> word
val mod        : word * word -> word

val ==         : word * word -> bool
val >          : word * word -> bool
val <          : word * word -> bool
val >=         : word * word -> bool
val <=         : word * word -> bool
val compare    : word * word -> order

val min        : word * word -> word
val max        : word * word -> word

val toString   : word -> string
val fromString : string -> word option
val scan       : StringCvt.radix
                -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
val fmt        : StringCvt.radix -> word -> string

val toInt      : word -> int
val toIntX     : word -> int            (* with sign extension *)
val fromInt    : int -> word

val toLarge    : word -> Word.word
val toLargeX   : word -> Word.word        (* with sign extension *)
val fromLarge  : Word.word -> word

val toLargeWord   : word -> Word.word
val toLargeWordX  : word -> Word.word        (* with sign extension *)
val fromLargeWord : Word.word -> word

val toLargeInt    : word -> int
val toLargeIntX   : word -> int         (* with sign extension *)
val fromLargeInt  : int -> word
end

(* [word] is the type of n-bit words, or n-bit unsigned
   integers. Unlike the built-in [word] type, the type [word] is
   guaranteed to be exactly 64-bit wide on all platforms. All
   arithmetic operations over [word] are taken modulo 2{^64}

   Performance notice: values of type [word] in this module occupy
   more memory space than values of built-in type [word], and
   arithmetic operations on [word64] are generally slower than those
   on [word].  Use [word64] only when the application requires exact
   64-bit arithmetic.

   Currently there is no lexical support for [word64] constants.

   [wordSize] is the value of n above.  For this module n is 64.

   [orb(w1, w2)] returns the bitwise `or' of w1 and w2.

   [andb(w1, w2)] returns the bitwise `and' of w1 and w2.

   [xorb(w1, w2)] returns the bitwise `exclusive or' or w1 and w2.

   [notb w] returns the bitwise negation (one's complement) of w.

   [~ w] returns the arithmetic negation (two's complement) of w.

   [<<(w, k)] returns the word resulting from shifting w left by k
   bits.  The bits shifted in are zero, so this is a logical shift.
   Consequently, the result is 0-bits when k >= wordSize.

   [>>(w, k)] returns the word resulting from shifting w right by k
   bits.  The bits shifted in are zero, so this is a logical shift.
   Consequently, the result is 0-bits when k >= wordSize.

   [~>>(w, k)] returns the word resulting from shifting w right by k
   bits.  The bits shifted in are replications of the left-most bit:
   the `sign bit', so this is an arithmetical shift.  Consequently,
   for k >= wordSize and wordToInt w >= 0 the result is all 0-bits, and
   for k >= wordSize and wordToInt w <  0 the result is all 1-bits.

   To make <<, >>, and ~>> infix, use the declaration
                          infix 5 << >> ~>>

   [+]
   [-]
   [*]
   [div]
   [mod] represent unsigned integer addition, subtraction,
   multiplication, division, and remainder, modulus 2 raised to the n'th
   power, where n=wordSize.  The operations (i div j) and (i mod j)
   raise Div when j=0.  Otherwise no exceptions are raised.

   [<]
   [<=]
   [>]
   [>=] compare words as unsigned integers.

   [compare(w1, w2)] returns LESS, EQUAL, or GREATER, according
   as w1 is less than, equal to, or greater than w2 (as unsigned integers).

   [min(w1, w2)] returns the smaller of w1 and w2 (as unsigned integers).

   [max(w1, w2)] returns the larger of w1 and w2 (as unsigned integers).

   [fmt radix w] returns a string representing w, in the radix (base)
   specified by radix.

     radix    description                     output format
     ------------------------------------------------------
      BIN     unsigned binary      (base  2)  [01]+
      OCT     unsigned octal       (base  8)  [0-7]+
      DEC     unsigned decimal     (base 10)  [0-9]+
      HEX     unsigned hexadecimal (base 16)  [0-9A-F]+

   [toString w] returns a string representing w in unsigned
   hexadecimal format.  Equivalent to (fmt HEX w).

   [fromString s] returns SOME(w) if a hexadecimal unsigned numeral
   can be scanned from a prefix of string s, ignoring any initial
   whitespace; returns NONE otherwise.  Raises Overflow if the scanned
   number cannot be represented as a word.  An unsigned hexadecimal
   numeral must have form, after possible initial whitespace:
        [0-9a-fA-F]+

   [scan radix getc charsrc] attempts to scan an unsigned numeral from
   the character source charsrc, using the accessor getc, and ignoring
   any initial whitespace.  The radix argument specifies the base of
   the numeral (BIN, OCT, DEC, HEX).  If successful, it returns
   SOME(w, rest) where w is the value of the numeral scanned, and rest
   is the unused part of the character source.  Raises Overflow if the
   scanned number cannot be represented as a word.  A numeral must
   have form, after possible initial whitespace:

     radix    input format
     -------------------------------------
      BIN     (0w)?[0-1]+
      OCT     (0w)?[0-7]+
      DEC     (0w)?[0-9]+
      HEX     (0wx|0wX|0x|0X)?[0-9a-fA-F]+

   [toInt w] returns the (non-negative) default size int represented
   by bit-pattern w.  Raises Overflow in case w is not representable
   as an integer.

   [toIntX w] returns the (signed) default size int represented by
   twos's complement bit-pattern w.

   [fromInt i] returns the word (bit-pattern) representing integer i.

   [toLargeInt w] returns the (non-negative) largest size int
   represented by bit-pattern w.  Raises Overflow in case w is not
   representable as an integer.

   [toLargeIntX w] returns the (signed) largest size int represented
   by two's complement bit-pattern w.

   [fromLargeInt i] returns the word representing integer i.

   [toLarge w] returns w.
   [toLargeX w] returns w.
   [fromLarge w] returns w.

   [toLargeWord w] returns w (deprecated).
   [toLargeWordX w] returns w (deprecated).
   [fromLargeWord w] returns w (deprecated).
*)
