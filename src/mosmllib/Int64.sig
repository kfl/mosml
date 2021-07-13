(* Int64 -- SML Basis Library *)
signature Int64 =
sig
type int

val precision : Int.int option
val minInt    : int option
val maxInt    : int option

val ~         : int -> int              (* Overflow      *)
val *         : int * int -> int        (* Overflow      *)
val div       : int * int -> int        (* Div, Overflow *)
val mod       : int * int -> int        (* Div           *)
val quot      : int * int -> int        (* Div, Overflow *)
val rem       : int * int -> int        (* Div           *)
val +         : int * int -> int        (* Overflow      *)
val -         : int * int -> int        (* Overflow      *)
val ==        : int * int -> bool
val >         : int * int -> bool
val >=        : int * int -> bool
val <         : int * int -> bool
val <=        : int * int -> bool
val abs       : int -> int              (* Overflow      *)
val min       : int * int -> int
val max       : int * int -> int

val sign      : int -> Int.int
val sameSign  : int * int -> bool
val compare   : int * int -> order

val toInt     : int -> Int.int
val fromInt   : Int.int -> int
val toLarge   : int -> int
val fromLarge : int -> int

val scan      : StringCvt.radix 
                -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
val fmt       : StringCvt.radix -> int -> string

val toString  : int -> string
val fromString : string -> int option   (* Overflow      *)

end

(* 

   [int] is the type of n-bit signed integers. Unlike the standard
   [Int.int] type, the type [int] in this module (also known as
   [int64]) is guaranteed to be exactly 64-bit wide on all platforms.

   Performance notice: values of type [int64] in this module occupy
   more memory space than values of built-in type [Int.int], and
   arithmetic operations on [int64] are generally slower than those
   on [Int.int].  Use [int64] only when the application requires exact
   64-bit arithmetic.

   Divergences from the Basis Library specification:

      - No lexical support for [int64] constants, thus no overloading
        of constants.

      - Arithmetic operators (and others) are not overloaded to
        support [int64].

      - The conversion functions in the built-in Int module does not
        know about this module.

      - [int64] is not an eqtype.

   [precision] is SOME n, where n is the number of significant bits in an
   integer. In this module n is 64.

   [minInt] is SOME n, where n is the most negative integer.

   [maxInt] is SOME n, where n is the most positive integer.

   [~]
   [*]
   [+]
   [-] are the usual operations on integers.  They raise Overflow if
   the result is not representable as an integer.

   [abs] returns the absolute value of its argument.  Raises Overflow
   if applied to the most negative integer.

   [div] is integer division, rounding towards minus infinity.
   Evaluating i div 0 raises Div.  Evaluating i div ~1 raises
   Overflow if i is the most negative integer.

   [mod] is the remainder for div.  If q = i div d and r = i mod d then
   it holds that qd + r = i, where either 0 <= r < d or d < r <= 0.  
   Evaluating i mod 0 raises Div, whereas i mod ~1 = 0, for all i.

   [quot] is integer division, rounding towards zero.  Evaluating
   quot(i, 0) raises Div.  Evaluating quot(i, ~1) raises Overflow if i
   is the most negative integer.

   [rem(i, d)] is the remainder for quot.  That is, if q = quot(i, d)
   and r = rem(i, d) then d * q + r = i, where r is zero or has the
   same sign as i.  If made infix, the recommended fixity for quot and
   rem is
        infix 7 quot rem

   [min(x, y)] is the smaller of x and y.

   [max(x, y)] is the larger of x and y.

   [sign x] is ~1, 0, or 1, according as x is negative, zero, or positive.

   [<]
   [<=]
   [>]
   [>=] are the usual comparisons on integers.

   [compare(x, y)] returns LESS, EQUAL, or GREATER, according 
   as x is less than, equal to, or greater than y.

   [sameSign(x, y)] is true iff sign x = sign y.

   [toInt x] is x (because this is the default int type in Moscow ML).

   [fromInt x] is x (because this is the default int type in Moscow ML).

   [toLarge x] is x (because this is the largest int type in Moscow ML).

   [fromLarge x] is x (because this is the largest int type in Moscow ML).

   [fmt radix i] returns a string representing i, in the radix (base)
   specified by radix.

     radix    description                     output format  
     ------------------------------------------------------
      BIN     signed binary      (base  2)    ~?[01]+
      OCT     signed octal       (base  8)    ~?[0-7]+
      DEC     signed decimal     (base 10)    ~?[0-9]+
      HEX     signed hexadecimal (base 16)    ~?[0-9A-F]+

   [toString i] returns a string representing i in signed decimal format.
   Equivalent to (fmt DEC i).
   
   [fromString s] returns SOME(i) if a decimal integer numeral can be
   scanned from a prefix of string s, ignoring any initial whitespace;
   returns NONE otherwise.  A decimal integer numeral must have form,
   after possible initial whitespace: 
        [+~-]?[0-9]+

   [scan radix getc charsrc] attempts to scan an integer numeral
   from the character source charsrc, using the accessor getc, and
   ignoring any initial whitespace.  The radix argument specifies the base
   of the numeral (BIN, OCT, DEC, HEX).  If successful, it returns
   SOME(i, rest) where i is the value of the number scanned, and rest
   is the unused part of the character source.  A numeral must have
   form, after possible initial whitespace:

     radix    input format 
     ---------------------------
      BIN     [+~-]?[0-1]+
      OCT     [+~-]?[0-7]+
      DEC     [+~-]?[0-9]+
      HEX     [+~-]?[0-9a-fA-F]+
*)
