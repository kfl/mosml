(* StringCvt -- SML Basis Library *)

datatype radix = BIN | OCT | DEC | HEX

datatype realfmt = 
    SCI of int option   (* scientific,  arg = # dec. digits, dflt=6 *)
  | FIX of int option   (* fixed-point, arg = # dec. digits, dflt=6 *)
  | GEN of int option   (* auto choice of the above,                *)
                        (* arg = # significant digits, dflt=12      *)

type cs                 (* character source state *)

type ('a, 'b) reader = 'b -> ('a * 'b) option

val scanString : ((char, cs) reader -> ('a, cs) reader) -> string -> 'a option

val splitl     : (char -> bool) -> (char, 'a) reader -> 'a -> string * 'a
val takel      : (char -> bool) -> (char, 'a) reader -> 'a -> string 
val dropl      : (char -> bool) -> (char, 'a) reader -> 'a -> 'a 
val skipWS     : (char, 'a) reader -> 'a -> 'a 

val padLeft    : char -> int -> string -> string
val padRight   : char -> int -> string -> string

(* 
   This structure presents tools for scanning strings and values from
   functional character streams, and for simple formatting.

   [('elm, 'src) reader] is the type of source readers for reading a
   sequence of 'elm values from a source of type 'src.  For instance, 
   a character source reader
        getc : (char, cs) reader 
   is used for obtaining characters from a functional character source
   src of type cs, one at a time. It should hold that

        getc src = SOME(c, src')        if the next character in src 
                                        is c, and src' is the rest of src;
                 = NONE                 if src contains no characters

   A character source scanner takes a character source reader getc as
   argument and uses it to scan a data value from the character
   source.

   [scanString scan s] turns the string s into a character source and
   applies the scanner `scan' to that source.

   [splitl p getc src] returns (pref, suff) where pref is the
   longest prefix (left substring) of src all of whose characters
   satisfy p, and suff is the remainder of src.  That is, the first
   character retrievable from suff, if any, is the leftmost character
   not satisfying p.  Does not skip leading whitespace.

   [takel p getc src] returns the longest prefix (left substring) of
   src all of whose characters satisfy predicate p.  That is, if the
   left-most character does not satisfy p, the result is the empty
   string.  Does not skip leading whitespace.  It holds that
        takel p getc src = #1 (splitl p getc src)

   [dropl p getc src] drops the longest prefix (left substring) of
   src all of whose characters satisfy predicate p.  If all characters
   do, it returns the empty source.  It holds that
        dropl p getc src = #2 (splitl p getc src)

   [skipWS getc src] drops any leading whitespace from src.
   Equivalent to dropl Char.isSpace.

   [padLeft c n s] returns the string s if size s >= n, otherwise pads
   s with (n - size s) copies of the character c on the left.  
   In other words, right-justifies s in a field n characters wide.

   [padRight c n s] returns the string s if size s >= n, otherwise pads
   s with (n - size s) copies of the character c on the right.
   In other words, left-justifies s in a field n characters wide.
*)
