(* Char -- SML Basis Library *)

type char = char

val minChar : char
val maxChar : char
val maxOrd  : int       

val chr     : int  -> char       (* May raise Chr *)
val ord     : char -> int
val succ    : char -> char       (* May raise Chr *)
val pred    : char -> char       (* May raise Chr *)

val isLower     : char -> bool   (* contains "abcdefghijklmnopqrstuvwxyz"  *)
val isUpper     : char -> bool   (* contains "ABCDEFGHIJKLMNOPQRSTUVWXYZ"  *)
val isDigit     : char -> bool   (* contains "0123456789"                  *)
val isAlpha     : char -> bool   (* isUpper orelse isLower                 *)
val isHexDigit  : char -> bool   (* isDigit orelse contains "abcdefABCDEF" *)
val isAlphaNum  : char -> bool   (* isAlpha orelse isDigit                 *)
val isPrint     : char -> bool   (* any printable character (incl. #" ")   *)
val isSpace     : char -> bool   (* contains " \t\r\n\v\f"                 *)
val isPunct     : char -> bool   (* printable, not space or alphanumeric   *) 
val isGraph     : char -> bool   (* (not isSpace) andalso isPrint          *)
val isAscii     : char -> bool   (* ord c < 128                            *)
val isCntrl     : char -> bool   (* control character                      *)

val toLower     : char -> char
val toUpper     : char -> char

val fromString  : string -> char option       (* ML escape sequences *)
val toString    : char -> string              (* ML escape sequences *)

val fromCString : string -> char option       (* C escape sequences  *)
val toCString   : char -> string              (* C escape sequences  *)

val contains    : string -> char -> bool
val notContains : string -> char -> bool

val <       : char * char -> bool
val <=      : char * char -> bool
val >       : char * char -> bool
val >=      : char * char -> bool
val compare : char * char -> order

(* 
   [char] is the type of characters.  

   [minChar] is the least character in the ordering <.

   [maxChar] is the greatest character in the ordering <.

   [maxOrd] is the greatest character code; equals ord(maxChar).

   [chr i] returns the character whose code is i.  Raises Chr if
   i<0 or i>maxOrd.

   [ord c] returns the code of character c.

   [succ c] returns the character immediately following c, or raises
   Chr if c = maxChar.

   [pred c] returns the character immediately preceding c, or raises
   Chr if c = minChar.

   [isLower c] returns true if c is a lowercase letter (a to z).

   [isUpper c] returns true if c is a uppercase letter (A to Z).

   [isDigit c] returns true if c is a decimal digit (0 to 9).

   [isAlpha c] returns true if c is a letter (lowercase or uppercase).

   [isHexDigit c] returns true if c is a hexadecimal digit (0 to 9 or 
   a to f or A to F).

   [isAlphaNum c] returns true if c is alphanumeric (a letter or a
   decimal digit).

   [isPrint c] returns true if c is a printable character (space or visible)

   [isSpace c] returns true if c is a whitespace character (blank, newline,
   tab, vertical tab, new page).

   [isGraph c] returns true if c is a graphical character, that is,
   it is printable and not a whitespace character.

   [isPunct c] returns true if c is a punctuation character, that is, 
   graphical but not alphanumeric.

   [isCntrl c] returns true if c is a control character, that is, if
   not (isPrint c).

   [isAscii c] returns true if 0 <= ord c <= 127.

   [toLower c] returns the lowercase letter corresponding to c,
   if c is a letter (a to z or A to Z); otherwise returns c.

   [toUpper c] returns the uppercase letter corresponding to c,
   if c is a letter (a to z or A to Z); otherwise returns c.

   [contains s c] returns true if character c occurs in the string s;
   false otherwise.  The function, when applied to s, builds a table
   and returns a function which uses table lookup to decide whether a
   given character is in the string or not.  Hence it is relatively
   expensive to compute  val p = contains s  but very fast to compute 
   p(c) for any given character.

   [notContains s c] returns true if character c does not occur in the
   string s; false otherwise.  Works by construction of a lookup table
   in the same way as the above function.

   [fromString s] attempts to scan a character or ML escape sequence
   from the string s.  Does not skip leading whitespace.  For
   instance, fromString "\\065" equals #"A".

   [toString c] returns a string consisting of the character c, if c
   is printable, else an ML escape sequence corresponding to c.  A
   printable character is mapped to a one-character string; bell,
   backspace, tab, newline, vertical tab, form feed, and carriage
   return are mapped to the two-character strings "\\a", "\\b", "\\t",
   "\\n", "\\v", "\\f", and "\\r"; other characters with code less
   than 32 are mapped to three-character strings of the form "\\^Z",
   and characters with codes 127 through 255 are mapped to
   four-character strings of the form "\\ddd", where ddd are three decimal 
   digits representing the character code.  For instance,
             toString #"A"      equals "A" 
             toString #"\\"     equals "\\\\" 
             toString #"\""     equals "\\\"" 
             toString (chr   0) equals "\\^@"
             toString (chr   1) equals "\\^A"
             toString (chr   6) equals "\\^F"
             toString (chr   7) equals "\\a"
             toString (chr   8) equals "\\b"
             toString (chr   9) equals "\\t"
             toString (chr  10) equals "\\n"
             toString (chr  11) equals "\\v"
             toString (chr  12) equals "\\f"
             toString (chr  13) equals "\\r"
             toString (chr  14) equals "\\^N"
             toString (chr 127) equals "\\127"
             toString (chr 128) equals "\\128"

   [fromCString s] attempts to scan a character or C escape sequence
   from the string s.  Does not skip leading whitespace.  For
   instance, fromString "\\065" equals #"A".

   [toCString c] returns a string consisting of the character c, if c
   is printable, else an C escape sequence corresponding to c.  A
   printable character is mapped to a one-character string; bell,
   backspace, tab, newline, vertical tab, form feed, and carriage
   return are mapped to the two-character strings "\\a", "\\b", "\\t",
   "\\n", "\\v", "\\f", and "\\r"; other characters are mapped to 
   four-character strings of the form "\\ooo", where ooo are three 
   octal digits representing the character code.  For instance,
             toString #"A"      equals "A" 
             toString #"A"      equals "A" 
             toString #"\\"     equals "\\\\" 
             toString #"\""     equals "\\\"" 
             toString (chr   0) equals "\\000"
             toString (chr   1) equals "\\001"
             toString (chr   6) equals "\\006"
             toString (chr   7) equals "\\a"
             toString (chr   8) equals "\\b"
             toString (chr   9) equals "\\t"
             toString (chr  10) equals "\\n"
             toString (chr  11) equals "\\v"
             toString (chr  12) equals "\\f"
             toString (chr  13) equals "\\r"
             toString (chr  14) equals "\\016"
             toString (chr 127) equals "\\177"
             toString (chr 128) equals "\\200"

   [<] 
   [<=]
   [>]
   [>=] compares character codes.  For instance, c1 < c2 returns true 
   if ord(c1) < ord(c2), and similarly for <=, >, >=.  

   [compare(c1, c2)] returns LESS, EQUAL, or GREATER, according as c1 is
   precedes, equals, or follows c2 in the ordering Char.< .
*)
