(* String -- SML Basis Library *)

local 
    type char = Char.char
in
    type string = string
    val maxSize   : int
    val size      : string -> int
    val sub       : string * int -> char
    val substring : string * int * int -> string
    val extract   : string * int * int option -> string
    val concat    : string list -> string
    val ^         : string * string -> string
    val str       : char -> string
    val implode   : char list -> string 
    val explode   : string -> char list

    val map       : (char -> char) -> string -> string 
    val translate : (char -> string) -> string -> string
    val tokens    : (char -> bool) -> string -> string list
    val fields    : (char -> bool) -> string -> string list
    val isPrefix  : string -> string -> bool

    val compare   : string * string -> order
    val collate   : (char * char -> order) -> string * string -> order

    val fromString  : string -> string option     (* ML escape sequences *)
    val toString    : string -> string            (* ML escape sequences *)
    val fromCString : string -> string option     (* C escape sequences *)
    val toCString   : string -> string            (* C escape sequences *)

    val <  : string * string -> bool
    val <= : string * string -> bool
    val >  : string * string -> bool
    val >= : string * string -> bool
end

(* 
   [string] is the type of immutable strings of characters, with
   constant-time indexing.

   [maxSize] is the maximal number of characters in a string.

   [size s] is the number of characters in string s.

   [sub(s, i)] is the i'th character of s, counting from zero.  
   Raises Subscript if i<0 or i>=size s.

   [substring(s, i, n)] is the string s[i..i+n-1].  Raises Subscript
   if i<0 or n<0 or i+n>size s.  Equivalent to extract(s, i, SOME n).

   [extract (s, i, NONE)] is the string s[i..size s-1].
   Raises Subscript if i<0 or i>size s. 

   [extract (s, i, SOME n)] is the string s[i..i+n-1].
   Raises Subscript if i<0 or n<0 or i+n>size s. 

   [concat ss] is the concatenation of all the strings in ss.
   Raises Size if the sum of their sizes is greater than maxSize.

   [s1 ^ s2] is the concatenation of strings s1 and s2.

   [str c] is the string of size one which contains the character c.

   [implode cs] is the string containing the characters in the list cs.
   Equivalent to  concat (List.map str cs).

   [explode s] is the list of characters in the string s.

   [map f s] applies f to every character of s, from left to right,
   and returns the string consisting of the resulting characters.
   Equivalent to  CharVector.map f s   
          and to  implode (List.map f (explode s)).

   [translate f s] applies f to every character of s, from left to
   right, and returns the concatenation of the resulting strings.
   Raises Size if the sum of their sizes is greater than maxSize.
   Equivalent to concat (List.map f (explode s)).

   [tokens p s] returns the list of tokens in s, from left to right, 
   where a token is a non-empty maximal substring of s not containing 
   any delimiter, and a delimiter is a character satisfying p.

   [fields p s] returns the list of fields in s, from left to right, 
   where a field is a (possibly empty) maximal substring of s not 
   containing any delimiter, and a delimiter is a character satisfying p.

   Two tokens may be separated by more than one delimiter, whereas two
   fields are separated by exactly one delimiter.  If the only delimiter 
   is the character #"|", then
        "abc||def" contains two tokens:   "abc" and "def"
        "abc||def" contains three fields: "abc" and "" and "def"

   [isPrefix s1 s2] is true if s1 is a prefix of s2.  
   That is, if there exists a string t such that s1 ^ t = s2.

   [fromString s] scans the string s as an ML source program string,
   converting escape sequences into the appropriate characters.  Does
   not skip leading whitespace.

   [toString s] returns a string corresponding to s, with
   non-printable characters replaced by ML escape sequences.
   Equivalent to String.translate Char.toString.

   [fromCString s] scans the string s as a C source program string,
   converting escape sequences into the appropriate characters.  Does
   not skip leading whitespace.

   [toCString s] returns a string corresponding to s, with
   non-printable characters replaced by C escape sequences.
   Equivalent to String.translate Char.toCString.

   [compare (s1, s2)] does lexicographic comparison, using the
   standard ordering Char.compare on the characters.  Returns LESS,
   EQUAL, or GREATER, according as s1 is less than, equal to, or
   greater than s2.

   [collate cmp (s1, s2)] performs lexicographic comparison, using the 
   given ordering cmp on characters.  

   [<]
   [<=]
   [>]
   [>=] compare strings lexicographically, using the representation 
   ordering on characters.
*)
