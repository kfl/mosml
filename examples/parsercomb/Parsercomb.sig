(* Parsercomb -- parser combinators *)

signature Parsercomb =
sig
    type 'elm stream
    type ('elm, 'res) parser = 'elm stream -> ('res * 'elm stream) option
       
    exception Parse of string

    val stream     : ('src -> ('elm * 'src) option) -> 'src -> 'elm stream

    val --         : ('a, 'b) parser * ('a, 'c) parser -> ('a, 'b * 'c) parser
    val ||         : ('a, 'b) parser * ('a, 'b) parser -> ('a, 'b) parser
    val >>         : ('a, 'b) parser * ('b -> 'c) -> ('a, 'c) parser
    val >>*        : ('a, 'b) parser * ('b -> 'c option) -> ('a, 'c) parser
    val >>=        : ('a, 'b) parser * ('b -> ('a, 'c) parser) 
                     -> ('a, 'c) parser
	
    val #--        : ('elm, 'a) parser * ('elm, 'b) parser -> ('elm, 'b) parser
    val --#        : ('elm, 'a) parser * ('elm, 'b) parser -> ('elm, 'a) parser

    val $--        : string * (char, 'a) parser -> (char, 'a) parser
    val --$        : (char, 'a) parser * string -> (char, 'a) parser

    val repeat0    : ('elm, 'res) parser -> ('elm, 'res list) parser
    val repeat1    : ('elm, 'res) parser -> ('elm, 'res * 'res list) parser
    val optional   : ('elm, 'res) parser -> ('elm, 'res option) parser 

    val getItem    : ('elm, 'elm) parser
    val failure    : ('elm, 'res) parser
    val success    : 'res -> ('elm, 'res) parser
    val eof        : 'res -> ('elm, 'res) parser

    val $#         : char   -> (char, char) parser
    val $          : string -> (char, string) parser

    val getLit     : ''elm -> (''elm, ''elm) parser

    val getChar    : (char -> bool) -> (char, char) parser
    val getChars0  : (char -> bool) -> (char, string) parser
    val getChars1  : (char -> bool) -> (char, string) parser

    val getElem    : ('elm -> bool) -> ('elm, 'elm) parser
    val getElems0  : ('elm -> bool) -> ('elm, 'elm list) parser
    val getElems1  : ('elm -> bool) -> ('elm, 'elm * 'elm list) parser

    val commitChar : string -> (char, 'res) parser -> (char, 'res) parser
    val commitElem : string -> ('elm -> string) 
                     -> ('elm, 'res) parser -> ('elm, 'res) parser

    (* Applying a parser to a stream *)

    val parse      : ('elm, 'res) parser -> 'elm stream -> 'res option

    val scan       : (('a, 'a stream) StringCvt.reader -> 'a stream -> 'b)
                     -> 'a stream -> 'b

    val skipWS     : (char, 'res) parser -> (char, 'res) parser

    val scanString : (char, 'res) parser -> string -> 'res option 
    val scanSubstr : (char, 'res) parser -> substring -> 'res option
    val scanList   : ('elm, 'res) parser -> 'elm list -> 'res option
end

(* 
   ['elm stream] is the type of a lazy streams (sequences) of 'elm values.

   [('elm, 'res) parser] is the type of parsers that consume elements
   from an 'elm stream to produce a result of type 'res.  The attempt
   may succeed with a result of type 'res, in which case the parser
   may have consumed elements of the stream, or it may fail, in which
   case the 'elm stream remains unchanged.

   Below we indicate those parsers that are guaranteed to consume from
   the stream if they succeed.  Such parsers can be safely used as the
   argument to e.g. the repeat0 and repeat1 parser-transformers.

   [stream getc src] builds a stream from a reader getc and a stream
   state src.

   [par1 -- par2] succeeds with (r1, r2) if par1 succeeds with r1 and
   then par2 succeeds with r2 on the remainder of the stream.

   [par1 || par2] succeeds with r1 if par1 succeeds with r1 on stream;
   otherwise succeeds with r2 if par2 succeeds with r2 on stream;
   otherwise fails.

   [par >> f] succeeds with f(r) if par succeeds with r on the stream.

   [par >>* f] succeeds with y if par succeeds with r on the stream,
   and f r = SOME y; fails otherwise.

   [par1 >>= parf2] succeeds with r2 if par1 succeeds with r1 and then
   (parf2 r1) succeeds with r2 on the remainder of the stream.

   [par1 #-- par2] succeeds with r2 if par1 succeeds with r1 and
   then par2 succeeds with r2 on the remainder of the stream.

   [par1 --# par2] succeeds with r1 if par1 succeeds with r1 and
   then par2 succeeds with r2 on the remainder of the stream.

   [s $-- par] succeeds with r if the stream begins with the
   characters of the string s, and par succeeds with r on the
   remainder of the stream.

   [par $-- s] succeeds with r if p succeeds with r, and then the
   remainder of the stream begins with the characters of the string s.

   Recommended fixities:

        infix 6 $-- --$ #-- --#
        infix 5 --
        infix 3 >> >>*
        infix 2 >>=
        infix 0 ||

   [repeat0 par] succeeds with [r1, r2, ..., rn] if par succeeds with
   r1, and then par succeeds with r2 on the remainder of the stream,
   and then par succeeds with r3 on the remainder, etc, for some n>=0.
   May succeed without consuming from the stream, even if par must
   consume from the stream to succeed.

   [repeat1 par] succeeds with (r1, [r2, ..., rn]) if par succeeds
   with r1, and then par succeeds with r2 on the remainder of the
   stream, and then par succeeds with r3 on the remainder, etc, for
   some n>=1.  Must consume from the stream to succeed, provided par
   must consume from the stream to succeed.  Equivalent to 
        par -- repeat0 par

   [optional par] succeeds with SOME(r) if par succeeds with r;
   succeeds with NONE otherwise.  May succeed without consuming from
   the stream.  Equivalent to 
        par || succeed NONE

   [success v] always succeeds with v, consuming nothing from the stream.

   [failure] always fails.

   [getItem] succeeds with r if the stream is non-empty and its first
   element is r; fails otherwise.  Must consume from the stream if it
   succeeds.

   [eof v] succeeds with v if the stream is empty (end of file); fails
   otherwise. 

   [$# c] succeeds with c if the stream begins with character c.
   Consumes from the stream if it succeeds.

   [$ s] succeeds with s if the stream begins with string s.  Consumes
   from the stream if it succeeds.

   [getChar p] succeeds with c if the first character c of the
   stream satisfies predicate p.  Consumes from the stream if it
   succeeds.

   [getChars0 p] always succeeds with s, where s is the longest prefix
   of the stream all of whose characters satisfy predicate p.  May
   succeed without consuming from the stream.  Equivalent to 
        repeat0 (getChar p) >> String.implode

   [getChars1 p] succeeds with s, where s is the longest non-empty
   prefix of the stream all of whose characters satisfy predicate p,
   if such a prefix exists; fails if the first character of the stream
   does not satisfy p.  Consumes from the stream if it succeeds.  
   Equivalent to
        repeat1 (getChar p) >> op:: >> String.implode

   [getLit elm] succeeds with elm if the first element of the stream
   equals elm; fails otherwise.  Consumes from the stream if it
   succeeds.

   [getElem p] succeeds with elm if the first element elm of the
   stream satisfies predicate p.  Consumes from the stream if it
   succeeds.

   [getElems0 p] always succeeds with elms, where elms is the longest
   prefix of the stream all of whose elements satisfy predicate p.
   May succeed without consuming from the stream.  Equivalent to 
        repeat0 (getElem pred) strm 

   [getElems1 p] succeeds with (elm1, elmr), where elm1::elmr is the
   longest non-empty prefix of the stream all of whose elements
   satisfy predicate p; fails if the first element of the stream does
   not satisfy p.  Consumes from the stream if it succeeds.
   Equivalent to 
        repeat1 (getElem pred) strm

   [commitChar cexpd par] succeeds with r if par succeeds with r;
   raises exception Parse otherwise.  The exception's string argument
   will have the form
      Expected <cexpd> but found <cfound> at character number n
   where cfound is the first character of the stream to which
   commitChar is applied, or the text "eof" if that stream is empty,
   and n is the number of the stream character cfound counting from the
   beginning of the stream.  The first character has number 0.

   [commitElem eexpd showe par] succeeds with r if par succeeds with r;
   raises exception Parse otherwise.  The exception's string argument
   will have the form
      Expected <eexpd> but found <efound> at element number n
   where efound is the result of applying showe to the first element
   of the stream, or the text "eof" if the stream is empty, and n is
   the number of stream element efound counting from the beginning of
   the stream.  The first element has number 0.

   [parse par strm] applies the parser par to the stream strm.
   Returns SOME(v) if the parser succeeds with v; returns NONE
   otherwise.

   [scan scfn] produces a parser corresponding to the scan function
   scfn.  Typical examples are:

        scan Bool.scan                  : (char, bool) parser
        scan Date.scan                  : (char, Date.date) parser
        scan Real.scan                  : (char, real) parser
        scan Time.scan                  : (char, Time.time) parser
        scan (Int.scan StringCvt.DEC)   : (char, int) parser
        scan (Word.scan StringCvt.HEX)  : (char, Word.word) parser
        scan (Word8.scan StringCvt.HEX) : (char, Word8.word) parser

   For instance, one may produce a list of real numbers by parsing
   space-separated real numerals from a string s:

     scanString (repeat0 (getChars0 Char.isSpace #-- scan Real.scan)) s

   [skipWS par] removes any leading whitespace from the stream and
   then applies par to the remainder.

   [scanSubstring par sus] applies the parser par to the stream of
   characters taken from the substring sus.  Returns SOME v if par
   succeeds with v; returns NONE otherwise.  Equivalent to 
        parse par (stream Substring.getc sus)

   [scanString par s] applies the parser par to the stream of
   characters taken from the string s.  Returns SOME v if par succeeds
   with v; returns NONE otherwise.

   Example: Parsing an integer numeral from the string "723984626junk":

      val intphr = getChars1 Char.isDigit
      val i : string option = scanString intphr "723984626junk"

   [scanList phr xs] applies the parser par to the stream of elements
   from the list xs.  Returns SOME v if par succeeds with v; returns 
   NONE otherwise.  Equivalent to 
        parse par (stream List.getItem cs)
*)
