(* TextIO -- SML Basis Library *)

type elem   = Char.char
type vector = string

(* Text input *)

type instream 

val openIn       : string -> instream
val closeIn      : instream -> unit
val input        : instream -> vector
val inputAll     : instream -> vector
val inputNoBlock : instream -> vector option
val input1       : instream -> elem option
val inputN       : instream * int -> vector
val inputLine    : instream -> string
val endOfStream  : instream -> bool
val lookahead    : instream -> elem option

type cs (* character source state *)

val scanStream   : ((char, cs) StringCvt.reader -> ('a, cs) StringCvt.reader) 
                   -> instream -> 'a option

val stdIn        : instream

(* Text output *)

type outstream

val openOut      : string -> outstream
val openAppend   : string -> outstream
val closeOut     : outstream -> unit
val output       : outstream * vector -> unit
val output1      : outstream * elem -> unit
val outputSubstr : outstream * substring -> unit
val flushOut     : outstream -> unit

val stdOut       : outstream
val stdErr       : outstream

val print        : string -> unit

(* 
   This structure provides input/output functions on text streams.
   The functions are state-based: reading from or writing to a stream
   changes the state of the stream.  The streams are buffered: output
   to a stream may not immediately affect the underlying file or
   device.

   Note that under DOS, Windows, OS/2, and MacOS, text streams will be
   `translated' by converting (e.g.) the double newline CRLF to a
   single newline character \n.

   [instream] is the type of state-based characters input streams.

   [outstream] is the type of state-based character output streams.

   [elem] is the type char of characters.

   [vector] is the type of character vectors, that is, strings.


   TEXT INPUT:

   [openIn s] creates a new instream associated with the file named s.
   Raises Io.Io is file s does not exist or is not accessible.

   [closeIn istr] closes stream istr.  Has no effect if istr is closed
   already.  Further operations on istr will behave as if istr is at
   end of stream (that is, will return "" or NONE or true).

   [input istr] reads some elements from istr, returning a vector v of
   those elements.  The vector will be empty (size v = 0) if and only
   if istr is at end of stream or is closed.  May block (not return
   until data are available in the external world).

   [inputAll istr] reads and returns the string v of all characters
   remaining in istr up to end of stream.

   [inputNoBlock istr] returns SOME(v) if some elements v can be read
   without blocking; returns SOME("") if it can be determined without
   blocking that istr is at end of stream; returns NONE otherwise.  If
   istr does not support non-blocking input, raises
   Io.NonblockingNotSupported.

   [input1 istr] returns SOME(e) if at least one element e of istr is
   available; returns NONE if istr is at end of stream or is closed;
   blocks if necessary until one of these conditions holds.

   [inputN(istr, n)] returns the next n characters from istr as a
   string, if that many are available; returns all remaining
   characters if end of stream is reached before n characters are
   available; blocks if necessary until one of these conditions holds.
   (This is the behaviour of the `input' function prescribed in the
   1990 Definition of Standard ML).

   [inputLine istr] returns one line of text, including the
   terminating newline character.  If end of stream is reached before
   a newline character, then the remaining part of the stream is
   returned, with a newline character added.  If istr is at end of
   stream or is closed, then the empty string "" is returned.

   [endOfStream istr] returns false if any elements are available in
   istr; returns true if istr is at end of stream or closed; blocks if
   necessary until one of these conditions holds.

   [lookahead istr] returns SOME(e) where e is the next element in the
   stream; returns NONE if istr is at end of stream or is closed;
   blocks if necessary until one of these conditions holds.  Does not
   advance the stream.

   [stdIn] is the buffered state-based standard input stream.

   [scanStream scan istr] turns the instream istr into a character
   source and applies the scanner `scan' to that source.  See
   StringCvt for more on character sources and scanners.  The Moscow
   ML implementation currently can backtrack only 512 characters, and
   raises Fail if the scanner backtracks further than that.


   TEXT OUTPUT:

   [openOut s] creates a new outstream associated with the file named
   s.  If file s does not exist, and the directory exists and is
   writable, then a new file is created.  If file s exists, it is
   truncated (any existing contents are lost).

   [openAppend s] creates a new outstream associated with the file
   named s.  If file s does not exist, and the directory exists and is
   writable, then a new file is created.  If file s exists, any
   existing contents are retained, and output goes at the end of the
   file.

   [closeOut ostr] closes stream ostr; further operations on ostr
   (except for additional close operations) will raise exception Io.Io.

   [output(ostr, v)] writes the string v on outstream ostr.

   [output1(ostr, e)] writes the character e on outstream ostr.

   [flushOut ostr] flushes the outstream ostr, so that all data
   written to ostr becomes available to the underlying file or device.

   [stdOut] is the buffered state-based standard output stream.

   [stdErr] is the unbuffered state-based standard error stream.  That
   is, it is always kept flushed, so flushOut(stdErr) is redundant.

   [print s] outputs s to stdOut and flushes immediately.


   The functions below are not yet implemented:

   [setPosIn(istr, i)] sets istr to the (untranslated) position i.
   Raises Io.Io if not supported on istr.

   [getPosIn istr] returns the (untranslated) current position of istr.
   Raises Io.Io if not supported on istr.

   [endPosIn istr] returns the (untranslated) last position of istr.
   Because of translation, one cannot expect to read 
        endPosIn istr - getPosIn istr
   from the current position.

   [getPosOut ostr] returns the current position in stream ostr.
   Raises Io.Io if not supported on ostr.

   [endPosOut ostr] returns the ending position in stream ostr.
   Raises Io.Io if not supported on ostr.

   [setPosOut(ostr, i)] sets the current position in stream to ostr to
   i.  Raises Io.Io if not supported on ostr.

   [mkInstream sistr] creates a state-based instream from the
   functional instream sistr.

   [getInstream istr] returns the functional instream underlying the
   state-based instream istr.

   [setInstream(istr, sistr)] redirects istr, so that subsequent input
   is taken from the functional instream sistr.

   [mkOutstream sostr] creates a state-based outstream from the
   outstream sostr.

   [getOutstream ostr] returns the outstream underlying the
   state-based outstream ostr.

   [setOutstream(ostr, sostr)] redirects the outstream ostr so that
   subsequent output goes to sostr.
*)
