(* BinIO -- SML Basis Library *)

type elem   = Word8.word
type vector = Word8Vector.vector

(* Binary input *)

type instream 

val openIn       : string -> instream
val closeIn      : instream -> unit
val input        : instream -> vector
val inputAll     : instream -> vector
val inputNoBlock : instream -> vector option
val input1       : instream -> elem option
val inputN       : instream * int -> vector
val endOfStream  : instream -> bool
val lookahead    : instream -> elem option

(* Binary output *)

type outstream

val openOut      : string -> outstream
val openAppend   : string -> outstream
val closeOut     : outstream -> unit
val output       : outstream * vector -> unit
val output1      : outstream * elem -> unit
val flushOut     : outstream -> unit

(* 
   This structure provides input/output functions on byte streams.
   The functions are state-based: reading from or writing to a stream
   changes the state of the stream.  The streams are buffered: output
   to a stream may not immediately affect the underlying file or
   device.

   [instream] is the type of state-based byte input streams.

   [outstream] is the type of state-based byte output streams.

   [elem] is the type Word8.word of bytes.

   [vector] is the type of Word8Vector.vector (byte vectors).


   BYTE INPUT:

   [openIn s] creates a new instream associated with the file named s.
   Raises Io.Io is file s does not exist or is not accessible.

   [closeIn istr] closes stream istr.  Has no effect if istr is closed
   already.  Further operations on istr will behave as if istr is at
   end of stream (that is, will return "" or NONE or true).

   [input istr] reads some elements from istr, returning a vector v of
   those elements.  The vector will be empty (size v = 0) if and only
   if istr is at end of stream or is closed.  May block (not return
   until data are available in the external world).

   [inputAll istr] reads and returns the vector v of all bytes
   remaining in istr up to end of stream.

   [inputNoBlock istr] returns SOME(v) if some elements v can be read
   without blocking; returns SOME("") if it can be determined without
   blocking that istr is at end of stream; returns NONE otherwise.  If
   istr does not support non-blocking input, raises
   Io.NonblockingNotSupported.

   [input1 istr] returns SOME(e) if at least one element e of istr is
   available; returns NONE if istr is at end of stream or is closed;
   blocks if necessary until one of these conditions holds.

   [inputN(istr, n)] returns the next n bytes from istr as a vector,
   if that many are available; returns all remaining bytes if end of
   stream is reached before n bytes are available; blocks if necessary
   until one of these conditions holds.  

   [endOfStream istr] returns false if any elements are available in
   istr; returns true if istr is at end of stream or closed; blocks if
   necessary until one of these conditions holds.

   [lookahead istr] returns SOME(e) where e is the next element in the
   stream; returns NONE if istr is at end of stream or is closed;
   blocks if necessary until one of these conditions holds.  Does not
   advance the stream.


   BYTE OUTPUT:

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

   [output(ostr, v)] writes the byte vector v on outstream ostr.

   [output1(ostr, e)] writes the byte e on outstream ostr.

   [flushOut ostr] flushes the outstream ostr, so that all data
   written to ostr becomes available to the underlying file or device.


   The functions below are not yet implemented:

   [setPosIn(istr, i)] sets istr to the position i.  Raises Io.Io if
   not supported on istr.

   [getPosIn istr] returns the current position of istr.  Raises Io.Io
   if not supported on istr.

   [endPosIn istr] returns the last position of istr.  

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
