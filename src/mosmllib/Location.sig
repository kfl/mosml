(* Location -- error reporting for mosmllex and mosmlyac               *)
(* Based on src/compiler/location from the Caml Light 0.6 distribution *)

datatype Location =  (* Source file positions                            *)
    Loc of int       (* Position of the first character                  *)
         * int       (* Position of the character following the last one *)

val errLocation : string * BasicIO.instream * Lexing.lexbuf -> Location
                  -> unit
val errMsg      : string * BasicIO.instream * Lexing.lexbuf -> Location
                  -> string -> 'a
val errPrompt   : string -> unit; 
val nilLocation : Location
val getCurrentLocation : unit -> Location
val mkLoc : 'a -> Location * 'a
val xLR   : Location * 'a -> Location
val xL    : Location * 'a -> int
val xR    : Location * 'a -> int
val xxLR  : Location * 'a -> Location * 'b -> Location
val xxRL  : Location * 'a -> Location * 'b -> Location

(* 
   These functions support error reporting in lexers and parsers
   generated with mosmllex and mosmlyac.  The directory
   mosml/examples/lexyacc/ contains an example of their use.
 
   [errLocation (file, stream, lexbuf) loc] prints the part of the lexer 
   input which is indicated by location loc.  
   
   If file <> "" then it is assumed to be the name of the file from
   which the lexer reads, the stream is assumed to be an open input
   stream associated with this file, and lexbuf is the lexer buffer
   used to read from the stream.  Under MS DOS (and presumably
   Windows, OS/2, and MacOS), the stream must have been opened in
   binary mode (with Nonstdio.open_in_bin), or else the positioning in
   the file will be wrong (due to the translation of CRLF into
   newline in text files).

   If file = "" then the lexer is assumed to read from some source
   other than a stream, and the lexbuf (rather than the instream) is
   used to obtain the location indicated, if possible.  In this case
   the stream is immaterial; it will not be used.

   [errMsg (file, stream, lexbuf) loc msg] calls errLocation to print
   the indicated part of the lexer input, then prints the error
   message msg and raises exception Fail.

   [errPrompt msg] prints "! ", the string msg, and a newline on
   standard output.  

   [nilLocation] is the undefined location.

   [getCurrentLocation ()] can be called within the semantic action
   part of a grammar rule (only) and returns the location of the
   string matching the left-hand side of the rule.

   [mkLoc a] can be called within the semantic action part of a
   grammar rule (only), and returns a pair (loc, a) of the current
   location and the value a.  This is typically used to decorate
   abstract syntax tree nodes with location information, for use in
   subsequent error reports.

   [xLR loc_a] returns the location of the decorated value loc_a.

   [xL loc_a] returns the left end position of loc_a.

   [xR loc_a] returns the right end position of loc_a.

   [xxLR loc_a loc_b] returns the location extending from the left end
   of loc_a to the right end of loc_b.

   [xxRL loc_a loc_b] returns the location extending from the right end
   of loc_a to the left end of loc_b.
*)
