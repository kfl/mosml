local
  open Mixture Globals Asynt;
in

val createLexerStream : BasicIO.instream -> Lexing.lexbuf;
        (* Create a lexer buffer on the given input channel.
           [createLexerStream inchan] returns a lexer buffer which reads
           from the input channel [inchan], at the current reading position. *)


val parseToplevelPhrase : Lexing.lexbuf -> Dec * bool;
val cleanEnv : (''_a, 'b) Env -> (''_a * 'b) list;
val reportFixityResult : string * InfixStatus -> unit;
val verbose : bool ref;
val compileSignature : (string list) -> string -> Mode -> string -> unit;
val compileUnitBody : (string list) -> string -> Mode -> string -> unit;

end;
