(* Lexer.sig *)

val enableHashbang  : bool -> unit;
val quotation : bool ref;
val resetLexerState : unit -> unit;
val Token : Lexing.lexbuf -> Parser.token;
