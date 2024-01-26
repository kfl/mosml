val quotation : bool ref;
val utf8 : bool ref;
val resetLexerState : unit -> unit;
val Token : Lexing.lexbuf -> Parser.token;
