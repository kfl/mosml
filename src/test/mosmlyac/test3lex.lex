{
 open Lexing test3 test3aux;

 exception LexicalError of string * int * int (* (message, loc1, loc2) *)

 fun lexerError lexbuf s = 
     raise LexicalError (s, getLexemeStart lexbuf, getLexemeEnd lexbuf);

 fun internalError lexbuf s =
     lexerError lexbuf ("internal error: " ^ s)

 val line = ref 1
 }

rule Token = parse
    [` ` `\t` `\r`]     { Token lexbuf }
  | `\n`                { line := !line + 1; Token lexbuf }
  | `-`?[`0`-`9`]+      { case Int.fromString (getLexeme lexbuf) of
                               NONE   => internalError lexbuf "NEGINT"
                             | SOME i => NUM i
                        }  
  | [`a`-`z``A`-`Z`][`a`-`z``A`-`Z``0`-`9``_`]*
                        { ID (getLexeme lexbuf, !line) }
  | `\\`                { LAM }
  | `.`                 { DOT }
  | `(`                 { LPAR }
  | `)`                 { RPAR }
  | eof                 { EOF }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

;
