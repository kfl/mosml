{
 open Lexing Parser;

 exception LexicalError of string * int * int (* (message, loc1, loc2) *)

 fun lexerError lexbuf s = 
     raise LexicalError (s, getLexemeStart lexbuf, getLexemeEnd lexbuf);

 val commentStart = ref 0;  (* Start of outermost comment being scanned *)
 
 fun commentNotClosed lexbuf =
     raise LexicalError ("Comment not terminated", 
                         !commentStart, getLexemeEnd lexbuf);
     
 val commentDepth = ref 0;  (* Current comment nesting *)

 (* Scan keywords as identifiers and use this function to distinguish them. *)
 (* If the set of keywords is large, use an auxiliary hashtable.            *)

 fun keyword s =
     case s of
         "let"          => LET
       | "letrec"       => LETREC
       | "in"           => IN
       | "case"         => CASE
       | "of"           => OF
       | "if"           => IF
       | "then"         => THEN
       | "else"         => ELSE
       | "pack"         => PACK
       | "end"          => END
       | _              => NAME s;

 }

rule Token = parse
    [` ` `\t` `\n` `\r`]     { Token lexbuf }
  | `~`?[`0`-`9`]+      { case Int.fromString (getLexeme lexbuf) of
                               NONE   => lexerError lexbuf "internal error"
                             | SOME i => INT i
                        }  
  | [`a`-`z``A`-`Z`][`a`-`z``A`-`Z``0`-`9`]*
                        { keyword (getLexeme lexbuf) }
  | "(*"                { commentStart := getLexemeStart lexbuf;
                          commentDepth := 1; 
                          SkipComment lexbuf; Token lexbuf }
  | "->"                { DASHARROW }
  | `=`                 { EQ }
  | "~="                { NE }
  | `>`                 { GT }
  | `<`                 { LT }
  | ">="                { GE }
  | "<="                { LE }
  | `+`                 { PLUS }                     
  | `-`                 { MINUS }                     
  | `*`                 { TIMES }                     
  | `/`                 { DIV }                     
  | `%`                 { MOD }                     
  | `|`                 { BAR }                     
  | `&`                 { AMPERSAND }                     
  | `\\`                { LAMBDA }                     
  | `(`                 { LPAR }
  | `)`                 { RPAR }
  | `{`                 { LBRACE }
  | `}`                 { RBRACE }
  | `;`                 { SEMI }
  | `,`                 { COMMA }
  | `.`                 { DOT }
  | eof                 { EOF }
  | _                   { lexerError lexbuf "Illegal symbol in input" }

and SkipComment = parse
    "*)"                { commentDepth := !commentDepth - 1;  
                          if !commentDepth = 0 then ()
                          else SkipComment lexbuf 
                        } 
   | "(*"               { commentDepth := !commentDepth + 1; 
                          SkipComment lexbuf }
   | (eof | `\^Z`)      { commentNotClosed lexbuf }
   | _                  { SkipComment lexbuf }
;
