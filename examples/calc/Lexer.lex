{
open Parser;        (* The token type is defined in Parser.sig *)

val intOfString = valOf o Int.fromString;

}

rule Token = parse
    [` ` `\t`]     { Token lexbuf }     (* skip blanks *)
  | [`\n` ]        { EOL }
  | [`0`-`9`]+     { INT(intOfString (getLexeme lexbuf)) }
  | `+`            { PLUS }
  | `-`            { MINUS }
  | `*`            { TIMES }
  | `/`            { DIV }
  | `(`            { LPAREN }
  | `)`            { RPAREN }
  | `~`            { UMINUS }
  | eof            { EOF }
  | _              { raise Fail "illegal symbol" }
;
