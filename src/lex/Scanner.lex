(* The lexical analyzer for lexer definitions. Bootstrapped! *)

{
open Fnlib Syntax Grammar Scan_aux;
}

rule main = parse
    [` ` `\n` `\r` `\t` ] +
    { main lexbuf }
  | "(*"
    { (comment_depth := 1; comment lexbuf; main lexbuf) }
  | "*)"
      { raise Lexical_error "unmatched comment bracket" }
  | [`A`-`Z` `a`-`z`] ( [ `A`-`Z` `a`-`z` `0`-`9` `_` `'`] )*
    { case getLexeme lexbuf of
        "rule"  => Trule
      | "parse" => Tparse
      | "and"   => Tand
      | "eof"   => Teof
      | "let"   => Tlet
      | s       => Tident s }
  | `"`
    { (reset_string_buffer();
       string lexbuf;
       Tstring(get_stored_string())) }
  | "`"
    { Tchar(char lexbuf) }
  | `{`
    { let val n1 = getLexemeEnd lexbuf
          val () = brace_depth := 1
          val n2 = action lexbuf
      in Taction(Location(n1, n2)) end }
  | `=`  { Tequal }
  | ";"  { Tend }
  | `|`  { Tor }
  | `_`  { Tunderscore }
  | "eof"  { Teof }
  | `[`  { Tlbracket }
  | `]`  { Trbracket }
  | `*`  { Tstar }
  | `?`  { Tmaybe }
  | `+`  { Tplus }
  | `(`  { Tlparen }
  | `)`  { Trparen }
  | `^`  { Tcaret }
  | `-`  { Tdash }
  | (eof | `\026`)
    { raise Lexical_error "unterminated lexer definition" }
  | _
    { raise Lexical_error
             ("illegal character #" ^ makestring(getLexeme lexbuf)) }

and action = parse
    `{`
        { (incr brace_depth; action lexbuf) }
  | `}`
        { (decr brace_depth;
           if !brace_depth = 0 then
             getLexemeStart lexbuf
           else
             action lexbuf) }
  | `"`
    { (reset_string_buffer();
       string lexbuf;
       reset_string_buffer();
       action lexbuf) }
  | "(*"
    { (comment_depth := 1; comment lexbuf; action lexbuf) }
  | "*)"
      { raise Lexical_error "unmatched comment bracket" }
  | (eof | `\026`)
    { raise Lexical_error "unterminated action" }
  | _
    { action lexbuf }

and string = parse
    `"`
    { () }
  | `\\` [` ` `\t` `\n` `\r`]+ `\\`
      { string lexbuf }
  | `\\` [`\\` `"` `n` `t` `b` `r`]
    { (store_string_char(char_for_backslash(getLexemeChar lexbuf 1));
       string lexbuf) }
  | `\\` `^` [`@`-`_`]
      { (store_string_char(
           Char.chr(Char.ord(getLexemeChar lexbuf 2) - 64));
         string lexbuf) }
  | `\\` [`0`-`9`] [`0`-`9`] [`0`-`9`]
    { let val code = char_for_decimal_code lexbuf 1 in
        if Char.ord code >= 256 then
          raise Lexical_error "character code in string > 255"
        else ();
        store_string_char code;
        string lexbuf
      end }
  | `\\`
      { raise Lexical_error "ill-formed escape sequence in string" }
  | (eof | `\026`)
    { raise Lexical_error "unterminated string" }
  | [`\001`-`\031` `\127` `\255`]
      { raise Lexical_error "invalid character in string" }
  | _
    { (store_string_char(getLexemeChar lexbuf 0);
       string lexbuf) }

and char = parse
    `\\` [`\\` `\`` `n` `t` `b` `r`] "`"
    { char_for_backslash (getLexemeChar lexbuf 1) }
  | `\\` `^` [`@`-`_`] "`"
      { Char.chr(Char.ord(getLexemeChar lexbuf 2) - 64) }
  | `\\` [`0`-`9`] [`0`-`9`] [`0`-`9`] "`"
    { let val code = char_for_decimal_code lexbuf 1 in
        if Char.ord code >= 256 then
          raise Lexical_error "character code in string > 255"
        else ();
        code
      end }
  | `\\`
      { raise Lexical_error "ill-formed escape sequence in character constant" }
  | (eof | `\026`)
    { raise Lexical_error "unterminated character constant" }
  | [`\001`-`\031` `\127` `\255`]
      { raise Lexical_error "invalid character in character constant" }
  | _ "`"
    { getLexemeChar lexbuf 0 }
  | _
    { raise Lexical_error "ill-formed character constant" }

and comment = parse
    "(*"
    { (incr comment_depth; comment lexbuf) }
  | "*)"
    { (decr comment_depth;
       if !comment_depth = 0 then () else comment lexbuf) }
  | (eof | `\026`)
    { raise Lexical_error "unterminated comment" }
  | _
    { comment lexbuf }
;
