{ }

let letter = [`a`-`z``A`-`Z`]
let digit  = [`0`-`9`]
let ident  = letter (letter | digit | [`_` `'`])*
let digit  = digit | [`A`-`F`]

rule Token = parse
    digit+   { Tok.INT(getLexeme lexbuf)}
  | ident    { Tok.IDENT(getLexeme lexbuf)}
;
