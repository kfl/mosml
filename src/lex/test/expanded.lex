{ }

rule Token = parse
    [`0`-`9``A`-`F`]+		{ Tok.INT(getLexeme lexbuf)}
  | [`a`-`z``A`-`Z`] ([`a`-`z``A`-`Z`] | [`0`-`9`] | [`_` `'`])* 
				{ Tok.IDENT(getLexeme lexbuf)}
;
