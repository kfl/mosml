
fun createLexerStream (is : BasicIO.instream) =
    Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n);

fun readlam filename = 
    let open BasicIO Nonstdio 
	val is     = open_in filename
	val lexbuf = createLexerStream is
	val lam    = test3.Lambda test3lex.Token lexbuf
	val env0   = fn s => raise Fail ("Unknown identifier: " ^ s)
    in 
	lam env0
	before close_in is 
    end
    handle SysErr _ => raise Fail ("Cannot read file " ^ filename)

