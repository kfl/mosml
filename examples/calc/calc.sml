local open BasicIO Nonstdio in

fun createLexerStream (is : instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)
;

val _ =
  (while true do
       let val lexbuf = createLexerStream std_in 
	   val () = print "Enter an integer expression: \n"
	   val result = Parser.Main Lexer.Token lexbuf
       in
	   print (makestring result ^ "\n")
       end)
       handle _ => print "Bye-bye!\n";

end
