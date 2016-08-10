val usage =
    "usage: mosmlpm [options] pmfile\n\
     \Options:\n\
     \  -c              Compile only\n\
     \  -o <file>       Place the output into <file>\n\
     \  -quiet          Make mosmlpm less chatty\n\
     \  -standalone     Pass the option -standalone on to mosmlc\n"

local
    val compileOnly = ref false
    val execFile    = ref ""
    val standalone  = ref false
in
fun compileAndLink filename =
    let 
        val mlbAST = Mlb_functions.openParseSingleFile filename
    in
        Mlb_functions.printAST mlbAST
    end

fun assign r v = r := v
fun assignTrue r () = r := true
fun main () =
    let val filename = ref "" 
	val _ =  
	    ArgParse.parse 
	       [("-c",          ArgParse.Unit (assignTrue compileOnly))
	       ,("-o",          ArgParse.String (assign execFile))
	       ,("-standalone", ArgParse.Unit (assignTrue standalone))
(*
	       ,("-quiet",      ArgParse.Unit (assignTrue MLBCompile.quiet))
           ,("-debug",      ArgParse.Unit (assignTrue MLBCompile.debugFlag))
*)
	       ] (assign filename)
    in  if !filename = "" then 
	    ( app print ["Error: no project file specified\n", 
			 usage]
            ; OS.Process.exit OS.Process.failure
            )
	else compileAndLink (!filename)
    end
end

val _ = (main() before OS.Process.exit OS.Process.success)
        handle e => (print ("Error occurred:\n"^exnMessage e^"\n");
		     OS.Process.exit OS.Process.failure)

