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
    let val outfile = if !execFile = "" then Path.base filename
		      else !execFile
	val options = if !standalone then ["-standalone "]
		      else []

    in  PMCompile.compile filename
      ; if !compileOnly then ()
	else ignore(PMCompile.link [] filename outfile)
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
	       ,("-quiet",      ArgParse.Unit (assignTrue PMCompile.quiet))
               ,("-debug",      ArgParse.Unit (assignTrue PMCompile.debugFlag))
	       ] (assign filename)
    in  if !filename = "" then 
	    app print ["Error: no project file specified\n", 
		       usage]
	else compileAndLink (!filename)
    end
end

val _ = main()

