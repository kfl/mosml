val usage =
    "usage: mosmlpm [options] pmfile\n\
     \Options:\n\
     \  -c              Compile only\n\
     \  -o <file>       Place the output into <file>\n\
     \  -standalone     Pass the option -standalosne on to mosmlc\n"


val main = 
    case CommandLine.arguments() of
	[] => print usage
      | file :: _ => ( PMCompile.compile file
                     ; PMCompile.link [] file (Path.base file)
		     ; ()
		     )
