
val main = 
    case CommandLine.arguments() of
	[] => print("usage: "^CommandLine.name()^" pmfile\n")
      | file :: _ => ( PMCompile.compile file
                     ; PMCompile.link [] file
		     ; ()
		     )
