(* test/cmdline.sml, PS 1997-03-07 *)

val _ = 
    (app print ["This program is invoked as `", CommandLine.name(), "'\n",
		"with arguments:\n"];
     app (fn a => (print a; print "\n")) (CommandLine.arguments ()))
