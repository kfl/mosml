(* File mosml/examples/pretty/pproman.sml 
 * Example demonstrating installable prettyprinters.
 *
 * Try it by running 
 *	mosml pproman.sml
 * from the command line.
 *)

val _ = (load "PP"; load "Int");

fun ppint pps d = 
    let open PP
	val romannum =
	    [(1000, "M"), (900, "CM"), (500, "D"), (400, "CD"), 
	     (100, "C"), (90, "XC"), (50, "L"), (40, "XL"), 
	     (10, "X"), (9, "IX"), (5, "V"), (4, "IV"), (1, "I")]
	fun choose (n : int, [])               = []
	  | choose (n, romans as ((s, name) :: romanr)) = 
	    if n >= s then name :: choose(n - s, romans) else choose(n, romanr)
	fun roman 0 = "nullus"
	  | roman n =
	    concat ((if n < 0 then "-" else "") :: choose (abs n, romannum))
    in
	begin_block pps INCONSISTENT 6; 
	add_string pps (roman d);
	end_block pps
    end;
    
val _ = installPP ppint;

val _ = print 
    "\nNow a pretty-printer for integers has been installed.\n\
     \To see it in action, evaluate e.g.\n\
     \  49;\n  23+5;\n  List.tabulate(10, fn i => i*3);\n\n";
    
