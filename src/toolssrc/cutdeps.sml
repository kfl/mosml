(* Compile with 
	mosmlc -noheader -P full cutdeps.sml -o cutdeps
 *)

local 
    open BasicIO
in

fun next () = 
    if end_of_stream std_in then ()
    else 
	let open Substring
	    val line = input_line std_in
	in 
	    output(std_out, line); 
	    if isEmpty (#2 (position "### DO NOT DELETE THIS LINE" (all line)))
		then next()
	    else ()
	end

val _ = (next (); flush_out std_out)
end


