(* Test Mosml.run: 
   - read from standard input
   - write to standard output and standard error 

 *)

fun main () = 
    let open TextIO
	val inp = inputAll stdIn
	fun stdout s = output(stdOut, s)
	fun stderr s = output(stdErr, s)
    in
	stdout "[";
	stdout inp;
	stdout "]\n";
	flushOut stdOut;
	stderr "<";
	stderr inp;
	stderr ">\n";
	stdout "---";
	stdout "==="
    end

val _ = main ()
