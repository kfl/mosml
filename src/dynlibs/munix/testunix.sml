app load ["Unix", "Signal", "Int", "Bool"];

open Unix TextIO;

fun startandstop n = 
    let val p = execute("./sieve", ["10000"])
	val (is, os) = streamsOf p
	val msg = Int.toString n
	val _ = (print msg; output(os, msg); output(os, "\n"); flushOut os)
	val res = input is
    in
	kill(p, Signal.term);
	reap p;
	print " ";
	print res;
	res
    end

val _ = List.tabulate(200, startandstop);

val p = execute("./sieve", ["10000000"]);

val _ = print "Started subprocess...\n";

val (is, os) = streamsOf p;

fun wr i = 
    (output(os, Int.toString i); output(os, "\n"); flushOut os; input is);

val res = map wr [2, 1999, 18762341, 76523, 666273];

val _ = kill(p, Signal.term);

val _ = reap p;

val _ = quit();
