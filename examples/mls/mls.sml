(* A very simple version of the Unix utility ls: list directory. *)

fun listdir path = 
    let open FileSys TextIO
	val dir = openDir path
	fun read NONE      res = res
	  | read (SOME f)  res = read (readDir dir) (f :: res)
	val filenames = Listsort.sort String.compare
	                (read (readDir dir) []) before closeDir dir 
	val (longest, count) = 
	    foldl (fn (x, (max, cnt)) => (Int.max(max, size x), cnt+1))
	          (0, 0) filenames
        val cols = Int.max(1, 80 div (longest + 2));
	val fstrows = (count-1) div cols
	val lastrow = count - cols * fstrows       (* 0 <= lastrow <= cols *)

	val filenames = Vector.fromList filenames	    
	fun file (row, col) = 
	    Vector.sub(filenames, col*fstrows+Int.min(lastrow, col)+row)
	fun left n s = StringCvt.padRight #" " n s
	fun prrow row j m = 
	    if j >= m then ""
	    else left (longest + 2) (file (row, j)) ^ prrow row (j+1) m
	fun println s = output(stdOut, s ^ "\n")
	fun prrows i = 
	    if i >= fstrows then () 
	    else (println (prrow i 0 cols); prrows (i+1))
    in
	prrows 0;		 	   (* Print all rows but the last *)
	println (prrow fstrows 0 lastrow)  (* Print the last row *)
    end;

fun errmsg s = TextIO.output(TextIO.stdErr, s ^ "\n");

val _ = 
    case Mosml.argv () of 
	[_]      => listdir "."
      | [_, dir] => ((listdir dir) 
	            handle OS.SysErr (explanation, _) => 
			errmsg ("Error: "^ explanation))
      | _        => errmsg "Usage: mls [directory]"

