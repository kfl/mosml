(* Array2 -- as of 1995-09-12, 1997-03-12, 1998-04-07 *)

(* Representation of an m * n array: an m-vector of n-arrays (rows),
   and the dimensions (m, n), m = number of rows, n = number of columns. *)

(* The implementation below is pretty naive, using the Array and Vector 
   operations and doing more bounds checks than strictly necessary.  *)

type 'a array = ('a Array.array Vector.vector * int * int) ref 

datatype traversal = RowMajor | ColMajor

type 'a region = { base : 'a array, row : int, col : int, 
		   nrows : int option, ncols : int option}

fun fromList [] = ref (Vector.fromList [], 0, 0)
  | fromList (xs1 :: xsr) =
    let val row1 = Array.fromList xs1
	val rowr = List.map Array.fromList xsr
	val cols = Array.length row1
	val vec = 
	    if List.all (fn r => Array.length r = cols) rowr then
		Vector.fromList (row1 :: rowr)
	    else
		raise Size
    in 
	ref (vec, Vector.length vec, cols)
    end

fun array (m, n, x) = 
    ref (Vector.tabulate(m, fn _ => Array.array(n, x)), m, n);

fun tabulate RowMajor (m, n, f) = 
    ref (Vector.tabulate(m, fn i => Array.tabulate(n, fn j => f(i,j))), m, n)
  | tabulate ColMajor (m, n, f) = 
    if m > 0 andalso n > 0 then 
	let val f00 = f(0, 0)
	    val arr = Vector.tabulate(m, fn i => Array.array(n, f00))
	    (* Column 0: do not apply f to (0,0) again: *)
	    val _ = Vector.appi (fn (r, a) => Array.update(a, 0, f(r, 0))) 
		                (arr, 1, NONE); 		
	    (* Remaining columns: loop, updating all rows: *)
	    fun loop c = 
		if c < n then  
		    (Vector.appi (fn (r, a) => Array.update(a, c, f(r, c))) 
		                 (arr, 0, NONE); 
		     loop (c+1))
		else ()
	in loop 1; ref (arr, m, n) end
    else
	tabulate RowMajor (m, n, f)

fun dimensions (ref (a,m,n)) = (m,n);

fun nRows (ref (a,m,n)) = m;

fun nCols (ref (a,m,n)) = n;

fun sub(ref (a,m,n), i, j) = Array.sub(Vector.sub(a, i), j);

fun update(ref (a,m,n), i, j, x) = Array.update(Vector.sub(a, i), j, x);

fun row (ref (a, _, _), i) = 
    Array.extract(Vector.sub(a, i), 0, NONE);

fun column (ref (a, m, n), j) = 
    if j<0 orelse j>=n then raise Subscript
    else Vector.tabulate(m, fn k => Array.sub(Vector.sub(a, k), j))

fun fold RowMajor f b (ref (a, _, _)) = 
    Vector.foldl (fn (xs, res) => Array.foldl f res xs) b a
  | fold ColMajor f b (ref (a, m, n)) = 
    let fun rows j i b = 
	    if i >= m then b 
	    else rows j (i+1) (f(Array.sub(Vector.sub(a, i), j), b))
	fun cols j b =
	    if j >= n then b 
	    else cols (j+1) (rows j 0 b)
    in cols 0 b end

fun stop len i NONE = 
    if i<0 orelse i>len then raise Subscript
    else len
  | stop len i (SOME n) = 
    if i<0 orelse n<0 orelse i+n>len then raise Subscript
    else i+n;

fun foldi RowMajor f b { base = ref (a, m, n), row, col, nrows, ncols } = 
    Vector.foldli 
           (fn (i, xs, res) => 
	       Array.foldli (fn (j,x,res) => f(i,j,x,res)) res (xs,col,ncols))
	   b (a, row, nrows)
  | foldi ColMajor f b { base = ref (a, m, n), row, col, nrows, ncols } = 
    let val stoprow = stop m row nrows
	val stopcol = stop n col ncols
	fun rows j i b = 
	    if i >= stoprow then b 
	    else rows j (i+1) (f(i, j, Array.sub(Vector.sub(a, i), j), b))
	fun cols j b =
	    if j >= stopcol then b 
	    else cols (j+1) (rows j row b)
    in cols col b end

fun app RowMajor f (ref (a, _, _)) = 
    Vector.app (Array.app f) a
  | app ColMajor f arr  =
    fold ColMajor (fn (a, _) => f a) () arr

fun appi RowMajor f { base = ref (a, _, _), row, col, nrows, ncols } =
    Vector.appi 
          (fn (i, xs) => Array.appi (fn (j, x) => f(i, j, x)) (xs, col, ncols))
	  (a, row, nrows)
  | appi ColMajor f reg =
    foldi ColMajor (fn (i, j, a, _) => f (i, j, a)) () reg

fun modify RowMajor f (ref (a, _, _)) = 
    Vector.app (Array.modify f) a
  | modify ColMajor f arr =
    foldi ColMajor (fn (i, j, a, _) => update(arr, i, j, f a)) ()
          {base=arr, row=0, col=0, nrows=NONE, ncols=NONE}

fun modifyi RowMajor f { base = ref (a, _, _), row, col, nrows, ncols } =
    Vector.appi 
          (fn (i, xs) => Array.modifyi (fn (j, x) => f(i, j, x)) 
	                               (xs, col, ncols))
	  (a, row, nrows)
  | modifyi ColMajor f (reg as {base, ...}) =
    foldi ColMajor (fn (i, j, a, _) => update(base, i, j, f (i, j, a))) () reg

fun copy { src = { base = ref (sa, sm, sn), row = src_row, col = src_col, 
		   nrows, ncols }, 
	   dst = ref (da, dm, dn), dst_row, dst_col } =
    let val stoprow = stop sm src_row nrows
	fun bottomUp from_row to_row = 
	    if from_row < src_row then ()
	    else
		(Array.copy { src = Vector.sub(sa, from_row), 
			      si = src_col, len = ncols,
			      dst = Vector.sub(da, to_row),
			      di = dst_col };
		 bottomUp (from_row-1) (to_row-1))
	fun topDown from_row to_row = 
	    if from_row >= stoprow then ()
	    else
		(Array.copy { src = Vector.sub(sa, from_row), 
			      si = src_col, len = ncols,
			      dst = Vector.sub(da, to_row),
			      di = dst_col };
		 topDown (from_row+1) (to_row+1))
    in
	if src_row <= dst_row then (* top dst overlaps with bot src; 
				      copy bottom-up *)
	    bottomUp (stoprow-1) (stoprow-1+dst_row-src_row)
	else (* bot dst overlaps with top src; copy top-down *)
	    topDown src_row dst_row
    end

	
