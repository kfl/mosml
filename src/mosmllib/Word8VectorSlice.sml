(* Word8VectorSlice -- SML Basis Library 
   sestoft@dina.kvl.dk 2000-10-24
*)

type elem = Word8.word
type vector = Word8Vector.vector

local
    prim_val vector_ : int -> vector                 = 1 "create_string";
    prim_val sub_    : vector -> int -> elem         = 2 "get_nth_char";
    prim_val update_ : vector -> int -> elem -> unit = 3 "set_nth_char";
    prim_val blit_   : vector -> int -> vector -> int -> int -> unit 
                                                     = 5 "blit_string";
    prim_val magic : 'a -> 'b = 1 "identity";
in

type slice = vector * int * int

(* Invariant on values (a, i, n) of type slice:
 *                  0 <= i <= i+n <= Word8Vector.length a, 
 * or equivalently, 0 <= i and 0 <= n and i+n <= Word8Vector.length a.  
 *)

fun length (a, i, n) = n;

fun sub((a', i', n'), i) = 
    if i<0 orelse i >= n' then raise Subscript
    else sub_ a' (i'+i);

fun slice (a, i, len) =
    let val alen = Word8Vector.length a
    in 
	case len of 
	    NONE   => if 0<=i andalso i<=alen then (a, i, alen - i)
		      else raise Subscript
	  | SOME n => if 0<=i andalso 0<=n andalso n<=alen-i then (a, i, n)
		      else raise Subscript
    end;

fun full a = (a, 0, Word8Vector.length a);

fun subslice ((a, i, n), i', NONE) =
    if 0<=i' andalso i'<=n then (a, i+i', n-i')
    else raise Subscript
  | subslice ((a, i, n), i', SOME n') =
    if 0<=i' andalso 0<=n' andalso n'<=n-i' then (a, i+i', n')
    else raise Subscript;
		      
fun base sli = sli;

fun vector (a : vector, i, n) = 
    let val newvec = vector_ n : vector
	fun copy j = 
	    if j<n then
		(update_ newvec j (sub_ a (i+j)); copy (j+1))
	    else
		()
    in copy 0; newvec end;

fun isEmpty (_, _, n) = n=0;

fun concat slis =
    let fun acc [] len                 = len
          | acc ((_, _, len1)::vr) len = acc vr (len1 + len)
        val len = acc slis 0
        val newvec = if len > Word8Vector.maxLen then raise Size 
		     else vector_ len : vector
        fun copyall to []                   = () (* Now: to = len *)
          | copyall to ((v1, i1, n1)::slir) = 
	    let fun copyv1 j = 
		if j<n1 then
		    (update_ newvec (to+j) (sub_ v1 (i1+j)); copyv1 (j+1))
		else
		    ()
	    in 
		(copyv1 0; copyall (to+n1) slir)
	    end
    in copyall 0 slis; newvec end;

fun getItem (a, i, 0) = NONE
  | getItem (a, i, n) = SOME(sub_ a i, (a, i+1, n-1));

fun find (p : elem -> bool) ((a,i,n) : slice) : elem option = 
    let val stop = i+n
	fun lr j = 
	    if j < stop then 
		if p (sub_ a j) then SOME (sub_ a j) else lr (j+1)
	    else NONE
    in lr i end;

fun exists (p : elem -> bool) ((a,i,n) : slice) : bool = 
    let val stop = i+n
	fun lr j = j < stop andalso (p (sub_ a j) orelse lr (j+1))
    in lr i end;

fun all (p : elem -> bool) ((a,i,n) : slice) : bool = 
    let val stop = i+n
	fun lr j = j >= stop orelse (p (sub_ a j) andalso lr (j+1))
    in lr i end;

fun app f (a, i, n) = 
    let val stop = i+n
	fun lr j = if j < stop then (f(sub_ a j); lr (j+1))
		   else ()
    in lr i end;

fun map (f : elem -> elem) (a : vector, i, n) : vector = 
    let val newvec = vector_ n : vector
	val stop = i+n
	fun lr j = 
	    if j < stop then 
		(update_ newvec (j-i) (f(sub_ a j)); lr (j+1))
	    else 
		()
    in lr i; newvec end;

fun foldl f e (a, i, n) = 
    let val stop = i+n
	fun lr j res = if j < stop then lr (j+1) (f(sub_ a j, res))
		       else res
    in lr i e end;

fun foldr f e (a, i, n) =
    let fun rl j res = if j >= i then rl (j-1) (f(sub_ a j, res))
		       else res
    in rl (i+n-1) e end;

fun findi (p : int * elem -> bool) ((a,i,n) : slice) : (int * elem) option = 
    let val stop = i+n
	fun lr j = 
	    if j < stop then 
		if p (j, sub_ a j) then SOME (j, sub_ a j) else lr (j+1)
	    else 
		NONE
    in lr i end;

fun appi f (a, i, n) = 
    let val stop = i+n
	fun lr j = 
	    if j < stop then (f(j, sub_ a j); lr (j+1)) 
	    else ()
    in lr i end;

fun mapi (f : int * elem -> elem) (a : vector, i, n) = 
    let val newvec = vector_ n : vector
	val stop = i+n
	fun lr j = 
	    if j < stop then 
		(update_ newvec (j-i) (f(j, sub_ a j)); lr (j+1))
	    else 
		()
    in lr i; newvec end;

fun foldli f e (a, i, n) = 
    let val stop = i+n
	fun lr j res = 
	    if j < stop then lr (j+1) (f(j, sub_ a j, res))
	    else res
    in lr i e end;

fun foldri f e (a, i, n) = 
    let fun rl j res = 
	    if j >= i then rl (j-1) (f(j, sub_ a j, res))
	    else res
    in rl (i+n-1) e end;

fun collate cmp ((a1,i1,n1), (a2,i2,n2)) =
    let val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point a1[i1..i1+j-1] = a2[i2..i2+j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
		case cmp(sub_ a1 (i1+j), sub_ a2 (i2+j)) of
		    EQUAL => h (j+1)
		  | res   => res
    in h 0 end;

end
