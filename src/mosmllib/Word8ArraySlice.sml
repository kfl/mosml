(* Word8ArraySlice -- SML Basis Library 
   sestoft@dina.kvl.dk 2000-10-24
*)

type elem = Word8.word
type array = Word8Array.array
type vector = Word8Vector.vector
type vector_slice = Word8VectorSlice.slice

local
    prim_val magic   : 'a -> 'b                        = 1 "identity";

    prim_type array_;

    fun from_array (a : array)  = !(magic a) : array_;
    prim_val vector_  : int -> vector                 = 1 "create_string";
    prim_val sub_     : array_ -> int -> elem         = 2 "get_nth_char";
    prim_val subv_    : vector -> int -> elem         = 2 "get_nth_char";
    prim_val update_  : array_ -> int -> elem -> unit = 3 "set_nth_char";
    prim_val updatev  : vector -> int -> elem -> unit = 3 "set_nth_char";
    prim_val length_  : array_ -> int                 = 1 "string_length";
in

type slice = array * int * int

(* Invariant on values (a, i, n) of type slice:
 *                  0 <= i <= i+n <= Word8Array.length a, 
 * or equivalently, 0 <= i and 0 <= n and i+n <= Word8Array.length a.  
 *)

fun length (a, i, n) = n;

fun sub((a', i', n'), i) = 
    if i<0 orelse i >= n' then raise Subscript
    else sub_ (from_array a') (i'+i);

fun update ((a', i', n'), i, v)  =
    if i<0 orelse i>=n' then raise Subscript 
    else update_ (from_array a') (i'+i) v;

fun slice (a, i, len) =
    let val alen = Word8Array.length a
    in 
	case len of 
	    NONE   => if 0<=i andalso i<=alen then (a, i, alen - i)
		      else raise Subscript
	  | SOME n => if 0<=i andalso 0<=n andalso n<=alen-i then (a, i, n)
		      else raise Subscript
    end;

fun full a = (a, 0, Word8Array.length a);

fun subslice ((a, i, n), i', NONE) =
    if 0<=i' andalso i'<=n then (a, i+i', n-i')
    else raise Subscript
  | subslice ((a, i, n), i', SOME n') =
    if 0<=i' andalso 0<=n' andalso n'<=n-i' then (a, i+i', n')
    else raise Subscript;
		      
fun base sli = sli;

fun vector (a : array, i, n) = 
    let val a = from_array a : array_ 
	val newvec = vector_ n : vector
	fun copy j = 
	    if j<n then
		(updatev newvec j (sub_ a (i+j)); copy (j+1))
	    else
		()
    in copy 0; newvec end;

fun copy {src=(a1,i1,n) : slice, dst=a2: array, di=i2} =
    let val a1 = from_array a1
	and a2 = from_array a2
    in
	if i2<0 orelse i2+n > length_ a2 then raise Subscript
	else if i1 < i2 then		(* copy from high to low *)
	         let fun hi2lo j = 
		     if j >= 0 then
			 (update_ a2 (i2+j) (sub_ a1 (i1+j)); hi2lo (j-1))
		     else ()
		 in hi2lo (n-1) end
	     else                       (* i1 >= i2, copy from low to high *)
		 let fun lo2hi j = 
		     if j < n then
			 (update_ a2 (i2+j) (sub_ a1 (i1+j)); lo2hi (j+1))
		     else ()
		 in lo2hi 0 end
    end;

fun copyVec {src : vector_slice, dst=a2: array, di=i2} =
    let val (a1, i1, n) = Word8VectorSlice.base src
	val a2 = from_array a2
    in
	if i2<0 orelse i2+n > length_ a2 then raise Subscript
	else 
	    let fun lo2hi j = if j < n then
		(update_ a2 (i2+j) (subv_ a1 (i1+j)); lo2hi (j+1))
			      else ()
	    in lo2hi 0 end
    end;

fun isEmpty (_, _, n) = n=0;

fun getItem (a, i, 0) = NONE
  | getItem (a, i, n) = SOME(sub_ (from_array a) i, (a, i+1, n-1));

fun find (p : elem -> bool) ((a,i,n) : slice) : elem option = 
    let val a = from_array a
	val stop = i+n
	fun lr j = 
	    if j < stop then 
		if p (sub_ a j) then SOME (sub_ a j) else lr (j+1)
	    else NONE
    in lr i end;

fun exists (p : elem -> bool) ((a,i,n) : slice) : bool = 
    let val a = from_array a
	val stop = i+n
	fun lr j = j < stop andalso (p (sub_ a j) orelse lr (j+1))
    in lr i end;

fun all (p : elem -> bool) ((a,i,n) : slice) : bool = 
    let val a = from_array a
	val stop = i+n
	fun lr j = j >= stop orelse (p (sub_ a j) andalso lr (j+1))
    in lr i end;

fun app f (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j = if j < stop then (f(sub_ a j); lr (j+1))
		   else ()
    in lr i end;

fun foldl f e (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j res = if j < stop then lr (j+1) (f(sub_ a j, res))
		       else res
    in lr i e end;

fun foldr f e (a, i, n) =
    let val a = from_array a
	fun rl j res = if j >= i then rl (j-1) (f(sub_ a j, res))
		       else res
    in rl (i+n-1) e end;

fun modify f (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j = if j < stop then (update_ a j (f(sub_ a j)); lr (j+1))
		   else ()
    in lr i end;

fun findi (p : int * elem -> bool) ((a,i,n) : slice) : (int * elem) option = 
    let val a = from_array a
	val stop = i+n
	fun lr j = 
	    if j < stop then 
		if p (j, sub_ a j) then SOME (j, sub_ a j) else lr (j+1)
	    else NONE
    in lr i end;

fun appi f (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j = 
	    if j < stop then (f(j, sub_ a j); lr (j+1)) 
	    else ()
    in lr i end;

fun foldli f e (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j res = 
	    if j < stop then lr (j+1) (f(j, sub_ a j, res))
	    else res
    in lr i e end;

fun foldri f e (a, i, n) = 
    let val a = from_array a
	fun rl j res = 
	    if j >= i then rl (j-1) (f(j, sub_ a j, res))
	    else res
    in rl (i+n-1) e end;

fun modifyi f (a, i, n) = 
    let val a = from_array a
	val stop = i+n
	fun lr j = 
	    if j < stop then (update_ a j (f(j, sub_ a j)); lr (j+1))
	    else ()
    in lr i end;

fun collate cmp ((a1,i1,n1), (a2,i2,n2)) =
    let val a1 = from_array a1
	and a2 = from_array a2
	val stop = if n1 < n2 then n1 else n2
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
