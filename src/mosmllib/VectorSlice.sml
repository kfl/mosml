(* VectorSlice -- SML Basis Library 
   sestoft@dina.kvl.dk 2000-10-18
*)

local
    type 'a vector = 'a Vector.vector;

    prim_val vector_ : int -> 'x -> 'a vector          = 2 "make_vect";
    prim_val subv_   : 'a vector -> int -> 'a          = 2 "get_vect_item";
    prim_val updatev : 'a vector -> int -> 'a -> unit  = 3 "set_vect_item";
in

type 'a slice = 'a vector * int * int

(* Invariant on values (a, i, n) of type 'a slice:
 *                  0 <= i <= i+n <= Vector.length a, 
 * or equivalently, 0 <= i and 0 <= n and i+n <= Vector.length a.  
 *)

fun length (a, i, n) = n;

fun sub((a', i', n'), i) = 
    if i<0 orelse i >= n' then raise Subscript
    else subv_ a' (i'+i);

fun slice (a, i, len) =
    let val alen = Vector.length a
    in 
	case len of 
	    NONE   => if 0<=i andalso i<=alen then (a, i, alen - i)
		      else raise Subscript
	  | SOME n => if 0<=i andalso 0<=n andalso n<=alen-i then (a, i, n)
		      else raise Subscript
    end;

fun full a = (a, 0, Vector.length a);

fun subslice ((a, i, n), i', NONE) =
    if 0<=i' andalso i'<=n then (a, i+i', n-i')
    else raise Subscript
  | subslice ((a, i, n), i', SOME n') =
    if 0<=i' andalso 0<=n' andalso n'<=n-i' then (a, i+i', n')
    else raise Subscript;
		      
fun base sli = sli;

fun vector (a : 'a vector, i, n) = 
    let val newvec = vector_ n () : 'a vector
	fun copy j = 
	    if j<n then
		(updatev newvec j (subv_ a (i+j)); copy (j+1))
	    else
		()
    in copy 0; newvec end;

fun isEmpty (_, _, n) = n=0;

fun concat slis =
    let fun acc [] len                 = len
          | acc ((_, _, len1)::vr) len = acc vr (len1 + len)
        val len = acc slis 0
        val newvec = if len > Vector.maxLen then raise Size 
		     else vector_ len () : 'a vector
        fun copyall to []                   = () (* Now: to = len *)
          | copyall to ((v1, i1, n1)::slir) = 
	    let fun copyv1 j = 
		if j<n1 then
		    (updatev newvec (to+j) (subv_ v1 (i1+j)); copyv1 (j+1))
		else
		    ()
	    in 
		(copyv1 0; copyall (to+n1) slir)
	    end
    in copyall 0 slis; newvec end;

fun getItem (a, i, 0) = NONE
  | getItem (a, i, n) = SOME(subv_ a i, (a, i+1, n-1));

fun find (p : 'a -> bool) ((a,i,n) : 'a slice) : 'a option = 
    let val stop = i+n
	fun lr j = 
	    if j < stop then 
		if p (subv_ a j) then SOME (subv_ a j) else lr (j+1)
	    else NONE
    in lr i end;

fun exists (p : 'a -> bool) ((a,i,n) : 'a slice) : bool = 
    let val stop = i+n
	fun lr j = j < stop andalso (p (subv_ a j) orelse lr (j+1))
    in lr i end;

fun all (p : 'a -> bool) ((a,i,n) : 'a slice) : bool = 
    let val stop = i+n
	fun lr j = j >= stop orelse (p (subv_ a j) andalso lr (j+1))
    in lr i end;

fun app f (a, i, n) = 
    let val stop = i+n
	fun lr j = if j < stop then (f(subv_ a j); lr (j+1))
		   else ()
    in lr i end;

fun map (f : 'a -> 'b) (a : 'a vector, i, n) = 
    let val newvec = vector_ n () : 'b vector
	val stop = i+n
	fun lr j = 
	    if j < stop then 
		(updatev newvec (j-i) (f(subv_ a j)); lr (j+1))
	    else 
		()
    in lr i; newvec end;

fun foldl f e (a, i, n) = 
    let val stop = i+n
	fun lr j res = if j < stop then lr (j+1) (f(subv_ a j, res))
		       else res
    in lr i e end;

fun foldr f e (a, i, n) =
    let fun rl j res = if j >= i then rl (j-1) (f(subv_ a j, res))
		       else res
    in rl (i+n-1) e end;

fun findi (p : int * 'a -> bool) ((a,i,n) : 'a slice) : (int * 'a) option = 
    let val stop = i+n
	fun lr j = 
	    if j < stop then 
		if p (j, subv_ a j) then SOME (j, subv_ a j) else lr (j+1)
	    else 
		NONE
    in lr i end;

fun appi f (a, i, n) = 
    let val stop = i+n
	fun lr j = 
	    if j < stop then (f(j, subv_ a j); lr (j+1)) 
	    else ()
    in lr i end;

fun mapi (f : int * 'a -> 'b) (a : 'a vector, i, n) = 
    let val newvec = vector_ n () : 'b vector
	val stop = i+n
	fun lr j = 
	    if j < stop then 
		(updatev newvec (j-i) (f(j, subv_ a j)); lr (j+1))
	    else 
		()
    in lr i; newvec end;

fun foldli f e (a, i, n) = 
    let val stop = i+n
	fun lr j res = 
	    if j < stop then lr (j+1) (f(j, subv_ a j, res))
	    else res
    in lr i e end;

fun foldri f e (a, i, n) = 
    let fun rl j res = 
	    if j >= i then rl (j-1) (f(j, subv_ a j, res))
	    else res
    in rl (i+n-1) e end;

fun collate cmp ((a1,i1,n1), (a2,i2,n2)) =
    let val stop = if n1 < n2 then n1 else n2
	fun h j = (* At this point a1[i1..i1+j-1] = a2[i2..i2+j-1] *)
	    if j = stop then if      n1 < n2 then LESS
                             else if n1 > n2 then GREATER
                             else                 EQUAL
	    else
		case cmp(subv_ a1 (i1+j), subv_ a2 (i2+j)) of
		    EQUAL => h (j+1)
		  | res   => res
    in h 0 end;

end
