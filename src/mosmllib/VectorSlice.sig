(* VectorSlice -- SML Basis Library *)

type 'a slice

val length   : 'a slice -> int
val sub      : 'a slice * int -> 'a
val slice    : 'a Vector.vector * int * int option -> 'a slice
val full     : 'a Vector.vector -> 'a slice
val subslice : 'a slice * int * int option -> 'a slice
val base     : 'a slice -> 'a Vector.vector * int * int
val vector   : 'a slice -> 'a Vector.vector
val concat   : 'a slice list -> 'a Vector.vector
val isEmpty  : 'a slice -> bool
val getItem  : 'a slice -> ('a * 'a slice) option

val find     : ('a -> bool) -> 'a slice -> 'a option
val exists   : ('a -> bool) -> 'a slice -> bool
val all      : ('a -> bool) -> 'a slice -> bool

val app      : ('a -> unit) -> 'a slice -> unit
val map      : ('a -> 'b) -> 'a slice -> 'b Vector.vector
val foldl    : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b
val foldr    : ('a * 'b -> 'b) -> 'b -> 'a slice -> 'b

val findi    : (int * 'a -> bool) -> 'a slice -> (int * 'a) option
val appi     : (int * 'a -> unit) -> 'a slice -> unit
val mapi     : (int * 'a -> 'b) -> 'a slice -> 'b Vector.vector
val foldli   : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b
val foldri   : (int * 'a * 'b -> 'b) -> 'b -> 'a slice -> 'b

val collate  : ('a * 'a -> order) -> 'a slice * 'a slice -> order

(* 
   ['ty slice] is the type of vector slices, that is, sub-vectors.  
   The slice (a,i,n) is valid if 0 <= i <= i+n <= size s, 
                or equivalently, 0 <= i and 0 <= n and i+n <= size s.  
   A valid slice sli = (a,i,n) represents the sub-vector a[i...i+n-1],
   so the elements of sli are a[i], a[i+1], ..., a[i+n-1], and n is
   the length of the slice.  Only valid slices can be constructed by
   the functions below.

   [length sli] returns the number n of elements in sli = (s,i,n).

   [sub (sli, k)] returns the k'th element of the slice, that is,
   a(i+k) where sli = (a,i,n).  Raises Subscript if k<0 or k>=n.

   [slice (a, i, NONE)] creates the slice (a, i, length a-i),
   consisting of the tail of a starting at i.  
   Raises Subscript if i<0 or i > Vector.length a.  
   Equivalent to slice (a, i, SOME(Vector.length a - i)).

   [slice (a, i, SOME n)] creates the slice (a, i, n), consisting of
   the sub-vector of a with length n starting at i.  Raises Subscript
   if i<0 or n<0 or i+n > Vector.length a.  

       slice             meaning 
       -----------------------------------------------------------
       (a, 0, NONE)      the whole vector              a[0..len-1]   
       (a, 0, SOME n)    a left sub-vector (prefix)    a[0..n-1]
       (a, i, NONE)      a right sub-vector (suffix)   a[i..len-1]
       (a, i, SOME n)    a general slice               a[i..i+n-1] 

   [full a] creates the slice (a, 0, Vector.length a).  
   Equivalent to slice(a,0,NONE)

   [subslice (sli, i', NONE)] returns the slice (a, i+i', n-i') when
   sli = (a,i,n).  Raises Subscript if i' < 0 or i' > n.

   [subslice (sli, i', SOME n')] returns the slice (a, i+i', n') when
   sli = (a,i,n).  Raises Subscript if i' < 0 or n' < 0 or i'+n' > n.

   [base sli] is the concrete triple (a, i, n) when sli = (a, i, n).

   [vector sli] creates and returns a vector consisting of the
   elements of the slice, that is, a[i..i+n-1] when sli = (a,i,n).

   [concat slis] creates a vector containing the concatenation of the
   slices in slis.

   [isEmpty sli] returns true if the slice sli = (a,i,n) is empty,
   that is, if n=0.

   [getItem sli] returns SOME(x, rst) where x is the first element and
   rst the remainder of sli, if sli is non-empty; otherwise returns
   NONE.  

   [find p sli] applies p to each element x of sli, from left to
   right, until p(x) evaluates to true; returns SOME x if such an x
   exists, otherwise NONE.

   [exists p sli] applies p to each element x of sli, from left to right,
   until p(x) evaluates to true; returns true if such an x exists,
   otherwise false.

   [all p sli] applies p to each element x of sli, from left to right,
   until p(x) evaluates to false; returns false if such an x exists,
   otherwise true.

   [app f sli] applies f to all elements of sli = (a,i,n), from
   left to right.  That is, applies f to a[j+i] for j=0,1,...,n.

   [map f sli] applies f to all elements of sli = (a,i,n), from left
   to right, and returns a vector of the results.

   [foldl f e sli] folds function f over sli = (a,i,n) from left to right.  
   That is, computes f(a[i+n-1], f(a[i+n-2],..., f(a[i+1], f(a[i], e))...)).

   [foldr f e sli] folds function f over sli = (a,i,n) from right to left.  
   That is, computes f(a[i], f(a[i+1],..., f(a[i+n-2], f(a[i+n-1], e))...)).

   The following iterators generalize the above ones by also passing
   the index into the vector a underlying the slice to the function
   being iterated.

   [findi p sli] applies p to the elements of sli = (a,i,n) and the
   underlying vector indices, and returns the least (j, a[j]) for
   which p(j, a[j]) evaluates to true, if any; otherwise returns NONE.
   That is, evaluates p(j, a[j]) for j=i,..i+n-1 until it evaluates to
   true for some j, then returns SOME(j, a[j]); otherwise returns NONE.

   [appi f sli] applies f to the slice sli = (a,i,n) and the
   underlying vector indices.  That is, applies f to successive pairs
   (j, a[j]) for j=i,i+1,...,i+n-1.

   [mapi f sli] applies f to the slice sli = (a,i,n) and the
   underlying vector indices, and returns a vector of the results.
   That is, applies f to successive pairs (j, a[j]) for
   j=i,i+1,...,i+n-1, and returns #[f(i,a[i]), ..., f(i+n-1,a[i+n-1])].

   [foldli f e sli] folds function f over the slice sli = (a,i,n) and
   the underlying vector indices from left to right.  That is, computes 
   f(i+n-1, a[i+n-1], f(..., f(i+1, a[i+1], f(i, a[i], e)) ...)).  

   [foldri f e sli] folds function f over the slice sli = (a,i,n) and
   the underlying vector indices from right to left.  That is, computes
   f(i, a[i], f(i+1, a[i+1], ..., f(i+n-1, a[i+n-1], e) ...)).
  
   [collate cmp (sli1, sli2)] returns LESS, EQUAL or GREATER according
   as sli1 precedes, equals or follows sli2 in the lexicographic
   ordering on slices induced by the ordering cmp on elements.
*)
