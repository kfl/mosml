(* Vector -- SML Basis Library *)

type 'a vector = 'a vector
val maxLen   : int

val fromList : 'a list -> 'a vector
val tabulate : int * (int -> 'a) -> 'a vector

val length   : 'a vector -> int
val sub      : 'a vector * int -> 'a
val extract  : 'a vector * int * int option -> 'a vector
val concat   : 'a vector list -> 'a vector

val app      : ('a -> unit) -> 'a vector -> unit
val map      : ('a -> 'b) -> 'a vector -> 'b vector
val foldl    : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
val foldr    : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b

val appi     : (int * 'a -> unit) -> 'a vector * int * int option -> unit
val mapi     : (int * 'a -> 'b) -> 'a vector * int * int option -> 'b vector
val foldli   : (int * 'a * 'b -> 'b) -> 'b -> 'a vector*int*int option -> 'b
val foldri   : (int * 'a * 'b -> 'b) -> 'b -> 'a vector*int*int option -> 'b

(* 
   ['ty vector] is the type of one-dimensional, immutable, zero-based
   constant-time-access vectors with elements of type 'ty.  
   Type 'ty vector admits equality if 'ty does.  Vectors v1 and v2 are 
   equal if they have the same length and their elements are equal.

   [maxLen] is the maximal number of elements in a vector.

   [fromList xs] returns a vector whose elements are those of xs.
   Raises Size if length xs > maxLen.

   [tabulate(n, f)] returns a vector of length n whose elements
   are f 0, f 1, ..., f (n-1), created from left to right.  Raises
   Size if n<0 or n>maxLen.

   [length v] returns the number of elements in v.

   [sub(v, i)] returns the i'th element of v, counting from 0.
   Raises Subscript if i<0 or i>=length v.

   [extract(v, i, NONE)] returns a vector of the elements v[i..length v-1]
   of v.  Raises Subscript if i<0 or i>length v.

   [extract(v, i, SOME n)] returns a vector of the elements v[i..i+n-1]
   of v.  Raises Subscript if i<0 or n<0 or i+n>length v.

   [concat vs] returns a vector which is the concatenation from left
   to right og the vectors in vs.  Raises Size if the sum of the
   sizes of the vectors in vs is larger than maxLen.

   [foldl f e v] folds function f over v from left to right.  That is,
   computes f(v[len-1], f(v[len-2], ..., f(v[1], f(v[0], e)) ...)),
   where len is the length of v.

   [foldr f e v] folds function f over v from right to left.  That is,
   computes f(v[0], f(v[1], ..., f(v[len-2], f(v[len-1], e)) ...)),
   where len is the length of v.

   [app f v] applies f to v[j] for j=0,1,...,length v-1.

   [map f v] applies f to v[j] for j=0,1,...,length v-1 and returns a 
   new vector containing the results.
   

   The following iterators generalize the above ones in two ways:

    * the index j is also being passed to the function being iterated;
    * the iterators work on a slice (subvector) of a vector.

   The slice (v, i, SOME n) denotes the subvector v[i..i+n-1].  That is,
   v[i] is the first element of the slice, and n is the length of the
   slice.  Valid only if 0 <= i <= i+n <= length v.

   The slice (v, i, NONE) denotes the subvector v[i..length v-1].  That
   is, the slice denotes the suffix of the vector starting at i.  Valid
   only if 0 <= i <= length v.  Equivalent to (v, i, SOME(length v - i)).

       slice             meaning 
       ----------------------------------------------------------
       (v, 0, NONE)      the whole vector             v[0..len-1]   
       (v, 0, SOME n)    a left subvector (prefix)    v[0..n-1]
       (v, i, NONE)      a right subvector (suffix)   v[i..len-1]
       (v, i, SOME n)    a general slice              v[i..i+n-1] 

   [foldli f e (v, i, SOME n)] folds function f over the subvector
   v[i..i+n-1] from left to right.  That is, computes 
   f(i+n-1, v[i+n-1], f(..., f(i+1, v[i+1], f(i, v[i], e)) ...)).  
   Raises Subscript if i<0 or n<0 or i+n > length v.

   [foldli f e (v, i, NONE)] folds function f over the subvector
   v[i..len-1] from left to right, where len =  length v.  That is, 
   computes f(len-1, v[len-1], f(..., f(i+1, v[i+1], f(i, v[i], e)) ...)).  
   Raises Subscript if i<0 or i > length v.

   [foldri f e (v, i, SOME n)] folds function f over the subvector
   v[i..i+n-1] from right to left.  That is, computes 
   f(i, v[i], f(i+1, v[i+1], ..., f(i+n-1, v[i+n-1], e) ...)).
   Raises Subscript if i<0 or n<0 or i+n > length v.

   [foldri f e (v, i, NONE)] folds function f over the subvector
   v[i..len-1] from right to left, where len = length v.  That is, 
   computes f(i, v[i], f(i+1, v[i+1], ..., f(len-1, v[len-1], e) ...)).
   Raises Subscript if i<0 or i > length v.

   [appi f (v, i, SOME n)] applies f to successive pairs (j, v[j]) for
   j=i,i+1,...,i+n-1.  Raises Subscript if i<0 or n<0 or i+n > length v.

   [appi f (v, i, NONE)] applies f to successive pairs (j, v[j]) for
   j=i,i+1,...,len-1, where len = length v.  Raises Subscript if i<0
   or i > length v.

   [mapi f (v, i, SOME n)] applies f to successive pairs (j, v[j]) for 
   j=i,i+1,...,i+n-1 and returns a new vector (of length n) containing 
   the results.  Raises Subscript if i<0 or n<0 or i+n > length v.

   [mapi f (v, i, NONE)] applies f to successive pairs (j, v[j]) for 
   j=i,i+1,...,len-1, where len = length v, and returns a new vector
   (of length len-i) containing the results.  Raises Subscript if i<0
   or i > length v.
*)
