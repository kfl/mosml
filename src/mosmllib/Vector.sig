(* Vector -- SML Basis Library *)

type 'a vector = 'a vector
val maxLen   : int

val fromList : 'a list -> 'a vector
val tabulate : int * (int -> 'a) -> 'a vector

val length   : 'a vector -> int
val sub      : 'a vector * int -> 'a
val update   : 'a vector * int * 'a -> 'a vector
val concat   : 'a vector list -> 'a vector

val find     : ('a -> bool) -> 'a vector -> 'a option
val exists   : ('a -> bool) -> 'a vector -> bool
val all      : ('a -> bool) -> 'a vector -> bool

val app      : ('a -> unit) -> 'a vector -> unit
val map      : ('a -> 'b) -> 'a vector -> 'b vector
val foldl    : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b
val foldr    : ('a * 'b -> 'b) -> 'b -> 'a vector -> 'b

val findi    : (int * 'a -> bool) -> 'a vector -> (int * 'a) option
val appi     : (int * 'a -> unit) -> 'a vector -> unit
val mapi     : (int * 'a -> 'b) -> 'a vector -> 'b vector
val foldli   : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b
val foldri   : (int * 'a * 'b -> 'b) -> 'b -> 'a vector -> 'b

val collate  : ('a * 'a -> order) -> 'a vector * 'a vector -> order

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

   [update(v, i, x)] creates a copy of v, sets position i to x, and
   returns the new vector.  Raises Subscript if i<0 or i>=length v.

   [concat vs] returns a vector which is the concatenation from left
   to right og the vectors in vs.  Raises Size if the sum of the
   sizes of the vectors in vs is larger than maxLen.

   [find p v] applies p to each element x of v, from left to right,
   until p(x) evaluates to true; returns SOME x if such an x exists,
   otherwise NONE.

   [exists p v] applies p to each element x of v, from left to right,
   until p(x) evaluates to true; returns true if such an x exists,
   otherwise false.

   [all p v] applies p to each element x of v, from left to right,
   until p(x) evaluates to false; returns false if such an x exists,
   otherwise true.

   [foldl f e v] folds function f over v from left to right.  That is,
   computes f(v[len-1], f(v[len-2], ..., f(v[1], f(v[0], e)) ...)),
   where len is the length of v.

   [foldr f e v] folds function f over v from right to left.  That is,
   computes f(v[0], f(v[1], ..., f(v[len-2], f(v[len-1], e)) ...)),
   where len is the length of v.

   [app f v] applies f to v[j] for j=0,1,...,length v-1.

   [map f v] applies f to v[j] for j=0,1,...,length v-1 and returns a 
   new vector containing the results.
   
   The following iterators generalize the above ones by passing also
   the vector element index j to the function being iterated.

   [findi p a] applies f to successive pairs (j, a[j]) for j=0,1,...,n-1, 
   until p(j, a[j]) evaluates to true; returns SOME (j, a[j]) if such
   a pair exists, otherwise NONE.

   [foldli f e v] folds function f over the vector from left to right.
   That is, computes f(n-1, v[n-1], f(..., f(1, v[1], f(0, v[0], e)) ...))  
   where n = length v.

   [foldri f e v] folds function f over the vector from right to left.  
   That is, computes f(0, v[0], f(1, v[1], ..., f(n-1, v[n-1], e) ...))
   where n = length v.

   [appi f v] applies f to successive pairs (j, v[j]) for j=0,1,...,n-1
   where n = length v.

   [mapi f v] applies f to successive pairs (j, v[j]) for
   j=0,1,...,n-1 where n = length v and returns a new vector
   containing the results.  

   [collate cmp (xs, ys)] returns LESS, EQUAL or GREATER according as
   xs precedes, equals or follows ys in the lexicographic ordering on
   vectors induced by the ordering cmp on elements.
*)
