(* Weak --- weak pointers and arrays of weak pointers *)

(* Single weak pointers *)

type 'a weak
val weak    : 'a -> 'a weak
val set     : 'a weak * 'a -> unit
val get     : 'a weak -> 'a                  (* Raises Fail *)
val isweak  : 'a weak -> bool

(* Arrays of weak pointers *)

prim_EQtype 'a array

val maxLen  : int

val array   : int -> '_a array               (* Raises Size               *)
val sub     : 'a array * int -> 'a           (* Raises Fail and Subscript *)
val update  : 'a array * int * 'a -> unit    (* Raises Subscript          *)
val isdead  : 'a array * int -> bool         (* Raises Subscript          *)
val length  : 'a array -> int

val app     : ('a -> unit) -> 'a array -> unit
val foldl   : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
val foldr   : ('a * 'b -> 'b) -> 'b -> 'a array -> 'b
val modify  : ('a -> 'a) -> 'a array -> unit

val appi    : (int * 'a -> unit) -> 'a array * int * int option -> unit
val foldli  : (int * 'a * 'b -> 'b) -> 'b -> 'a array * int * int option 
              -> 'b
val foldri  : (int * 'a * 'b -> 'b) -> 'b -> 'a array * int * int option 
              -> 'b
val modifyi : (int * 'a -> 'a) -> 'a array * int * int option -> unit

(*
   ['a weak] is the type of weak pointers to objects of type 'a.  A
   weak pointer is a pointer that cannot itself keep an object alive.
   Hence the object pointed to by a weak pointer may be deallocated by
   the garbage collector if the object is reachable only by weak
   pointers.  In this case, subsequent accesses via the `get' function
   will raise Fail "Dangling weak pointer".  (We raise an exception
   instead of returning an option value, because access via a weak
   pointer to a deallocated object is likely to be a programming
   error).

   Integers, characters, words and booleans will not be deallocated by
   the garbage collector and will remain reachable forever by a weak
   pointer.  Reals, strings, tuples and other non-nullary constructors
   may be deallocated by the garbage collector.  Constants, even
   composite ones, will not be deallocated either.

   [weak v] creates and returns a weak pointer to value v.

   [get w] returns the value pointed to by weak pointer w, if the
   value is still alive.  Otherwise raises Fail "Dangling weak pointer".

   [set(w, v)] makes the weak pointer w point to the value v.

   [isweak w] returns true if the value pointed to by w is dead;
   returns false otherwise.  If an object is reported to be dead, it
   remains dead.  However, an object is reported to be live just if it
   has not yet been deallocated by the garbage collector.  The
   allocation of any new value may activate the garbage collector and
   cause the object to die.  Thus
        if not (isweak w) then get w else "blah" 
   will not raise exception Fail, whereas the following might:
        if not (isweak w) then ([1,2] @ [3,4]; get w) else "blah" 
   because evaluation of the list append may cause w to die.

   The value of isweak w is the same as that of
         (get w; false) handle Fail _ => true
   but evaluating the latter expression may have the side effect of
   keeping w alive for slightly longer, because a pointer to w is
   returned by get w.

   ---

   ['a array] is the type of arrays of weak pointers to objects of
   type 'a.

   A value of type 'a Weak.weak (above) is equivalent to, but more
   efficient than, a one-element 'a Weak.array.  On the other hand, an
   'a Weak.array is more efficient than an ('a Weak.weak) Array.array.

   [array n] creates an array of n weak pointers.  Initially, any
   access to the array will raise Fail.

   [sub(a, i)] returns the object pointed to by cell i (counting from
   0) of the array a, if it is live.  Raises Fail "Dangling weak
   pointer" if cell i has never been updated or if the object pointed
   to has been deallocated by the garbage collector.  Raises Subscript
   if i<0 or i>=length a.  To make `sub' infix, use the declaration
                             infix 9 sub

   [update(a, i, v)] updates cell i of array a to point (weakly) to
   the value v.  Raises Subscript if i<0 or i>=length a.

   [isdead(a, i)] returns true if the object in cell i of array a is
   dead, and false otherwise.  Analogous to isweak; see above.

   [length a] returns the number of elements in a.

   [maxLen] is the maximal number of elements in an array.

   The iterators described below operate on the live elements only.
   Note that an element a[k] may die in the course of folding f over
   earlier elements (e.g. a[1] ... a[k-1]).  Thus the functions should
   be used with great care.

   [foldl f e a] folds function f over the live elements of a, from
   left to right.  

   [foldr f e a] folds function f over the live elements of a, from
   right to left.

   [app f a] applies f to the live elements of a from left to right.

   [modify f a] applies f to a[j] and updates a[j] with the result
   f(a[j]), for each live element a[j], from left to right.

   The following iterators generalize the above ones in two ways:

    . the index j is also being passed to the function being iterated;
    . the iterators work on a slice (subarray) of an array.

   The slice (a, i, SOME n) denotes the subarray a[i..i+n-1].  That is,
   a[i] is the first element of the slice, and n is the length of the
   slice.  Valid only if 0 <= i <= i+n <= length a.

   The slice (a, i, NONE) denotes the subarray a[i..length a-1].  That
   is, the slice denotes the suffix of the array starting at i.  Valid
   only if 0 <= i <= length a.  Equivalent to (a, i, SOME(length a - i)).

       slice             meaning 
       ----------------------------------------------------------
       (a, 0, NONE)      the whole array              a[0..len-1]   
       (a, 0, SOME n)    a left subarray (prefix)     a[0..n-1]
       (a, i, NONE)      a right subarray (suffix)    a[i..len-1]
       (a, i, SOME n)    a general slice              a[i..i+n-1] 

   [foldli f e (a, i, SOME n)] folds function f over the live elements
   of the subarray a[i..i+n-1] from left to right.  Raises Subscript
   if i<0 or n<0 or i+n > length a.

   [foldli f e (a, i, NONE)] folds function f over the live elements
   of the subarray a[i..len-1] from left to right, where len = length
   a.  Raises Subscript if i<0 or i > length a.

   [foldri f e (a, i, SOME n)] folds function f over the live elements
   of the subarray a[i..i+n-1] from right to left.  Raises Subscript
   if i<0 or n<0 or i+n > length a.

   [foldri f e (a, i, NONE)] folds function f over the live elements
   of the subarray a[i..len-1] from right to left, where len = length
   a.  Raises Subscript if i<0 or i > length a.

   [appi f (a, i, SOME n)] applies f to successive pairs (j, a[j]) for
   j=i,i+1,...,i+n-1, provided a[j] is live.  Raises Subscript if i<0
   or n<0 or i+n > length a.

   [appi f (a, i, NONE)] applies f to successive pairs (j, a[j]) for
   j=i,i+1,...,len-1, where len = length a, provided a[j] is live.
   Raises Subscript if i<0 or i > length a.

   [modifyi f (a, i, SOME n)] applies f to (j, a[j]) and updates a[j]
   with the result f(j, a[j]) for j=i,i+1,...,i+n-1, provided a[j] is
   live.  Raises Subscript if i<0 or n<0 or i+n > length a.

   [modifyi f (a, i, NONE)] applies f to (j, a[j]) and updates a[j]
   with the result f(j, a[j]) for j=i,i+1,...,len-1, provided a[j] is
   live.  Raises Subscript if i<0 or i > length a.  
*)
