(* ListPair -- SML Basis Library *)

val zip    : 'a list * 'b list -> ('a * 'b) list
val unzip  : ('a * 'b) list -> 'a list * 'b list
val map    : ('a * 'b -> 'c)   -> 'a list * 'b list -> 'c list
val app    : ('a * 'b -> unit) -> 'a list * 'b list -> unit
val all    : ('a * 'b -> bool) -> 'a list * 'b list -> bool
val exists : ('a * 'b -> bool) -> 'a list * 'b list -> bool
val foldr  : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
val foldl  : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c

val allEq    : ('a * 'b -> bool) -> 'a list * 'b list -> bool

exception UnequalLengths

val zipEq    : ('a list * 'b list) -> ('a * 'b) list
val mapEq    : ('a * 'b -> 'c) -> 'a list * 'b list -> 'c list
val appEq    : ('a * 'b -> 'c) -> 'a list * 'b list -> unit
val foldrEq  : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c
val foldlEq  : ('a * 'b * 'c -> 'c) -> 'c -> 'a list * 'b list -> 'c

(* 
   These functions process pairs (xs, ys) of lists.  
   There are three groups of functions:

     * zip, map, app, all, exists, foldr and foldl raise no exception
       when the argument lists are found to be of unequal length; the
       excess elements from the longer list are simply disregarded.

     * zipEq, mapEq, appEq, foldrEq and foldlEq raise exception
       UnequalLengths when the argument lists are found to be of
       unequal length.

     * allEq raises no exception but returns false if the lists are
       found to have unequal lengths (after traversing the lists).

   [zip (xs, ys)] returns the list of pairs of corresponding elements
   from xs and ys.  

   [unzip xys] returns a pair (xs, ys), where xs is the list of first
   components of xys, and ys is the list of second components from
   xys.  Hence zip (unzip xys) has the same result and effect as xys.

   [map f (xs, ys)] applies function f to the pairs of corresponding
   elements of xs and ys from left to right and returns the list of
   results.  Hence map f (xs, ys) has the same result and effect as
   List.map f (zip (xs, ys)).

   [app f (xs, ys)] applies function f to the pairs of corresponding
   elements of xs and ys from left to right and returns ().  Hence 
   app f (xs, ys) has the same result and effect as 
   List.app f (zip (xs, ys)).

   [all p (xs, ys)] applies predicate p to the pairs of corresponding
   elements of xs and ys from left to right until p evaluates to false
   or one or both lists is exhausted; returns true if p is true of all
   such pairs; otherwise false.  Hence all p (xs, ys) has the same
   result and effect as List.all p (zip (xs, ys)).

   [exists p (xs, ys)] applies predicate p to the pairs of
   corresponding elements of xs and ys from left to right until p
   evaluates to true or one or both lists is exhausted; returns true
   if p is true of any such pair; otherwise false.  
   Hence exists p (xs, ys) has the same result and effect as 
   List.exists p (zip (xs, ys)).  Also, exists p (xs, ys) is equivalent 
   to not(all (not o p) (xs, ys)).

   [foldr f e (xs, ys)] evaluates f(x1, y1, f(x2, y2, f(..., f(xn, yn, e))))
   where xs = [x1, x2, ..., x(n-1), xn, ...],
         ys = [y1, y2, ..., y(n-1), yn, ...], 
   and    n = min(length xs, length ys).
   Equivalent to List.foldr (fn ((x, y), r) => f(x, y, r)) e (zip(xs, ys)).

   [foldl f e (xs, ys)] evaluates f(xn, yn, f( ..., f(x2, y2, f(x1, y1, e))))
   where xs = [x1, x2, ..., x(n-1), xn, ...], 
         ys = [y1, y2, ..., y(n-1), yn, ...], 
   and    n = min(length xs, length ys).
   Equivalent to List.foldl (fn ((x, y), r) => f(x, y, r)) e (zip(xs, ys)).

   [zipEq (xs, ys)] returns the list of pairs of corresponding
   elements from xs and ys.  Raises UnequalLengths if xs and ys do not
   have the same length.

   [mapEq f (xs, ys)] applies function f to pairs of corresponding
   elements of xs and ys from left to right, and then returns the list
   of results if xs and ys have the same length, otherwise raises
   UnequalLengths.  If f has no side effects and terminates, then
   it is equivalent to List.map f (zipEq (xs, ys)).

   [appEq f (xs, ys)] applies function f to pairs of corresponding
   elements of xs and ys from left to right, and then raises
   UnequalLengths if xs and ys have the same length. 

   [foldrEq f e (xs, ys)] raises UnequalLengths if xs and ys do not
   have the same length.  Otherwise evaluates 
         f(x1, y1, f(x2, y2, f(..., f(xn, yn, e))))
   where xs = [x1, x2, ..., x(n-1), xn],
         ys = [y1, y2, ..., y(n-1), yn], 
   and n = length xs = length ys.
   Equivalent to List.foldr (fn ((x,y),r) => f(x,y,r)) e (zipEq(xs, ys)).

   [foldlEq f e (xs, ys)] evaluates 
   f(xn, yn, f( ..., f(x2, y2, f(x1, y1, e))))
   where xs = [x1, x2, ..., x(n-1), xn, ...], 
         ys = [y1, y2, ..., y(n-1), yn, ...], 
   and    n = min(length xs, length ys).
   Then raises UnequalLengths if xs and ys do not have the same
   length.  If f has no side effects and terminates normally, then it is
   equivalent to List.foldl (fn ((x,y),r) => f(x,y,r)) e (zipEq(xs, ys)).

   [allEq p (xs, ys)] works as all p (xs, ys) but returns false if xs
   and ys do not have the same length.  Equivalent to 
       all p (xs, ys) andalso length xs = length ys.
*)
