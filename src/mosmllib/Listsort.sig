(* Listsort *)

val sort      : ('a * 'a -> order) -> 'a list -> 'a list
val sorted    : ('a * 'a -> order) -> 'a list -> bool
val merge     : ('a * 'a -> order) -> 'a list * 'a list -> 'a list
val mergeUniq : ('a * 'a -> order) -> 'a list * 'a list -> 'a list
val eqclasses : ('a * 'a -> order) -> 'a list -> 'a list list

(* 
   [sort ordr xs] sorts the list xs in nondecreasing order, using the
   given ordering.  Uses Richard O'Keefe's smooth applicative merge
   sort.

   [sorted ordr xs] checks that the list xs is sorted in nondecreasing
   order, in the given ordering.

   [merge ordr (xs, ys)] returns a sorted list of the elements of the
   sorted lists xs and ys, preserving duplicates.  Both xs and ys must
   be sorted by ordr, that is, must satisfy
      sorted ordr xs andalso sorted ordr ys
   Then the result satisfies 
      sorted ordr (merge ordr (xs, ys))

   [mergeUniq ordr (xs, ys)] returns a sorted list of the elements of
   the sorted lists xs and ys, without duplicates.  Both xs and ys must
   be sorted by ordr.

   [eqclasses ordr xs] returns a list [xs1, xs2, ..., xsn] of
   non-empty equivalence classes of xs, obtained by sorting the list 
   and then grouping consecutive runs of elements that are EQUAL by ordr.
   If ordr is a total order, then it holds for xi in xsi and xj in xsj:
      ordr(xi, xj) = EQUAL   iff i=j and 
      ordr(xi, xj) = LESS    iff i<j and 
      ordr(xi, xj) = GREATER iff i>j 
   Thus ordr(xi, xj) = Int.compare(i, j).  A list of representatives
   for the equivalence classes of xs under ordering ordr can be
   obtained by
      List.map List.hd (eqclasses ordr xs) 
*)
