(* Rbset -- ordered sets implemented by red-black trees *)
(* Intention: should resemble SML/NJs ORD_SET signature *)

signature Rbset = sig
type 'item set

exception NotFound
exception NonMonotonic

val empty        : ('item * 'item -> order) -> 'item set
val singleton    : ('item * 'item -> order) -> 'item -> 'item set
val add          : 'item set * 'item -> 'item set
val add'         : 'item * 'item set -> 'item set
val addList      : 'item set * 'item list -> 'item set
val isEmpty      : 'item set -> bool
val isSubset     : 'item set * 'item set -> bool
val member       : 'item set * 'item -> bool
val delete       : 'item set * 'item -> 'item set
val numItems     : 'item set ->  int
val getOrder     : 'item set -> ('item * 'item -> order)
val union        : 'item set * 'item set -> 'item set
val intersection : 'item set * 'item set -> 'item set
val difference   : 'item set * 'item set -> 'item set
val listItems    : 'item set -> 'item list
val app          : ('item -> unit) -> 'item set -> unit
val revapp       : ('item -> unit) -> 'item set -> unit
val foldr        : ('item * 'b -> 'b) -> 'b -> 'item set -> 'b
val foldl        : ('item * 'b -> 'b) -> 'b -> 'item set -> 'b
val map          : ('item -> 'newitem) * ('newitem * 'newitem -> order) 
                   -> 'item set -> 'newitem set
val mapMono      : ('item -> 'newitem) * ('newitem * 'newitem -> order) 
                   -> 'item set -> 'newitem set
val find         : ('item -> bool) -> 'item set -> 'item option
val min          : 'item set -> 'item option
val max          : 'item set -> 'item option
val hash         : ('item -> word) -> 'item set -> word
val equal        : 'item set * 'item set -> bool
val compare      : 'item set * 'item set -> order 

val depth        : 'item set -> int

datatype 'item intv = 
    All
  | From of 'item
  | To   of 'item
  | FromTo of 'item * 'item

val subset  : 'item set * 'item intv -> 'item set
val sublist : 'item set * 'item intv -> 'item list

end

(* 

   ['item set] is the type of sets of ordered elements of type 'item.
   The ordering relation on the elements is used in the representation
   of the set.  The result of combining or comparing two sets with
   different underlying ordering relations is undefined.  The
   implementation uses Okasaki-style red-black trees.

   [empty ordr] creates a new empty set with the given ordering
   relation.  

   [singleton ordr i] creates the singleton set containing i, with the
   given ordering relation.

   [add(s, i)] adds item i to set s.  

   [addList(s, xs)] adds all items from the list xs to the set s.

   [isEmpty s] returns true if and only if the set is empty.

   [equal(s1, s2)] returns true if and only if the two sets have the
   same elements, as determined by the ordering relation given when
   the sets were created.  

   [isSubset(s1, s2)] returns true if and only if s1 is a subset of s2.

   [member(s, i)] returns true if and only if i is in s.

   [delete(s, i)] removes item i from s.  Raises NotFound if i is not in s.
   
   [numItems s] returns the number of items in set s.

   [union(s1, s2)] returns the union of s1 and s2.  

   [intersection(s1, s2)] returns the intersection of s1 and s2.

   [difference(s1, s2)] returns the difference between s1 and s2 (that
   is, the set of elements in s1 but not in s2).

   [listItems s] returns a list of the items in set s, in increasing
   order.

   [app f s] applies function f to the elements of s, in increasing
   order.

   [revapp f s] applies function f to the elements of s, in decreasing
   order. 

   [foldl f e s] applies the folding function f to the entries of the
   set in increasing order.

   [foldr f e s] applies the folding function f to the entries of the
   set in decreasing order. 

   [map (f, ordr) s] creates a new set with underlying ordering ordr
   by applying function f to all elements of the set s.

   [mapMono (f, ordr) s] creates a new set by applying the strictly
   monotonically increasing function f to all elements of s.  The new
   set will have ordering ordr.  This is faster than map (f, ordr) s by 
   a logarithmic factor, but the function must satisfy 
      ordr(f x, f y) = ordr'(x, y) 
   for all elements x, y in s, where ordr' is the ordering relation 
   on s; otherwise exception NonMonotonic is thrown.

   [find p s] returns SOME i, where i is an item in s which satisfies
   p, if one exists; otherwise returns NONE.  Traverses the entries of
   the set in increasing order.

   [min s] returns SOME i, where i is the least item in the set s, if s is 
   non-empty; returns NONE if s is empty.

   [max s] returns SOME i, where i is the greatest item in the set s,
   if s is non-empty; returns NONE if s is empty.

   [hashCode h s] returns the hashcode of the set, which is the sum of
   the hashcodes of its elements, as computed by the function h.

   [compare (s1, s2)] returns LESS, EQUAL or GREATER according as s1
   precedes, equals or follows s2 in the lexicographic ordering that
   would be obtained by comparing the sorted lists of elements of the
   two sets.  It holds that 
      equal(s1, s2)    if and only if compare(s1, s2) = EQUAL
      isSubset(s1, s2) implies compare(s1, s2) = LESS
      isSubset(s2, s1) implies compare(s1, s2) = GREATER

   [subset(s, intv)] returns a set of those elements of s that belong
   to the interval intv.  The intervals have the following meaning:

       All             denotes  all elements
       From e1         denotes  elements e for which cmp(e1, e) <> GREATER
       To e2           denotes  elements e for which cmp(e, e2) = LESS
       FromTo(e1, e2)  denotes  elements e for which cmp(e1, e) <> GREATER
                                                 and cmp(e, e2) = LESS

   [sublist(s, intv)] returns a list, in order, of those elements of s
   that belong to the interval intv.  Thus sublist(s, All) is equivalent 
   to listItems s.
*)
