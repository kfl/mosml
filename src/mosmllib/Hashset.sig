(* Hashset -- sets implemented by hashtables *)

signature Hashset = sig
type 'item set

exception NotFound

val empty        : ('_item -> word) * ('_item * '_item -> bool) -> '_item set
val singleton    : ('_item -> word) * ('_item * '_item -> bool) -> '_item 
                   -> '_item set

val member       : '_item set * '_item -> bool
val retrieve     : '_item set * '_item -> '_item
val peek         : '_item set * '_item -> '_item option

val add          : '_item set * '_item -> unit
val addList      : '_item set * '_item list -> unit
val delete       : '_item set * '_item -> unit

val isEmpty      : '_item set -> bool
val isSubset     : '_item set * '_item set -> bool
val equal        : '_item set * '_item set -> bool
val numItems     : '_item set ->  int
val listItems    : '_item set -> '_item list

val app          : ('_item -> unit) -> '_item set -> unit
val fold         : ('_item * 'b -> 'b) -> 'b -> '_item set -> 'b
val all          : ('_item -> bool) -> '_item set -> bool
val exists       : ('_item -> bool) -> '_item set -> bool
val find         : ('_item -> bool) -> '_item set -> '_item option
val copy         : '_item set -> '_item set

val hash         : '_item set -> word
val polyHash     : 'a -> word

end

(* 
   ['item set] is the type of sets of elements of type 'item, with a
   given hash function and equality predicate.

   [empty (hash, equal)] creates a new empty set with the given hash
   function and equality predicate.  It must hold that equal(x, y)
   implies hash x = hash y.

   [singleton (hash, equal) i] creates the singleton set containing i,
   with the given hash function and equality predicate.

   [member(s, i)] returns true if and only if i is in s.

   [retrieve(s, i)] returns i if it is in s; raises NotFound otherwise.

   [peek(s, i)] returns SOME i if i is in s; returns NONE otherwise.

   [add(s, i)] adds item i to set s.  

   [addList(s, xs)] adds all items from the list xs to the set s.

   [delete(s, i)] removes item i from s.  Raises NotFound if i is not in s.

   [isEmpty s] returns true if the set is empty; false otherwise.

   [equal(s1, s2)] returns true if and only if the two sets have the
   same elements.  

   [isSubset(s1, s2)] returns true if and only if s1 is a subset of s2.
   
   [numItems s] returns the number of items in set s.

   [listItems s] returns a list of the items in set s, in some order.

   [app f s] applies function f to the elements of s, in some order.

   [fold f e s] applies the folding function f to the entries of the
   set in some order.

   [find p s] returns SOME i, where i is an item in s which satisfies
   p, if one exists; otherwise returns NONE.  

   [hash s] returns the hashcode of the set, which is the sum of the
   hashcodes of its elements, as computed by the hash function given
   when the set was created.  

   [polyHash v] returns a system-defined hashcode for the value v.
   This pseudo-polymorphic hash function can be used together with the
   standard equality function (=) to create a Hashset for any type that 
   admits equality, as follows:

       val set = Hashset.empty (Hashset.hash, op =);
*)
