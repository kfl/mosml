(* Splaymap -- applicative maps implemented by splay-trees       *)
(* From SML/NJ lib 0.2, copyright 1993 by AT&T Bell Laboratories *)

type ('key, 'a) dict

exception NotFound

val mkDict    : ('_key * '_key -> order) -> ('_key, '_a) dict
val insert    : ('_key, '_a) dict * '_key * '_a -> ('_key, '_a) dict
val find      : ('key, 'a) dict * 'key -> 'a
val peek      : ('key, 'a) dict * 'key -> 'a option
val remove    : ('_key, '_a) dict * '_key -> ('_key, '_a) dict * '_a
val numItems  : ('key, 'a) dict -> int
val listItems : ('key, 'a) dict -> ('key * 'a) list
val app       : ('key * 'a -> unit) -> ('key,'a) dict -> unit
val revapp    : ('key * 'a -> 'b) -> ('key,'a) dict -> unit
val foldr     : ('key * 'a * 'b -> 'b)-> 'b -> ('key,'a) dict -> 'b
val foldl     : ('key * 'a * 'b -> 'b) -> 'b -> ('key,'a) dict -> 'b
val map       : ('_key * 'a -> '_b) -> ('_key,'a) dict -> ('_key, '_b) dict
val transform : ('a -> '_b) -> ('_key,'a) dict -> ('_key, '_b) dict

(* 
   [('key, 'a) dict] is the type of applicative maps from domain type
   'key to range type 'a, or equivalently, applicative dictionaries
   with keys of type 'key and values of type 'a.  They are implemented
   as ordered splay-trees (Sleator and Tarjan).

   [mkDict ordr] returns a new, empty map whose keys have ordering
   ordr.

   [insert(m, i, v)] extends (or modifies) map m to map i to v.

   [find (m, k)] returns v if m maps k to v; otherwise raises NotFound.
   
   [peek(m, k)] returns SOME v if m maps k to v; otherwise returns NONE.

   [remove(m, k)] removes k from the domain of m and returns the
   modified map and the element v corresponding to k.  Raises NotFound
   if k is not in the domain of m.

   [numItems m] returns the number of entries in m (that is, the size
   of the domain of m).

   [listItems m] returns a list of the entries (k, v) of keys k and
   the corresponding values v in m, in increasing order of k.

   [app f m] applies function f to the entries (k, v) in m, in
   increasing order of k (according to the ordering ordr used to
   create the map or dictionary).

   [revapp f m] applies function f to the entries (k, v) in m, in
   decreasing order of k.

   [foldl f e m] applies the folding function f to the entries (k, v)
   in m, in increasing order of k.

   [foldr f e m] applies the folding function f to the entries (k, v)
   in m, in decreasing order of k.

   [map f m] returns a new map whose entries have form (k, f(k,v)),
   where (k, v) is an entry in m.

   [transform f m] returns a new map whose entries have form (k, f v),
   where (k, v) is an entry in m.
*)
