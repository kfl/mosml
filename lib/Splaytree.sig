(* Splaytree -- Splay tree structure supporting Splaymap and Splayset.
 * 
 * Modified for Moscow ML from SML/NJ library which is 
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  
 * See file mosml/copyrght/copyrght.att for details.
 *)

datatype 'a splay = 
    SplayObj of {value : 'a,
		 right : 'a splay,
		 left  : 'a splay}
  | SplayNil

val splay : ('a -> order) * 'a splay -> order * 'a splay
val join  : 'a splay * 'a splay -> 'a splay

(* 
   [splay (cmp,tree)] returns (r,tree') 
   where tree' is tree adjusted using the comparison function cmp
   and, if tree' = SplayObj{value,...}, then r = cmp value.
   tree' = SplayNil iff tree = SplayNil, in which case r is undefined.

   [join(t,t')] returns a new splay tree formed of t and t'
*)
