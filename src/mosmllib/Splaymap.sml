(* Splaymap -- modified for Moscow ML from 
 * SML/NJ library v. 0.2 which is 
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  
 * See file mosml/copyrght/copyrght.att for details.
 *
 * Applicative map (or, dictionary) structure implemented by splay-trees.
 *)

open Splaytree

datatype ('key, 'a) dict = 
  DICT of {cmpKey : 'key * 'key -> order,
	   root   : ('key * 'a) splay ref,
	   nobj   : int}

exception NotFound

fun cmpf cmpKey k = fn (k',_) => cmpKey(k',k)

fun mkDict cmpKey = DICT{cmpKey = cmpKey, root = ref SplayNil, nobj = 0}

(* Insert an item. *)
fun insert (DICT{cmpKey,root,nobj},key,v) =
    case splay (cmpf cmpKey key, !root) of
        (_,SplayNil) => 
          DICT{cmpKey=cmpKey,nobj=1,
	       root=ref(SplayObj{value=(key,v),left=SplayNil,right=SplayNil})}
      | (EQUAL,SplayObj{value,left,right}) => 
          DICT{cmpKey=cmpKey,nobj=nobj,
	       root=ref(SplayObj{value=(key,v),left=left,right=right})}
      | (LESS,SplayObj{value,left,right}) => 
          DICT{cmpKey=cmpKey,
	       nobj=nobj+1,
	       root=ref(SplayObj{value=(key,v),
				 left=SplayObj{value=value,
					       left=left,
					       right=SplayNil},
				 right=right})
          }
      | (GREATER,SplayObj{value,left,right}) => 
	    DICT{cmpKey=cmpKey,
		 nobj=nobj+1,
		 root=ref(SplayObj{value=(key,v),
				   left=left,
				   right=SplayObj{value=value,
						  left=SplayNil,
						  right=right}
            })
          }

(* Find an item, raising NotFound if not found *)
fun find (d as DICT{cmpKey,root,nobj},key) =
      case splay (cmpf cmpKey key, !root) of
        (_,SplayNil) => raise NotFound
      | (EQUAL,r as SplayObj{value,...}) => (root := r; #2 value)
      | (_,r) => (root := r; raise NotFound)

	(* Look for an item, return NONE if the item doesn't exist *)
fun peek arg = (SOME(find arg)) handle NotFound => NONE

(* Remove an item.  Raise NotFound if not found. *)
fun remove (DICT{cmpKey,root,nobj}, key) = 
    (case (splay (cmpf cmpKey key, !root))
	   of (_,SplayNil) => raise NotFound
	    | (EQUAL,SplayObj{value,left,right}) => 
		  (DICT{cmpKey=cmpKey,
			root=ref(join(left,right)),
			nobj=nobj-1}, #2 value)
	    | (_,r) => (root := r; raise NotFound))

(* Return the number of items in the table *)
fun numItems (DICT{nobj,...}) = nobj
    
(* Return a list of the items (and their keys) in the dictionary *)
fun listItems (DICT{root,...}) =
    let fun apply SplayNil                     res = res
          | apply (SplayObj{value,left,right}) res =
	    apply left (value :: apply right res)
    in apply (!root) [] end

(* Apply a function to the entries of the dictionary *)
fun app af (DICT{root,...}) =
      let fun apply SplayNil = ()
            | apply (SplayObj{value,left,right}) = 
                (apply left; af value; apply right)
    in
      apply (!root)
    end

fun revapp af (DICT{root,...}) =
    let fun apply SplayNil = ()
	  | apply (SplayObj{value,left,right}) = 
	    (apply right; af value; apply left)
    in apply (!root) end

(* Fold function *)
fun foldr abf b (DICT{root,...}) =
    let fun apply SplayNil                     res = res
	  | apply (SplayObj{value,left,right}) res =
	    apply left (abf(#1 value, #2 value, apply right res))
    in apply (!root) b end

fun foldl abf b (DICT{root,...}) =
    let fun apply SplayNil                     res = res
	  | apply (SplayObj{value,left,right}) res =
	    apply right (abf(#1 value, #2 value, apply left res))
    in apply (!root) b end

(* Map a table to a new table that has the same keys*)
fun map af (DICT{cmpKey,root,nobj}) =
    let fun ap SplayNil                     = SplayNil
	  | ap (SplayObj{value,left,right}) = 
	    let val left' = ap left
		val value' = (#1 value, af value)
	    in SplayObj{value = value', left = left', right = ap right} end
    in
      DICT{cmpKey=cmpKey,root = ref(ap (!root)), nobj = nobj}
    end

fun transform af (DICT{cmpKey,root,nobj}) =
    let fun ap SplayNil = SplayNil
	  | ap (SplayObj{value,left,right}) = 
	    let
		val left' = ap left
		val value' = (#1 value, af (#2 value))
	    in
		SplayObj{value = value', left = left', right = ap right}
	    end
    in DICT{cmpKey=cmpKey, root = ref(ap (!root)), nobj = nobj} end
