(* Splayset -- modified for Moscow ML 1995-04-22
 * from SML/NJ library v. 0.2
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  
 * See file mosml/copyrght/copyrght.att for details.
 *
 * Set of values with an ordering relation, implemented as splay-trees.
 *)

open Splaytree

datatype 'key set = 
  OS of {cmpKey : 'key * 'key -> order,
	 root   : 'key splay ref,
	 nobj   : int}

exception NotFound
fun cmpf cmpKey k = fn k' => cmpKey(k',k)

fun empty cmpKey = OS{cmpKey = cmpKey, root = ref SplayNil, nobj = 0}

fun singleton cmpKey v = 
    OS{cmpKey= cmpKey, 
       root = ref(SplayObj{value=v,left=SplayNil,right=SplayNil}),
       nobj=1}

(* Primitive insertion. *)
fun insert cmpKey (v,(nobj,root)) =
      case splay (cmpf cmpKey v, root) of
        (_,SplayNil) => 
          (1,SplayObj{value=v,left=SplayNil,right=SplayNil})
      | (EQUAL,SplayObj{value,left,right}) => 
          (nobj,SplayObj{value=v,left=left,right=right})
      | (LESS,SplayObj{value,left,right}) => 
          (nobj+1,
           SplayObj{
             value=v,
             left=SplayObj{value=value,left=left,right=SplayNil},
             right=right})
      | (GREATER,SplayObj{value,left,right}) => 
          (nobj+1,
           SplayObj{
              value=v,
              left=left,
              right=SplayObj{value=value,left=SplayNil,right=right}})

(* Add an item. *)
fun add (OS{cmpKey,root,nobj},v) = let
      val (cnt,t) = insert cmpKey (v,(nobj,!root))
      in
        OS{cmpKey=cmpKey, nobj=cnt, root=ref t}
      end

(* Insert a list of items. *)
fun addList (OS{cmpKey,root,nobj},l) = let
      val (cnt,t) = List.foldl (insert cmpKey) (nobj,!root) l
      in OS{cmpKey=cmpKey, nobj=cnt, root=ref t} end

(* Look for an item, return NONE if the item doesn't exist *)
fun peek (d as OS{cmpKey,root,nobj}, key) =
      case splay (cmpf cmpKey key, !root) of
        (_,SplayNil) => NONE
      | (EQUAL,r as SplayObj{value,...}) => (root := r; SOME value)
      | (_,r) => (root := r; NONE)

(* Find an item *)
fun member arg = case peek arg of NONE => false | SOME _ => true

(* Find an item, raising NotFound if not found *)
fun retrieve arg = case peek arg of NONE => raise NotFound | SOME v => v

(* Remove an item.
 * Raise NotFound if not found
 *)
fun delete (OS{cmpKey,root,nobj},key) =
  case splay (cmpf cmpKey key, !root) of
    (_,SplayNil) => raise NotFound
  | (EQUAL,SplayObj{value,left,right}) => 
      OS{cmpKey=cmpKey, root=ref(join(left,right)), nobj=nobj-1}
  | (_,r) => (root := r; raise NotFound)

	(* Return the number of items in the table *)
fun numItems (OS{nobj,...}) = nobj

fun isEmpty (OS{nobj=0,...}) = true
  | isEmpty _ = false

local
  fun member cmpKey (x,tree) = let
        fun mbr SplayNil = false
          | mbr (SplayObj{value,left,right}) =
              case cmpKey (x,value) of
                LESS => mbr left
              | GREATER => mbr right
              | _ => true
      in mbr tree end

    (* true if every item in t is in t' *)
  fun treeIn cmpKey (t,t') = let
        fun isIn SplayNil = true
          | isIn (SplayObj{value,left=SplayNil,right=SplayNil}) =
              member cmpKey (value, t')
          | isIn (SplayObj{value,left,right=SplayNil}) =
              member cmpKey (value, t') andalso isIn left
          | isIn (SplayObj{value,left=SplayNil,right}) =
              member cmpKey (value, t') andalso isIn right
          | isIn (SplayObj{value,left,right}) =
              member cmpKey (value, t') andalso isIn left andalso isIn right
        in
          isIn t
        end
in
fun isSubset (OS{cmpKey,root=rt,nobj=n},OS{root=rt',nobj=n',...}) =
      n<=n' andalso treeIn cmpKey (!rt,!rt');

fun equal (OS{cmpKey,root=rt,nobj=n},OS{root=rt',nobj=n',...}) =
    n=n' andalso treeIn cmpKey (!rt,!rt');
end

fun split cmpKey value s =
      case splay(cmpf cmpKey value, s) of
        (EQUAL,SplayObj{value,left,right}) => (SOME value, left, right)
      | (LESS,SplayObj{value,left,right}) => (NONE, SplayObj{value=value,left=left,right=SplayNil},right)
      | (GREATER,SplayObj{value,left,right}) => (NONE, left, SplayObj{value=value,right=right,left=SplayNil})
      | (_,SplayNil) => (NONE, SplayNil, SplayNil)

fun intersection (s as OS{nobj=0,...},_) = s
  | intersection (_,s as OS{nobj=0,...}) = s
  | intersection (OS{cmpKey,root,...},OS{root=root',...}) =
    let fun inter SplayNil _ = (SplayNil,0)
	  | inter _ SplayNil = (SplayNil,0)
	  | inter s (SplayObj{value,left,right}) =
	    case split cmpKey value s of
		(SOME v, l, r) =>
                    let val (l',lcnt) = inter l left
                        val (r',rcnt) = inter r right
                    in (SplayObj{value=v,left=l',right=r'},lcnt+rcnt+1) end
	      | (_,l,r) =>
                    let val (l',lcnt) = inter l left
                        val (r',rcnt) = inter r right
                    in (join(l',r'),lcnt+rcnt) end
          val (root,cnt) = inter (!root) (!root')
    in OS{cmpKey = cmpKey, root = ref root, nobj = cnt} end

fun count st =
    let fun cnt SplayNil n = n
	  | cnt (SplayObj{left,right,...}) n = cnt left (cnt right (n+1))
    in cnt st 0 end

fun difference (s as OS{nobj=0,...},_) = s
  | difference (s,OS{nobj=0,...}) = s
  | difference (OS{cmpKey, root,...}, OS{root=root',...}) =
    let fun diff SplayNil _ = (SplayNil,0)
	  | diff s SplayNil = (s, count s)
	  | diff s (SplayObj{value,right,left}) =
	    let val (_,l,r)   = split cmpKey value s
		val (l',lcnt) = diff l left
		val (r',rcnt) = diff r right
	    in (join(l',r'),lcnt+rcnt) end
	val (root,cnt) = diff (!root) (!root')
    in OS{cmpKey = cmpKey, root = ref root, nobj = cnt} end

fun union (OS{nobj=0,...},s) = s
  | union (s,OS{nobj=0,...}) = s
  | union (OS{cmpKey, root,...}, OS{root=root',...}) =
      let fun uni SplayNil s = (s,count s)
            | uni s SplayNil = (s, count s)
            | uni s (SplayObj{value,right,left}) =
                let val (_,l,r) = split cmpKey value s
                    val (l',lcnt) = uni l left
                    val (r',rcnt) = uni r right
                in
                  (SplayObj{value=value,right=r',left=l'},lcnt+rcnt+1)
                end
          val (root,cnt) = uni (!root) (!root')
      in
        OS{cmpKey = cmpKey, root = ref root, nobj = cnt}
      end

(* Return a list of the items (and their keys) in the dictionary *)
fun listItems (OS{root,...}) =
    let fun apply SplayNil                     res = res
          | apply (SplayObj{value,left,right}) res =
              apply left (value :: apply right res)
    in apply (!root) [] end

(* Apply a function to the entries of the dictionary *)
fun app af (OS{root,...}) =
      let fun apply SplayNil = ()
            | apply (SplayObj{value,left,right}) = 
                (apply left; af value; apply right)
    in
      apply (!root)
    end

fun revapp af (OS{root,...}) =
    let fun apply SplayNil = ()
	  | apply (SplayObj{value,left,right}) = 
	    (apply right; af value; apply left)
    in apply (!root) end

(* Fold function *)
fun foldr abf b (OS{root,...}) =
    let fun apply SplayNil                     res = res
	  | apply (SplayObj{value,left,right}) res =
	    apply left (abf(value, apply right res))
    in apply (!root) b end

fun foldl abf b (OS{root,...}) =
    let fun apply SplayNil                     res = res
	  | apply (SplayObj{value,left,right}) res =
	    apply right (abf(value, apply left res))
    in apply (!root) b end

fun find p (OS{root,...}) = 
    let fun ex SplayNil = NONE
	  | ex (SplayObj{value=v,left=l,right=r}) =
            if p v then SOME v
            else case ex l of
		NONE => ex r
	      | a => a 
    in ex (!root) end
