(* Hashset -- sets implemented using a hash function and equality predicate.
 * Note: in contrast to Rbset this representation is imperative.  
 * 
 * 80 % complete * 2001-10-28

   A functional (persistent) hashset or hashmap does not seem to make
   much sense.  The point of a hashset or hashmap is to provide
   near-constant-time access, so should use an array or vector.  But
   copying the array on every update or insertion is too unpleasant,
   as its size will be proportional to the number of elements in the
   set or entries in the map.  

   We miss hashcode functions for string, int, real, ... or must use
   the general pseudo-polymorphic function hash : 'a -> word.

   Sigh.  The SML/NJ ord-set-sig and hash-table-sig use the name find
   with entirely different meanings.

 * Considerably modified for Moscow ML from SML/NJ Library version 0.2
 *
 * COPYRIGHT (c) 1992 by AT&T Bell Laboratories.
 * See file mosml/copyrght/copyrght.att for details.
 *
 * Original author: John Reppy, AT&T Bell Laboratories, Murray Hill, NJ 07974
 *)

exception NotFound

datatype 'key bucket_t
  = NIL
  | B of word * 'key * 'key bucket_t   (* hashcode, item, rest *)

datatype 'key set = 
    HT of { hashVal : 'key -> word,
	    sameKey : 'key * 'key -> bool,
	    table   : 'key bucket_t Array.array ref,
	    n_items : int ref}

local 
    prim_val andb_      : word -> int -> int = 2 "and";
    prim_val lshift_    : int -> int -> int = 2 "shift_left";
in 
    fun index (i, sz) = andb_ i (sz-1)

    (* find smallest power of 2 (>= 32) that is >= n *)
    fun roundUp n = 
	let fun f i = if (i >= n) then i else f (lshift_ i 1)
	in f 32 end
end;

(* Conditionally grow the table *)

fun growTable (HT{table, n_items, ...}) = 
    let val arr = !table
	val sz = Array.length arr
    in
	if (!n_items >= sz) then 
	    let val newSz = sz+sz
		val newArr = Array.array (newSz, NIL)
		fun copy NIL = ()
		  | copy (B(h, key, rest)) = 
		    let val indx = index (h, newSz)
		    in
			Array.update (newArr, indx,
				      B(h, key, Array.sub(newArr, indx)));
			copy rest
		    end
	    in
		Array.app copy arr;
		table := newArr
	    end
	else ()
    end (* growTable *);

(* Create a new empty table  *)

fun empty (hashVal, sameKey) = 
    HT{
       hashVal=hashVal,
       sameKey=sameKey,
       table = ref (Array.array(roundUp 13, NIL)),
       n_items = ref 0
       };

(* Add an item.  If the key already has an item associated with it, *
   then the old item is discarded. *)

fun add (tbl as HT{hashVal, sameKey, table, n_items}, key) =
    let val arr = !table
	val sz = Array.length arr
	val hash = hashVal key
	val indx = index (hash, sz)
	fun look NIL = 
	    (Array.update(arr, indx, B(hash, key, Array.sub(arr, indx)));
	     n_items := !n_items + 1;
	     growTable tbl;
	     NIL)
	  | look (B(h, k, r)) = 
	    if ((hash = h) andalso sameKey(key, k)) then 
		B(hash, key, r)
	    else (case (look r) of 
		      NIL => NIL
		    | rest => B(h, k, rest)
                  (* end case *))
    in
	case (look (Array.sub (arr, indx))) of 
	    NIL => ()
	  | b   => Array.update(arr, indx, b)
    end;

(* Add a list of elements to the set  *)

fun addList (set, xs) = 
    List.app (fn x => add(set, x)) xs

(* Create a new singleton set *)

fun singleton (hashVal, sameKey) item = 
    let val set = HT{hashVal=hashVal,
		     sameKey=sameKey,
		     table = ref (Array.array(roundUp 13, NIL)),
		     n_items = ref 0
		     }
    in add (set, item); set end

(* Being empty, subset of, or equal *)

fun isEmpty (HT{n_items=ref n, ...}) = (n = 0)

fun all p (HT{table, ...}) = 
    let fun allF NIL               = true
	  | allF (B(_, key, rest)) = p key andalso allF rest
    in
	Array.all allF (!table)
    end

fun exists p (HT{table, ...}) = 
    let fun existsF NIL               = false
	  | existsF (B(_, key, rest)) = p key orelse existsF rest
    in
	Array.exists existsF (!table)
    end

fun member (set as HT{sameKey, ...}, x) = 
    exists (fn y => sameKey(x, y)) set

fun isSubset (set1 as HT{sameKey, n_items=ref n1, ...}, 
	      set2 as HT{n_items=ref n2, ...}) = 
    n1<=n2 andalso 
    all (fn x1 => exists (fn x2 => sameKey(x1, x2)) set2) set1

fun equal (set1 as HT{n_items=ref n1, ...}, set2 as HT{n_items=ref n2, ...}) = 
    n1=n2 andalso isSubset (set1, set2) andalso isSubset(set2, set1)    

(* Retrieve an item, or raise NotFound *)

fun retrieve (HT{hashVal, sameKey, table, ...}, key) = 
    let val arr = !table
	val sz = Array.length arr
	val hash = hashVal key
	val indx = index (hash, sz)
	fun look NIL = raise NotFound
	  | look (B(h, k, r)) = 
	    if ((hash = h) andalso sameKey(key, k)) then k
	    else look r
    in
	look (Array.sub (arr, indx))
    end;

(* Find an item, return NONE if the item doesn't exist *)

fun peek (HT{hashVal, sameKey, table=ref arr, ...}, key) = 
    let val sz = Array.length arr
	val hash = hashVal key
	val indx = index (hash, sz)
	fun look NIL = NONE
	  | look (B(h, k, r)) = 
	    if (hash = h) andalso sameKey(key, k) then SOME k
	    else look r
    in
	look (Array.sub (arr, indx))
    end;
    
(* Delete an item *)

fun delete (HT{hashVal, sameKey, table, n_items}, key) = 
    let val arr = !table
	val sz = Array.length arr
	val hash = hashVal key
	val indx = index (hash, sz)
	fun look NIL          = raise NotFound
	  | look (B(h, k, r)) = 
	    if (hash = h) andalso sameKey(key, k) then
		(k, r)
	    else 
		let val (item, r') = look r 
		in (item, B(h, k, r')) end
	val (item, bucket) = look (Array.sub (arr, indx))
    in
	Array.update (arr, indx, bucket);
	n_items := !n_items - 1
    end (* remove *);

(* Return the number of items in the table *)

fun numItems (HT{n_items, ...}) = !n_items

(* Return a list of the members of the set *)

fun listItems (HT{table = ref arr, n_items, ...}) = 
    let fun loop NIL             res = res
	  | loop (B(_, k, rest)) res = loop rest (k :: res)
	fun coll (bucket, res) = loop bucket res
    in
	Array.foldr coll [] arr
    end;

(* Apply a function to the members of the set *)

fun app f (HT{table, ...}) = 
    let fun appF NIL               = ()
	  | appF (B(_, key, rest)) = (f key; appF rest)
    in
	Array.app appF (!table)
    end

(* Fold over the members of the set *)

fun fold f e (HT{table, ...}) = 
    let fun loop NIL             res = res
	  | loop (B(_, k, rest)) res = loop rest (f (k, res))
	fun foldF (bucket, res) = loop bucket res
    in
	Array.foldr foldF e (!table)
    end

(* Find a member that satisfies the predicate *)

fun find p (HT{table=ref arr, ...}) = 
    let fun loopb NIL             = NONE
	  | loopb (B(_, k, rest)) = if p k then SOME k else loopb rest 
	fun loopf i = 
	    if i<0 then 
		NONE
	    else 
		case loopb (Array.sub(arr, i)) of
		    NONE => loopf (i-1)
		  | res  => res
    in
	loopf (Array.length arr - 1)
    end
    
(* Create a copy of the hashset *)

fun copy (HT{hashVal, sameKey, table=ref arr, n_items}) = 
    let val newArr = Array.array (Array.length arr, NIL)
    in
	Array.copy { src=arr, dst=newArr, di=0 };
	HT{hashVal=hashVal, 
	   sameKey=sameKey,
	   table = ref newArr, 
	   n_items = ref(!n_items)}
    end;

(* The hash code of a hashset -- use the stored hash codes *)

fun hash (HT{table, ...}) = 
    let fun loop NIL             res = res
	  | loop (B(h, _, rest)) res = loop rest (h + res)
	fun addHash (bucket, res) = loop bucket res
    in
	Array.foldr addHash 0w0 (!table)
    end

(* The built-in pseudo-polymorphic hash function *)

prim_val hash_param : int -> int -> 'a -> word = 3 "hash_univ_param";

fun polyHash x = hash_param 50 500 x;
