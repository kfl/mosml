(* Example: a simple example illustrating the difference between
   generative and applicative functors and the motivation for supporting
   both.
   NB: 

   o A functor is generative when its formal argument is declared within
   parenthesis, eg;
   functor funid (modid:sigexp) = modexp;

   o A functor is applicative when its formal argument
   is declared *without* parentheses, eg:
   functor funid modid:sigexp = modexp; 
*)

(* Collection specifies a generic abstract type of collections *)

signature Collection =
    sig type elem
	type collection
        val empty : collection
	val add : elem -> collection -> collection
    end;

(* We define two functor's that implement "Collection", 
   o The "Set" functor implements "Collections" using sets 
     represented as ordered lists. 
     Since the set representation depends on an element ordering: 
     it should vary with each ordering.
     For this reason, the "Set" functor is implemented as a 
     "generative" functor.
   o The "List" functor implements "Collections" using lists
     The list representation does not depend on any element
     ordering, and need not vary.
     For this reason, the "List" functor is implemented as an 
     "applicative" functor.
*) 
   
(* "Set" functor should be generative because the return type 
   "collection" depends on the ordering "X.ord", which may vary with each
   application of "Set" *)

functor Set ( X:sig type elem val ord : elem * elem -> bool end ) 
         (* ^ parentheses, "Set" is generative                  ^ *) 
  = struct 
	type elem = X.elem
	type collection = X.elem list
	val empty = []
	fun add e [] = [e]
	  | add e (e'::c) = 
  	    if X.ord(e,e')
		then e :: e' :: c
	    else if X.ord(e',e)
		     then 
			 e :: c
		 else e :: add e c
    end :> Collection where type elem = X.elem;
  
structure IncSet = Set(struct type elem = int val ord = op <= end);
structure DecSet = Set(struct type elem = int val ord = op >= end);

(* because the functor "Set" is generative, "IncSet.collection" and
   "DecSet.collection" are distinct types, eventhough "Set"'s argument
   type "elem" = "int" was the same in both applications. *)

(* val fail =  DecSet.add 2 (IncSet.add 1 IncSet.empty); *)

(* The functor "List" is applicative since the type "collection"
   depends only on the type "X.elem" (but not on any ordering on
   "X.elem") *)

functor  List X:sig type elem end  
    (* ^ no parentheses, "List" is applicative  ^ *) 
  = struct type elem = X.elem
	   type collection = X.elem list
	   val empty = []
	   fun add e c = e::c
    end :> Collection where type elem = X.elem;

structure IncList = List(struct type elem = int val ord = op <= end);
structure DecList = List(struct type elem = int val ord = op >= end);
structure BoolList = List(struct type elem = bool end);

(* because "List" is applicative "IncList.collection" and
   "DecList.collection" are compatible types, because "List"'s
   argument type "elem" = "int" is the same *)

val ok =  DecList.add 2 (IncList.add 1 IncList.empty);

(* but "BoolList.collection" is a different type, because the "List"'s
   argument type "elem" = "bool" is distinct from "int" *)

(* val fail = BoolList.add true (IncList.add 1 IncList.empty); *)










