(* a simple example illustrating the difference between
   generative and applicative functors  and the motivation for supporting
   both.
   NB: 
   o A functor is generative when its formal argument is declared within
   parenthesis, eg;
   functor funid (modid:sigexp) = modexp;

   o A functor is applicative when its formal argument
   is declared *without* parentheses, eg:
   functor funid modid:sigexp = modexp; 
*)

signature Collection =
    sig type e
	type t
        val empty : t
	val add : e -> t -> t
    end;

(* we make the Set functor generative since the type t should
   depend on the ordering X.ord, which may vary with each application
   of Set *)

functor Set (X:sig type e 
                   val ord : e * e -> bool
	       end) =
struct type e = X.e
       type t = X.e list
       val empty = []
       fun add e [] = [e]
	 | add e (e'::t) = 
	   if X.ord(e,e')
	       then e :: e' :: t
	   else if X.ord(e',e)
		    then 
			e :: t
		else e :: add e t
end:> Collection where type e = X.e;

structure IncSet = Set(struct type e = int val ord = op <= end);
structure DecSet = Set(struct type e = int val ord = op >= end);
val fail =  DecSet.add 2 (IncSet.add 1 IncSet.empty);


(* On the other hand, we can let List functor be applicative since the 
   type t depends only on the type X.e (but not on any ordering on X.e) *)
functor List X:sig type e end =
struct type e = X.e
       type t = X.e list
       val empty = []
       fun add e t = e::t
end:> Collection where type e = X.e;

structure IncList = List(struct type e = int val ord = op <= end);
structure DecList = List(struct type e = int val ord = op >= end);
val ok =  DecList.add 2 (IncList.add 1 IncList.empty);
structure BoolList = List(struct type e = bool end);
val fail = BoolList.add true (IncList.add 1 IncList.empty);










