(* Example: simple recursive structures and signatures *)

(* a simple recursive structure *)

structure S = 
   rec (X:sig structure Odd : sig val test : int -> bool 
                              end 
          end)  (* forward declaration *)
   struct structure Even = struct fun test 0 = true 
				    | test n = X.Odd.test (n-1) (* forward reference *)
			   end	
	  structure Odd = struct fun test 0 = false
				   | test n = Even.test (n-1)
			  end	       
   end;
(* test *)
val t1 = S.Even.test 100;
val t2 = S.Odd.test 100;

(* a simple recursive signature, specifying mutually recursive datatypes
   that span module boundaries *)

signature REC = 
    rec(X: sig structure Odd: sig type t end end)  (* the forward declaration *)
    sig structure Even: sig datatype t = Zero 
                                       | Succ of X.Odd.t (* forward reference *)
                        end
	structure  Odd: sig datatype t = One  
                                       | Succ of Even.t end
    end;

(* the recursive signature can now be used to implement a 
   recursive structure T with mutually recursive datatypes 
   that span module boundaries 
*)

structure T = 
    rec(X:REC) (* the forward declaration *)
    struct 
	structure Even = struct datatype t = datatype X.Even.t 
				fun succ Zero = X.Odd.One 
				  | succ E = X.Odd.Succ E
			 end
	structure Odd  = struct datatype t = datatype X.Odd.t
				fun succ O = Even.Succ O
			 end
    end

(* test *)
val t3 = T.Even.Succ (T.Odd.Succ T.Even.Zero);






