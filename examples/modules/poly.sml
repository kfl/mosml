(* Example: implementing polynomials using higher-order functors *)

(* the signature of structures implementing natural numbers *) 

signature NAT = 
    sig type nat
	val z:nat                         (* zero *)
	val s:nat->nat                    (* successor *)
	val i:'a -> ('a->'a) -> nat -> 'a (* iteration *)
    end;

(* the signature of structures evaluating polynomials,
   presented as a list of coefficients 
   eg 
     eval x [c0,...,cn] = (c0 * x^0) + .... + (cn * x^n)
*)

signature POLY = 
  sig structure Nat:NAT;
       val eval:Nat.nat->(Nat.nat list)->Nat.nat;
  end;

(* The implementation of  "MkPoly".

   "MkPoly" is a higher-order functor taking 3 arguments.
     "N" - is a structure implementing natural numbers
     "A" - is functor mapping "N" to an implementation of addition.
     "M" - is a higher-order functor mapping "N" and "A" to an
           implementation of multiplication
 *)

functor  MkPoly =
  functor(N:NAT) => 
  functor(A:functor(X:NAT where type  nat = N.nat) ->
                  sig val add:X.nat -> X.nat -> X.nat end) =>
  functor(M:functor(X:NAT where type nat = N.nat)->
	    functor(A:functor(Y:NAT where type nat = N.nat) ->
		      sig  val add: Y.nat -> Y.nat-> Y.nat  end)->
	    sig val mult: X.nat-> X.nat -> X.nat end) =>
    struct
       structure Nat = N;
       structure Add = A  N;
       structure Mult = M  N  A;
       fun  eval x [] = Nat.z
	 |  eval x (h::l) = Add.add h (Mult.mult x (eval x l))
    end; 

(* The implementations of  N, A, M. *)

structure N =
  struct type nat = int;
         val z = 0;
	 fun s i = i + 1;
	 fun i b f 0 = b 
	   | i b f n = f (i b f (n-1)) 
  end;

(* we implement addition by iterating successor *)
functor A = functor(X:NAT where type nat = N.nat)=>
               struct fun add n m = X.i n X.s m  
	       end;

(* we implement mulitiplication by iterating addition *)
functor M = functor(X:NAT where type nat = N.nat)=>
            functor(A:functor(Y:NAT where type nat = X.nat)->
                                sig val add: Y.nat -> Y.nat -> Y.nat end)=>
                    struct structure Add = A X;
			   fun mult n m = X.i X.z (Add.add n) m
                    end;


(* applying "MkPoly" ...  *)
structure Poly = MkPoly N A M;

(* evaluating  0(x^0) + 1(x^2)+ 0(x^3) + 1(x^4) at x = 2 yields 10 *)
val test = Poly.eval 2 [0,1,0,1];

(* the functor "A'" is a more general version of "A". *)                                                              
functor A' = functor(X:sig type nat;
			   val s: nat->nat;
			   val i: nat->(nat->nat)->nat->nat
		       end)=>
                      struct  val add =  fn n =>  fn m => X.i  n  X.s  m;
                              fun sum b [] = b
                                | sum b (h::l) = add h (sum b l)
                      end;
(* the application "MkPoly N A'" type checks even though the type of "A'" 
   is more general than required: *)

structure Poly' = MkPoly N A' M;

(*
 efficient implementations of  "N", "A" and "M" 
 that directly exploit integer addition and multiplication
*)

structure N = struct type  nat = int;
		     val z =0;
		     val s = fn i => i + 1;
		     fun i b f 0 = b
		       | i b f j = f (i b f (j-1))
	      end;

functor A = functor(X:sig  end)=>
               struct val add = fn i => fn j => i + j
               end;

functor M = functor(X:sig  end)=>
	    functor(A:functor(Y:NAT where type nat = int)->
		      sig  end)=>
	    struct val mult = fn i =>  fn j => i *  j
	    end;

(* a fast but not abstract implementation of "POLY" based on 
   the efficient versions of "N A M". 
*)

structure IntPoly = MkPoly N A M;
val test = IntPoly.eval 2 [0,1,0,1];

(* efficient and *abstract* implementations of  "N", "A" and "M" *)                  
structure FastNat =
  struct
       structure N = N;
       functor A = A;
       functor M = M
  end  :>
  sig
         structure N: NAT;
         functor A:functor(X:sig end)->
                             sig  val add:  N.nat->N.nat->N.nat  end;
         functor M:functor(X:sig end)->
		   functor(A:functor(Y:NAT where type nat = N.nat) ->
			     sig end)->
                   sig val mult: N.nat->N.nat->N.nat end
  end;

(* A fast and abstract implementation of POLY based
   on the efficient and abstract implementation of FastNat. 
*)

structure FastPoly = MkPoly FastNat.N FastNat.A FastNat.M;











