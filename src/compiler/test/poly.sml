(* Author: Claudio Vittorio Russo, Department of Computer Science, University of Edinburgh, Scotland
 Copyright: 1998. All rights reserved.
*)

(* Figure  5.1 The specification of a module evaluating polynomials. *)

signature POLY = 
  sig structure Nat:sig type nat;
                        val z:nat;
			val s:nat->nat;
			val i:'a -> ('a->'a) -> nat -> 'a;
		    end;
       val  eval:Nat.nat->(Nat.nat list)->Nat.nat;
  end;

(* Figure  5.5 The implementation of  MkPoly. *)

functor  MkPoly  =
functor(N:sig  type  nat;
                     val  z:nat;
                     val  s:nat->nat;
                     val  i:'a->('a->'a)->nat->'a
          end)=>
functor(A:functor(X:sig  type  nat  =  N.nat;
                              val  z:nat;
                              val  s:nat->nat;
                              val  i:'a->('a->'a)->nat->'a
                   end)->
                  sig  val  add:X.nat->X.nat->X.nat  end) =>
functor(M:functor(X:sig  type  nat  =  N.nat;
                        val  z:nat;
                        val  s:nat->nat;
                        val  i:'a->('a->'a)->nat->'a
                   end)->
                  functor(A:functor(Y:sig  type  nat  =  X.nat;
                                         val  z:nat;
                                         val  s:nat->nat;
                                         val  i:'a->('a->'a)->nat->'a
                                    end)->
                                   sig  val  add:Y.nat->Y.nat->Y.nat  end)->
                  sig  val  mult:X.nat->X.nat->X.nat    end) =>
    struct
       structure  Nat  =  N;
       structure  Add  =  A  N;
       structure  Mult  =  M  N  A;
       fun  eval x [] = Nat.z
	 |  eval x (h::l) = Add.add  h  (Mult.mult  x  (eval x  l))
    end; 

(* Figure  5.6 The implementations of  N, A, M. *)

structure  Nat  =
  struct  type nat  =  int;
               val  z  =  0;
               val  s  =   fn x =>  x + 1;
               fun  i b f 0 = b 
		 |  i b f n = f (i b f (n-1)) 
  end;

structure  N  =  Nat;

functor  A  =  functor(X:sig  type  nat  =  N.nat;
                                             val  z:nat;
                                             val  s:nat->nat;
                                             val  i:'a->('a->'a)->nat->'a
                                      end)=>
                    struct  val  add  =   fn n =>  fn m =>   X.i  n  X.s  m  end;

functor  M  =  functor(X:sig  type  nat  =  N.nat;
                                             val  z:nat;
                                             val  s:nat->nat;
                                             val  i:'a->('a->'a)->nat->'a
                                      end)=>
                    functor(A:functor(Y:sig  type  nat  =  X.nat;
                                                              val  z:nat;
                                                              val  s:nat->nat;
                                                              val  i:'a->('a->'a)->nat->'a
                                                      end)->
                                      sig  val  add:Y.nat->Y.nat->Y.nat  end)=>
                    struct  structure  Add  =  A  X;
                                 val  mult  =   fn n =>  fn m =>   X.i  X.z  (Add.add  n)  m
                    end;

(*  applying MkPoly ...  *)
structure  Poly =  MkPoly N A M;

(* evaluating  0(x^0) + 1(x^2)+ 0(x^3) + 1(x^4) at x = 2 yields 10 *)
val test = Poly.eval 2 [0,1,0,1];

(* Figure  5.7 The functor A' is a more general version of  A. *)                                                              
functor  A' =  functor(X:sig  type  nat;
                              val  s:nat->nat;
                              val  i:  nat->(nat->nat)->nat->nat
                         end)=>
                      struct  val  add  =   fn n =>  fn m => X.i  n  X.s  m;
                              fun  sum b [] = b
                                |  sum b (h::l) = add h (sum b l)
                      end;
(* The application MkPoly N A' type checks even though the type of A' 
   is more general than required: *)

structure Poly' = MkPoly N A' M;

(*
 Figure  5.8 Efficient implementations of  N, A and M 
 that directly exploits integer addition and multiplication instead of 
 iteration.
*)

structure  N  =  struct  type  nat  =  int;
                            val z =0;
                            val s = fn i => i + 1;
                            fun i b f 0 = b
			      | i b f j = f (i b f (j-1))
                    end;

functor  A  =  functor(X:sig  end)=>
                    struct val add = fn i => fn j => i + j
                    end;

functor  M  =  functor(X:sig  end)=>
                    functor(A:functor(Y:sig  type  nat  =  int;
                                            val  z:  nat;
                                            val  s:  nat->nat;
                                            val  i:'a->('a->'a)->nat->'a
                                       end)->
                                      sig  end)=>
                    struct val mult = fn i =>  fn j => i *  j
                    end;

(* a fast but not abstract implementation of
   POLY based on the efficient versions of N A M. 
*)

structure IntPoly = MkPoly N A M;
val test = IntPoly.eval 2 [0,1,0,1];

(* Figure  5.9 An efficient and *abstract* implementation of  N, A and M *)                                                     

structure  FastNat  =
  struct
       structure  N  =  N;
       functor A  =  A;
       functor M  =  M
  end  :>
  sig
         structure  N:  sig  type  nat;
			     val  z:nat;
			     val  s:nat->nat;
			     val  i:'a->('a->'a)->nat->'a
			end;
         functor  A:functor(X:sig  end)->
                             sig  val  add:  N.nat->N.nat->N.nat  end;
         functor  M:functor(X:sig  end)->
                           functor(A:functor(Y:sig  type  nat  =  N.nat;
                                                  val  z:nat;
                                                  val  s:nat->nat;
                                                  val  i:'a->('a->'a)->nat->'a
                                             end)->
                                            sig  end)->
                           sig  val  mult:  N.nat->N.nat->N.nat  end
  end;

(* A fast and abstract implementation of POLY based
   on the efficient and abstract implementation of FastNat. 
*)

structure FastPoly = MkPoly FastNat.N FastNat.A FastNat.M;











