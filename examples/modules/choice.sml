(* Example: conditional module declarations using first-class modules *)

(* conditional structures *)

signature NAT = 
   sig type nat 
       val Z:nat 
       val S:nat -> nat 
       val plus: nat -> nat -> nat 
   end

structure SafeNat =  (* unlimited range but slow *)
  struct datatype nat = Z | S of nat 
         fun plus Z m = m 
           | plus (S n) m = S (plus n m)
  end

structure FastNat =  (* limited range but fast *)
  struct type nat = int 
         val Z = 0 
         fun S n = n + 1 
         fun plus n m = n + m 
  end

type natpack = [ NAT ]                                      (* package type *)

val safeNat = [ structure SafeNat as NAT ];                 (* packing *)

val fastNat = [ structure FastNat as NAT ];    

(* choosing a structure at run-time  *)

structure Nat as NAT = 	                                    (* unpacking *)
      if (913 mod 7 = 5) then safeNat else fastNat

val natlist = [safeNat,fastNat] : natpack list;             (* etc. *)


(* similarly, conditional functors ... *)

signature NATFUN = functor(X:sig end)->
   sig type nat 
       val Z:nat 
       val S:nat -> nat 
       val plus: nat -> nat -> nat 
   end

functor SafeNatFun() =  (* unlimited range but slow *)
  struct datatype nat = Z | S of nat 
         fun plus Z m = m 
           | plus (S n) m = S (plus n m)
  end

functor FastNatFun() =  (* limited range but fast *)
  struct type nat = int 
         val Z = 0 
         fun S n = n + 1 
         fun plus n m = n + m 
  end

val safeNatFun = [ functor SafeNatFun as NATFUN ];             (* packing *)

val fastNatFun = [ functor FastNatFun as NATFUN ];    

(* choosing a structure at run-time  *)

functor NatFun as NATFUN = 	                               (* unpacking *)
      if (913 mod 7 = 5) then safeNatFun else fastNatFun

type natfunpack = [ NATFUN ]                                   (* package type *)

val natfunlist = [safeNatFun,fastNatFun] : natfunpack list;    (* etc. *)