(* This file is just like array.sml except that we permute the order
   of structure components to (weakly) test the correctness of
   coercion code *)

(* Author: Claudio Vittorio Russo, Department of Computer Science, University of Edinburgh, Scotland
 Copyright: 1998. All rights reserved.
*)
(* The  signature  Array  specifies  a  structure  implementing  fixed-size arrays.
*)

signature  Array  =
  sig  type 'a array;
       val init:'a  ->  ('a array);
       val update: ('a array)  ->  int  ->  'a  ->  ('a array)
       val sub: ('a array)  ->  int  ->  'a;
  end;

(* The structure ArrayZero implementing arrays of size 2^0. *)

structure  ArrayZero =
    struct
         val update = fn a => fn i => fn x => x
         val init = fn x => x;
         val sub = fn a => fn i => a;
 	 type 'a array = 'a;
    end;


(* The  functor  ArraySucc  mapping  an  implementation  
   of  arrays  of size 2^n  to an implementation of arrays of size 2^(n+1)
*)
functor ArraySucc (A:Array) =
      struct
             val update  =  fn (even,odd) => fn i => fn x => 
		 if  (i mod  2) = 0
		 then (A.update even (i div  2)  x, odd)
		 else (even, A.update  odd (i div 2)  x)
             type 'a array  =  ('a A.array)  *  ('a A.array);            
             val sub  =  fn (even,odd) => fn i =>
                           if  (i mod  2) = 0
			       then A.sub  even  (i div  2)
                               else A.sub  odd  (i div 2);
             val init  =  fn x =>  (A.init  x, A.init  x)
      end;

(* The  function  mkArray:   applying  mkArray  to  an  integer n
   (n >=0)   returns an abstract implementation of arrays of size 2^n 
*)

fun  mkArray 0 = [structure  ArrayZero  as  Array]
  |  mkArray n = let structure A as Array = mkArray (n-1) 
		 in
                   [structure  ArraySucc(A)  as  Array]
                 end
;

fun two 0 = 1
|   two n = 2 * two (n-1);

val n = 10;
val max = two n;

fun iter f b 0 = b | iter f b n = f (iter f b (n-1)) n;

structure A as Array = mkArray n;

val a = A.init 0;

(* test every entry is initialised to zero *)

val test1 = if iter (fn l => fn n => l andalso (A.sub a n) = 0) true max 
		then "OK" 
	    else "FAIL";

val a = iter (fn a => fn n => A.update a n n) a max;

val l = iter (fn l => fn n => (A.sub a n)::l) [] max

(* test every entry has been updated *)
val test2 = if iter (fn l => fn n => l andalso (A.sub a n) = n) true max 
		then "OK" 
	    else "FAIL";
