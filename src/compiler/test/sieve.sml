(*
 Author: Claudio Vittorio Russo, Department of Computer Science, University of Edinburgh, Scotland
 Copyright: 1998. All rights reserved.
*)

fun divides divisor n = (n mod divisor) = 0;
fun succ n = n + 1;

(* Figure  7.3 The signature Stream of integer streams *)
    
signature  Stream  =
    sig type state;
        val start: state;
        val next:  state  ->  state;
        val value: state  ->  int
    end;

(* Figure  7.4 The function sift implementing sift .*)
                                                                        

val sift = fn s => 
    let structure S as Stream = s  
    in
	[structure 
	     struct
		 val divisor  =  S.value  S.start;
		 fun filter  state =
		     if  divides  divisor  (S.value  state)
			 then  (filter  (S.next  state))
		     else  state;
		 type state =  S.state;
		 val start  =  filter  S.start;
		 val next  =  fn state => filter  (S.next  state);
		 val value  =  S.value
	     end
	     as  Stream]
    end;

(* Figure  7.5 The implementation Sieve of Sieve. *)

structure  Sieve  =
    struct
	type  state  =  [Stream];
	val  start  =  [structure 
			    struct
				type  state  =  int;
				val  start  =  2;
				val  next  =  succ;
				val  value  =  fn state => state
			    end
			    as  Stream];
	val  next  =  sift;
	val  value  =  fn state => let structure  S as Stream = state  
				   in S.value  S.start
				   end
    end :> Stream;

(* Figure  7.6  The  function  nthprime  implementing  the  mathematical 
   function nthprime.
*)

fun  nthstate 0 = Sieve.start
  |  nthstate n = Sieve.next  (nthstate  (n  -1));

val  nthprime  = fn n => Sieve.value  (nthstate  n);

val primes = 
    let fun primes n state =
	if n <= 0 
	    then []
        else
	    (Sieve.value state) :: (primes (n-1) (Sieve.next state))
    in fn n => primes n Sieve.start
    end;

(* Figure  7.7 A stratified implementation StratSieve of Sieve. *)
                                                              
structure  StratSieve  =
    struct
         structure  Start  = 
	     struct 
		 type state = int;
		 val start = 2;
		 val next = succ;
		 val value = fn state => state
	     end;
         functor  Next(S:Stream) =
             struct
		 type state =  S.state;
		 val divisor =  S.value  S.start;
		 fun filter state =
		     if  divides  divisor  (S.value  state)
			 then  filter  (S.next  state)
		     else  state;
		 val start =  filter  S.start;
		 val next =  fn state => filter  (S.next  state);
		 val  value  =  S.value
	     end;
         functor  Value(S:Stream) =
	     struct  val  value  =  S.value  (S.start)  end
    end;

val hundred_primes = primes 100;

val test = if primes 5 = [2,3,5,7,11] then "OK" else "FAIL";

















