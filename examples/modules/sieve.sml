(* Example: implementing the Sieve of Eratosthenes 
            using first-class structures
*)

fun divides divisor n = (n mod divisor) = 0;

(* the signature "Stream" of structures representing integer streams:
   a stream is represented as an infinite state process
   that emits a sequence of integer values
*)
    
signature  Stream  =
    sig type state;                   (* states are opaque *)
        val start: state;             (* the initial state *)
        val next:  state  ->  state;  (* the state transition function *)
        val value: state  ->  int     (* the value of a state *)
    end;

(* the Core function "sift s" takes a packaged stream s
   and returns the packaged stream obtained by removing 
   all value from s that are divisible by the initial value of s
*)
                                                                        

fun sift s = 
    let structure S as Stream = s  (* open the package s as the structure S*)
    in
	[structure                 (* create a new package from S *)
	     struct
		 val divisor  =  S.value  S.start;
		 fun filter  state =
		     if  divides  divisor  (S.value  state)
			 then  (filter  (S.next  state))
		     else  state;
		 type state =  S.state;
		 val start  =  filter  S.start;
		 fun next state = filter  (S.next  state);
		 val value  =  S.value
	     end
	     as  Stream]
    end;

(* the implementation of the Sieve *)

structure  Sieve  =
    struct
	type  state  =  [Stream];      (* the type of package Streams *)
	val  start  =  [structure      (* the stream 2,3,4,5 .... *)
			    struct
				type  state  =  int;
				val  start  =  2;
				fun  next n = n + 1;
				fun  value state = state
			    end
			    as  Stream];
	val  next  =  sift;
	fun  value state = let structure  S as Stream = state  
			   in S.value  S.start
			   end
    end :> Stream;

(* the function "nthstate n" returns the nth state of the sieve *)

fun  nthstate 0 = Sieve.start
  |  nthstate n = Sieve.next  (nthstate  (n  -1));

(* the  function "nthprime n" returns the nth prime *)

val  nthprime  = fn n => Sieve.value  (nthstate  n);

val primes = 
    let fun primes n state =
	if n <= 0 
	    then []
        else
	    (Sieve.value state) :: (primes (n-1) (Sieve.next state))
    in fn n => primes n Sieve.start
    end;

val hundred_primes = primes 100;

val test = if primes 5 = [2,3,5,7,11] then "OK" else "FAIL";

















