(**** ML Programs from Chapter 5 of

  ML for the Working Programmer, 2nd edition
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1996)

Copyright (C) 1996 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.
****)


(*** Functions as values ***)

fun insort lessequal = 
    let fun ins (x, []) = [x]
          | ins (x, y::ys) = 
              if lessequal(x,y) then x::y::ys 
              else y :: ins (x,ys)
        fun sort [] = []
          | sort (x::xs) = ins (x, sort xs)
    in  sort  end;

(*Sections*)
fun secl x f y = f(x,y);
fun secr f y x = f(x,y);

fun summation f m =
    let fun sum (i,z) : real =
            if  i=m  then  z  else  sum (i+1, z + (f i))
    in  sum(0, 0.0)  end;

fun takewhile pred [] = []
  | takewhile pred (x::xs) = 
        if  pred x  then  x :: takewhile pred xs  
        else  [];

fun dropwhile pred [] = []
  | dropwhile pred (x::xs) = 
        if  pred x  then  dropwhile pred xs  
        else  x::xs;

infix mem;
fun x mem xs = List.exists (secr op= x) xs;

fun pair x y = (x,y);
fun cartprod (xs, ys) = 
    foldr (fn (x, pairs) => 
                 foldr (fn (y,l) => (x,y)::l) pairs ys)
              [] xs;

fun repeat f n x = 
    if n>0  then  repeat f (n-1) (f x)
            else  x;


(**** Sequences, or Infinite Lists ***)

datatype 'a seq = Nil
                | Cons of 'a * (unit -> 'a seq);

(*Type seq is free in this signature!*)
signature SEQUENCE = 
  sig
  exception Empty
  val cons : 'a * 'a seq -> 'a seq
  val null : 'a seq -> bool
  val hd : 'a seq -> 'a
  val tl : 'a seq -> 'a seq
  val fromList  : 'a list -> 'a seq
  val toList  : 'a seq -> 'a list
  val take : 'a seq * int -> 'a list
  val drop : 'a seq * int -> 'a seq
  val @  : 'a seq * 'a seq -> 'a seq
  val interleave : 'a seq * 'a seq -> 'a seq
  val map : ('a -> 'b) -> 'a seq -> 'b seq
  val filter : ('a -> bool) -> 'a seq -> 'a seq
  val iterates : ('a -> 'a) -> 'a -> 'a seq
  val from : int -> int seq
  end;


structure Seq : SEQUENCE =
  struct
  exception Empty;

  fun hd (Cons(x,xf)) = x
    | hd Nil = raise Empty;

  fun tl (Cons(x,xf)) = xf()
    | tl Nil = raise Empty;

  fun cons(x,xq) = Cons(x, fn()=>xq);

  fun null (Cons _) = false
    | null Nil      = true;

  fun fromList l = foldr cons Nil l;

  fun toList Nil = []
    | toList (Cons(x,xf)) = x :: toList (xf());

  fun take (xq, 0) = []
    | take (Nil, n) = raise Subscript
    | take (Cons(x,xf), n) = x :: take (xf(), n-1);

  fun drop (xq, 0) = xq
    | drop (Nil, n) = raise Subscript
    | drop (Cons(x,xf), n) = drop (xf(), n-1);

  fun Nil @ yq = yq
    | (Cons(x,xf)) @ yq = Cons(x, fn()=> (xf()) @ yq);

  fun interleave (Nil,    yq) = yq
    | interleave (Cons(x,xf), yq) = 
	  Cons(x, fn()=> interleave(yq, xf()));

  (** functionals for sequences **)
  fun map f Nil  = Nil
    | map f (Cons(x,xf)) = Cons(f x, fn()=> map f (xf()));

  fun filter pred Nil = Nil
    | filter pred (Cons(x,xf)) =
	  if pred x then Cons(x, fn()=> filter pred (xf()))
		    else filter pred (xf());

  fun iterates f x = Cons(x, fn()=> iterates f (f x));

  fun from k = Cons(k, fn()=> from(k+1));

  end;



(*** Simple applications ***)

(*Return ALL ways of making change (not just the first way)  *)
fun seqChange (coins, coinvals, 0, coinsf)       = Cons(coins,coinsf)
  | seqChange (coins, [],  amount, coinsf)       = coinsf()
  | seqChange (coins, c::coinvals, amount, coinsf) =
      if amount<0 then coinsf()
      else seqChange(c::coins, c::coinvals, amount-c, 
		       fn() => seqChange(coins, coinvals, amount, coinsf));


(*Sequence of random numbers*)
local val a = 16807.0  and  m = 2147483647.0 
      fun nextRand seed =
	    let val t = a*seed
	    in  t - m * real (Real.floor(t/m))  end
in
  fun randseq s = Seq.map (secr op/ m) 
                          (Seq.iterates nextRand (real s))
end;


(** prime numbers **)

fun sift p = Seq.filter (fn n => n mod p <> 0);

fun sieve (Cons(p,nf)) = Cons(p, fn()=> sieve (sift p (nf())));

val primes = sieve (Seq.from 2);


(** Numerical methods: square roots **)

fun nextApprox a x = (a/x + x) / 2.0;

fun within (eps:real) (Cons(x,xf)) =
      let val Cons(y,yf) = xf() 
      in  if Real.abs(x-y) < eps then y
	  else within eps (Cons(y,yf))
      end;

fun qroot a = within 1E~12 (Seq.iterates (nextApprox a) 1.0);


(** Interleaving and sequences of sequences **)

fun makeqq (xq,yq) = Seq.map (fn x=> Seq.map (pair x) yq) xq;

fun takeqq (xqq, (m,n)) = List.map (secr Seq.take n) (Seq.take (xqq,m));

fun enumerate Nil  = Nil
  | enumerate (Cons(Nil, xqf)) = enumerate (xqf())
  | enumerate (Cons(Cons(x,xf), xqf)) =
        Cons(x, fn()=> Seq.interleave(enumerate (xqf()), xf()));

val double =  fn n => n*2;
fun powof2 n = repeat double n 1;
fun pack_(i,j) = powof2(i-1) * (2*j - 1);

val pairqq = makeqq (Seq.from 1, Seq.from 1);
val nqq = Seq.map (Seq.map pack_) pairqq;


(*** Searching ***)

fun depthFirst next x =
    let fun dfs [] = Nil
	  | dfs(y::ys) = Cons(y, fn()=> dfs(next y @ ys))
    in  dfs [x]  end;

fun breadthFirst next x =
    let fun bfs [] = Nil
	  | bfs(y::ys) = Cons(y, fn()=> bfs(ys @ next y))
    in  bfs [x]  end;


(** Generating palindromes **)

fun nextChar l = [#"A"::l, #"B"::l, #"C"::l];
fun isPalin l = (l = rev l);

fun show n csq = map implode (Seq.take(csq,n));

show 8 (Seq.filter isPalin (breadthFirst nextChar []));


(** 8 Queens Problem **)

fun safeQueen oldqs newq =
    let fun nodiag (i, []) = true
          | nodiag (i, q::qs) =
              Int.abs(newq-q)<>i andalso nodiag(i+1,qs)
    in  not (newq mem oldqs) andalso nodiag (1,oldqs)  end;

fun upto (m,n) = 
    if m>n then []  else  m :: upto(m+1,n);

fun nextQueen n qs =
    map (secr op:: qs) (List.filter (safeQueen qs) (upto(1,n)));

fun isFull n qs = (length qs=n);

fun depthQueen n = Seq.filter (isFull n) (depthFirst (nextQueen n) []);


(** Depth-first iterative deepening **)

fun depthIter next x =
    let fun dfs k (y, sf) = 
	     if k=0 then fn()=> Cons(y,sf)
	     else foldr (dfs (k-1)) sf (next y)
        fun deepen k = dfs k (x, fn()=> deepen (k+1)) ()
    in  deepen 0  end;

