(**** ML Programs from Chapter 2 of

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

(*** Records ***)

val henryV = 
  {name		= "Henry V",
   born		= 1387,
   crowned	= 1413,
   died		= 1422,
   quote	= "Bid them achieve me and then sell my bones"};

val henryVI =
  {name		= "Henry VI",
   born		= 1421,
   crowned	= 1422,
   died		= 1471,
   quote	= "Weep, wretched man, I'll aid thee tear for tear."};

val richardIII =
  {name		= "Richard III",
   born		= 1452,
   crowned	= 1483,
   died		= 1485,
   quote	= "Plots have I laid, inductions dangerous..."};

type king = {name: string, 
             born: int, 
             crowned: int, 
             died: int, 
             quote: string};



(*** Raising to an integer power ***)

(*defined only for k>0*)
fun power(x,k) : real =
    if k=1 then x
    else if k mod 2 = 0 then   power(x*x, k div 2)
                        else x*power(x*x, k div 2);



(*** Fibonacci numbers ***)

(*Naive implementation!*)
fun naive_fib 0 = 0
  | naive_fib 1 = 1
  | naive_fib n = naive_fib(n-2) + naive_fib(n-1);


(*A faster implementation, but not tail recursive*)
fun nextfib(prev, curr:int) = (curr, prev+curr);

fun fibpair (n) = 
    if n=1 then (0,1)
    else nextfib(fibpair(n-1));


(*Good example of typechecking with overloading*)
fun itfib (n, prev, curr) : int =
    if n=1 then curr  (*does not take n=0*)
    else itfib (n-1, curr, prev+curr);

fun fib (n) = itfib(n,0,1);



(*** Real square roots ***)

fun findroot (a, x, acc) =
    let val nextx = (a/x + x) / 2.0
    in  if abs (x-nextx) < acc*x  
        then nextx  
        else findroot (a, nextx, acc)
    end;

fun sqroot a = findroot (a, 1.0, 1.0E~10);


fun sqroot a = 
    let val acc = 1.0E~10
        fun findroot x =
            let val nextx = (a/x + x) / 2.0
            in  if abs (x-nextx) < acc*x  
                then nextx  
                else findroot nextx
            end
    in  findroot 1.0  end;


(*** Mutually recursive functions: approximating pi ***)

fun pos d = neg(d-2.0) + 1.0/d
and neg d = if d>0.0  then  pos(d-2.0) - 1.0/d  else  0.0;

(*Note!  The final term has the correct sign only if
   d = 1 (mod 4)	in pos(d)
   d = 3 (mod 4)	in neg(d)
*)

fun piapprox n = 4.0 * pos (real (4*n+1));


(*** Introduction to modules ***)

structure Complex =
  struct
  type t = real*real;
  val zero = (0.0, 0.0);
  fun sum   ((x,y), (x',y')) = (x+x', y+y') : t;
  fun diff  ((x,y), (x',y')) = (x-x', y-y') : t;
  fun prod  ((x,y), (x',y')) = (x*x' - y*y', x*y' + x'*y) : t;
  fun recip (x,y) = 
	      let val t:real = x*x + y*y
	      in  (x/t, ~y/t)  end
  fun quo   (z,z') = prod(z, recip z');
  end;

signature ARITH =
   sig
   type t
   val zero : t
   val sum  : t * t -> t
   val diff : t * t -> t
   val prod : t * t -> t
   val quo  : t * t -> t
   end;
