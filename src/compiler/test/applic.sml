(* coercing an applicative to a generative functor should succeed *)
functor ok(F:functor X:sig type t end -> sig type u end) =
    op F:functor (X:sig type t end) -> sig type u end;

(* coercing a generative to an applicative functor should fail *)
functor fail(F:functor(X:sig type t end) -> sig type u end) =
    op F:functor X:sig type t end -> sig type u end;

(* tests for applicative functors and signatures: need to improve! *)

signature S =  functor X:sig type w type t = w end -> sig type u end;

functor F = (functor X:sig type t  end => struct type u = X.t * X.t end):S;

functor G = (functor X:sig type t  end  => struct type u = int * int end):S;


functor Fail = (functor (X:sig type t  end) => struct datatype u = C of X.t * X.t end):S;

(* test printing of applicative datatypes *)

(* unary dependencies *)

functor F X:sig type t val x: t end =
struct 
    datatype u = 
	C of X.t 
      |  D of u  
    val y = C X.x 
    val z = D y 
end;

structure X = F(type t = string val x = "x");
open X;

(* binary dependencies *)

functor F X:sig type s val w: s type t val x: t  end =
struct
    datatype u = 
	C of X.s * X.t 
      | D of u  
    val y = C (X.w,X.x)  
    val z = D y 
end;

structure X = F(type t = string val x = "x" type s = bool val w = true);
open X;

(* binary dependencies and non-regularity *)
functor F X:sig type s val w: s type t val x: t  end =
struct 
    datatype 'a u = 
	C of X.s * X.t * 'a 
      | D of ('a * 'a) u 
    val y = C (X.w,X.x,1)  
    val z = D (C (X.w,X.x,(1,1)))
end;

structure X = F(type t = string val x = "x" type s = bool val w = true);
open X;



functor A = functor X:sig type a and b val a: a and b:b  end => 
		 struct datatype u = C of X.a * X.b
		     val y= C (X.a,X.b) end;

structure A = A(struct type a = int and b = bool val a = 1 and b = false end);
open A;

(* no more mister nice guy *)

functor A = functor X:sig type a and b and 'a c val a: a and b:b and f: 'a -> 'a c  end => 
		 struct datatype u = C of X.a * X.b
                        datatype v = B | D of X.a * X.b * v
		        val u = C (X.a,X.b) 
                        val v = D (X.a,X.b,D(X.a,X.b,D(X.a,X.b,B)))
                        datatype 'a w = B of 'a | D of ('a * 'a) w
                        val w = D (B ((X.a,X.b),(X.a,X.b)))
                        datatype 'a x = B of 'a | D of ('a X.c) x
                        val x = D (B (X.f(X.a,X.b)))
		 end;
structure A = A(struct type a = int and b = bool type 'a c = {l:'a,r: 'a} val a = 1 and b = false val f = fn a => {l=a,r=a} end);
open A;












