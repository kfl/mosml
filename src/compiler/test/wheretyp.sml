datatype t = C of t;
datatype 'a u = D of 'a;
datatype ('a,'b) v = E of ('a * 'b);

signature S = sig 
        datatype t = C of t 
	datatype 'a u = D of 'a;
	datatype ('a,'b) v = E of ('a * 'b)
end;

signature Ok = S where type t = t and type 'a u = 'a u and type ('a,'b) v = ('a,'b) v;

signature FailWeirdButValidSML = S where type t = int;
signature Fail = S where type t = int * int;

signature FailWeirdButValidSML = S where type 'a u = 'a list;
signature Fail = S where type 'a u = int u;
signature Fail = S where type 'a u = 'a * 'a;

datatype ('a,'b) w = W;
type ('a,'b,'c) x = ('a,'b) w;
type ('a,'b) y = ('a,'a) w;
signature FailWeirdButValidSML = S where type ('a,'b) v = ('a,'b) w;
signature FailWeirdButValidSML = S where type ('a,'b) v = ('a,'b,int) x;
signature Fail = S where type ('a,'b) v = ('a,'a)w;
signature Fail = S where type ('a,'b) v = ('a,'a)w;
signature Fail = S where type ('a,'b) v = ('a,'b)y;

datatype t = C of bool | D of int;

signature Fail = sig datatype u = C of bool end where type u = t;

signature Fail = sig datatype u = C of bool | D of bool end where type u = t;

signature Fail = sig datatype u = C of bool | D of int end where type u = int;

signature OK = sig datatype u = C of bool | D of int end where type u = t;

(* check indirections in the realization *)

type s = t;

signature Fail = sig datatype u = C of bool end where type u = s;

signature Fail = sig datatype u = C of bool | D of bool end where type u = s;

signature OK = sig datatype u = C of bool | D of int end where type u = s;

(* check indirections in the spec *)

signature Fail = sig datatype u = C of bool type v = u end where type v = t;

signature Fail = sig datatype u = C of bool | D of bool type v = u end where type v = t;

signature OK = sig datatype u = C of bool | D of int type v = u end where type v = t;

(* the reason we reject more where type constraints than SML is to preclude elaboration of the following *)
local datatype t = C of int;
      signature Fail = sig datatype u = C of bool end where type u = t;
      structure X = rec(X:S)struct datatype u = datatype X.u end 
      val X.C b = C 0
in 
end;


