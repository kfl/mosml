(* systematic tests *)
(* these produce *alot* of output so we enclose them in a local
   declaration *)
(* additional declarations *)

local
signature L = sig type t = unit
                  val x: unit
		  exception e;
		  prim_val p : unit -> unit = 1 "identity";
		  structure X: sig end;                  
		  functor F:functor(X:sig end)->sig end 
	      end;

signature G = sig end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;
functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;

(* scheme match *)

signature L = sig val x : ('a -> 'a) -> ('b -> 'b) end;
signature G = sig val x : ('a -> 'a) -> ('a -> 'a) end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;
functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;

val 'a ok = 
let 
      signature L = sig val x : ('a -> 'a) -> ('b -> 'b) end;
      signature G = sig val x : ('a -> 'a) -> ('a -> 'a) end;

      functor L = functor (L:L)=>
	  L:G;
      functor LL = functor(LL:functor(L:L)->L)=>
	  LL:functor(L:L)->G;
      functor GL = functor(GL:functor(L:G)->L)=>
	  GL:functor(L:L)->L;
      functor YL = functor(YL:sig structure Y:L end)=>
	  YL:sig structure Y: G end;
      functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
	  L_YL:functor(L:L)->sig structure Y: G end;
      functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
	  YG_L:functor(YL:sig structure Y: L end)->L;
      functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
	  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
      functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
	  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;
in ()
end;

(* status match *)

signature L = sig 
                  val x: unit

		  exception e;

		  exception f 
    
                  datatype t = C

		  prim_val p : unit -> unit = 1 "identity";
		  prim_val q : unit -> unit = 1 "identity";
                  
	      end;
signature G = sig 
                  val x : unit

		  exception e

		  val f : exn 

                  type t; val C : t;
		  
		  prim_val p : unit -> unit = 1 "identity";
                  val q: unit->unit

	      end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;
functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;

(* opaque match *)
signature L = sig type t
                  type u = unit
                  type v = unit -> unit
		  datatype w = C of unit
		  datatype x = F of unit -> unit
		  eqtype z

		  eqtype e
		  type f = unit
		  datatype g = D of unit

		  prim_EQtype p
		  prim_EQtype q
		  prim_EQtype r
	      end;
signature G = sig type t
                  type u
		  type v
		  type w
		  type x
		  type z

		  eqtype e
		  eqtype f
		  eqtype g

		  prim_EQtype p
		  type q
		  eqtype r
	      end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;
functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;


(* transparent match *)

signature L = sig type ('a,'b) t = 'a * 'b end;
signature G = sig type ('c,'d) t = 'c * 'd end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;
functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;

(* eta match *)

signature L = sig 
		  type t 
		  type u = t 

		  type 'a t1 
		  type 'a u1 = 'a t1 
    
		  type ('a,'b) t2 
		  type ('a,'b) u2 = ('a,'b) t2 

		  datatype d = C | D of d 
		  structure E : sig datatype e = datatype d end

		  datatype 'a d1 = C1 of 'a | D1 of 'a d1 
		  structure E1 : sig datatype e1 = datatype d1 end
    
		  datatype ('a,'b) d2 =  C2 of 'a * 'b | D2 of ('b,'a) d2 
		  structure E2 : sig datatype e2 = datatype d2 end
	      end;
signature G = sig 
		  type  u 
		  type  t = u 

		  type 'a u1 
		  type 'a t1 = 'a u1 

		  type ('a,'b) u2 
		  type ('a,'b) t2 = ('a,'b) u2 

		  structure E : sig datatype e = C | D of e  end
		  datatype d = datatype E.e

		  structure E1 : sig datatype 'a e1 = C1 of 'a | D1 of 'a e1 end
		  datatype d1 = datatype E1.e1 
    
		  structure E2 : sig datatype ('a,'b) e2 = C2 of 'a * 'b | D2 of ('b,'a) e2  end
		  datatype d2 = datatype E2.e2  

	      end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;
functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;

(* datatype match *)

structure X = struct 
		  datatype t = C of (t,unit u) v 
		  and 'a u = D of t 
		  and ('a,'b) v = E of ('a * 'b) u
	      end
signature L = sig datatype t = C of (t,unit u) v 
		  and 'a u = D of t 
		  and ('a,'b) v = E of ('a * 'b) u
		  structure X : sig datatype t = datatype X.t
		                    datatype u = datatype X.u
				    datatype v = datatype X.v
				end
		  structure Y : sig datatype t = datatype X.t
		                    datatype u = datatype X.u
				    datatype v = datatype X.v
				end
	      end;
signature G = sig datatype t = C of (t,unit u) v 
		  and 'a u = D of t 
		  and ('a,'b) v = E of ('a * 'b) u
		  structure X : sig datatype t = datatype X.t
				    datatype u = datatype X.u
				    datatype v = datatype X.v
				end
		  structure Y : sig datatype t = C of (t,unit u) v 
				    and 'a u = D of t 
				    and ('a,'b) v = E of ('a * 'b) u
				end
	      end;

functor L = functor (L:L)=>

  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;
functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;

(* applicative/generative match *)

signature L = functor X:sig type t end ->sig type u end;
signature G = functor(X:sig type t end)->sig type u end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;

(*  applicative generative match (cont.)
    we need to embed the functors in structures for these tests 
*)

signature L = sig functor F: functor  X:sig type t end ->sig type u end end;
signature G = sig functor F: functor(X:sig type t end)->sig type u end end;

functor YL = functor(YL:sig structure Y:L end)=>
  YL:sig structure Y: G end;
functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
  L_YL:functor(L:L)->sig structure Y: G end;
functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
  YG_L:functor(YL:sig structure Y: L end)->L;
functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;

(* end of tests *)
in 
    val matchsuc = "OK"
end;




 








 



