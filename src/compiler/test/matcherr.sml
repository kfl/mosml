(* test generation of signature mismatch errors *)

(* misc tests --- not exhaustive *)

structure X = struct open General end: sig val + : (int * int) -> int end;
structure X = struct val x = fn 1 => Div end : sig exception x of int end;
structure X = struct val x = fn y => y end : sig prim_val  x : 'a ->'a = 1 "identity" end;
exception e (* static *);
structure X = struct exception e = e end : sig exception e (* dynamic *) end;
structure X = struct val x = fn 1 => 1 end : sig val x : 'a -> 'a end;
structure X = struct val x = fn 1 => Div end : sig exception x of int end;
structure X = struct val x = fn y => y end : sig prim_val  x : 'a ->'a = 1 "identity" end;
structure X = struct datatype t = C of int end : sig datatype t = C of bool end;

signature S = sig type t end where type 'a t = int;
signature S = sig structure X : sig type t end end where type 'a X.t = int;
signature S = sig structure Y : sig structure X : sig type t end end end where type 'a Y.X.t = int;

(* module mismatches *)
structure X = struct end:functor(X:sig end)->sig end;
functor X = (functor(X:sig end)=>struct end) : sig end;
functor X = (functor(X:sig end)=>struct end): functor(X:sig end)->functor(X:sig end)->sig end;
functor X = (functor(X:sig end)=>functor(X:sig end)=>struct end): functor(X:sig end)->sig end;
functor X = (functor(X:functor(X:sig end)->sig end)=>struct end): functor(X:sig end)->sig end;
functor X = (functor(X:sig end)=>struct end): functor(F:functor(X:sig end)->sig end)->sig end;


(* systematic tests *)

(* missing type declarations *)

signature G = sig type t = unit end;
signature L = sig end;

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

(* missing value declarations *)
 
signature G = sig val x: unit end;
signature L = sig end;

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

(* missing structure declarations *)

signature G = sig structure X: sig end end;
signature L = sig end;

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

(* missing functor declarations *)

signature G = sig functor F:functor(X:sig end)->sig end 
	      end;
signature L = sig end;

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

(* scheme mismatch *)

signature G = sig val x : 'a -> 'a end;
signature L = sig val x : int -> int end;

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

(* scheme mismatch with free type parameter *)
val 'b fail = 
let 
      signature L = sig val x : ('a -> 'a) -> ('b -> 'b) end;
      signature G = sig val x : ('a -> 'a) -> ('a -> 'a) end;

      functor L = functor (L:L)=>
	  L:G;
in unit
end
;
val 'b fail = 
let 
      signature L = sig val x : ('a -> 'a) -> ('b -> 'b) end;
      signature G = sig val x : ('a -> 'a) -> ('a -> 'a) end;
      functor LL = functor(LL:functor(L:L)->L)=>
	  LL:functor(L:L)->G;
in unit
end
;
val 'b fail = 
let 
      signature L = sig val x : ('a -> 'a) -> ('b -> 'b) end;
      signature G = sig val x : ('a -> 'a) -> ('a -> 'a) end;
      functor GL = functor(GL:functor(L:G)->L)=>
	  GL:functor(L:L)->L;
in unit
end
;
val 'b fail = 
let 
      signature L = sig val x : ('a -> 'a) -> ('b -> 'b) end;
      signature G = sig val x : ('a -> 'a) -> ('a -> 'a) end;
      functor YL = functor(YL:sig structure Y:L end)=>
	  YL:sig structure Y: G end;
in unit
end
;
val 'b fail = 
let 
      signature L = sig val x : ('a -> 'a) -> ('b -> 'b) end;
      signature G = sig val x : ('a -> 'a) -> ('a -> 'a) end;
      functor L_YL = functor(L_YL:functor(L:L)->sig structure Y:L end)=>
	  L_YL:functor(L:L)->sig structure Y: G end;
in unit
end
;
val 'b fail = 
let 
      signature L = sig val x : ('a -> 'a) -> ('b -> 'b) end;
      signature G = sig val x : ('a -> 'a) -> ('a -> 'a) end;
      functor YG_L = functor(YG_L:functor(YG:sig structure Y:G end)->L)=>
	  YG_L:functor(YL:sig structure Y: L end)->L;
in unit
end
;
val 'b fail = 
let 
      signature L = sig val x : ('a -> 'a) -> ('b -> 'b) end;
      signature G = sig val x : ('a -> 'a) -> ('a -> 'a) end;
      functor L_YG_L = functor(L_YG_L:functor(L:L)->functor(YG:sig structure Y:G end)->L)=>
	  L_YG_L:functor(L:L)->functor(YL:sig structure Y:L end)->L;
in unit
end
;
val 'b fail = 
let 
      signature L = sig val x : ('a -> 'a) -> ('b -> 'b) end;
      signature G = sig val x : ('a -> 'a) -> ('a -> 'a) end;
      functor FLYGL = functor(FLYGL:sig functor F: functor(L:L)->functor(YG:sig structure Y:G end)->L end)=>
	  FLYGL:sig functor F: functor(L:L)->functor(YL:sig structure Y:L end)->L end;
in ()
end;

(* status mismatch *)

signature G = sig exception x end;
signature L = sig val x : exn end;

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

(* conenv mismatch *)

signature G = sig datatype t = C of int end;
signature L = sig datatype t = D of int end;

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

(* arity mismatch *)

signature G = sig type t = unit end;
signature L = sig type 'a t = unit end;

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

(* ref equality mismatch *)

signature G = sig datatype myref = datatype ref end;
signature L = sig type 'a myref = 'a end;

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

(* ref equality mismatch *)

signature G = sig prim_EQtype t end;
signature L = sig type t end;

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

(* equality mismatch *)

signature G = sig eqtype t end;
signature L = sig type t = unit -> unit end;

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

(* transparent mismatch *)

signature G = sig type t = unit end;
signature L = sig type t = unit -> unit end;

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

(* datatype mismatch *)

signature G = sig datatype t = C end;
signature L = sig type t = unit end;

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

(* scope violations *)

signature G = functor X:sig type t end ->sig type u end;
signature L = functor(X:sig type t end)->sig type u end;

functor L = functor (L:L)=>
  L:G;
functor LL = functor(LL:functor(L:L)->L)=>
  LL:functor(L:L)->G;
functor GL = functor(GL:functor(L:G)->L)=>
  GL:functor(L:L)->L;

(* scope violations (cont.)
   we need to embed the functors in structures for these tests *)

signature G = sig functor F: functor  X:sig type t end ->sig type u end end;
signature L = sig functor F: functor(X:sig type t end)->sig type u end end;

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





 



