val ok = fn x => let type t = 'a in x: t end;

val ok = fn x => let type t = 'a -> 'b in x: t end;

type fail = 'a -> 'b;

val ok = fn x:'a => let type t = 'a in x: t end;

val ok = fn x:'b => let datatype d = C type t = 'b * d in (x,C): t;1 end;

val fail = fn x:'b => let datatype d = C type t = 'b * d in (x,C): t end;

val ok = fn x:'a => let datatype t = c of 'a in (fn c x => x) (c x) end;

local
val f = fn x:''a => [structure struct  datatype t = c of ''a end 
 	 	     as sig datatype t = c of ''a end];
in
structure Ok as sig datatype t = c of int end = f 1;
end;

local
val 'a f = fn () => [structure struct  datatype t = c of 'a end 
 	 	     as sig datatype t = c of 'a end];
in
structure Ok' as sig datatype t = c of unit->unit end = f () 
structure Ok as sig datatype t = c of bool->bool end = f ()
end;

(* this only works with valuepoly:=false because 'a would otherwise not be quantified *)
valuepoly:= false;
local 
val 'a x = [structure struct  datatype t = c of 'a end as sig datatype t = c of 'a end]
in
structure Ok' as sig datatype t = c of unit->unit end = x 
structure Ok as sig datatype t = c of bool->bool end = x
end;
valuepoly:=true;


val ok = fn x:'a => 
    let 
	structure S = struct val x:'a = x end 
    in S.x
    end;

val ok = 
    fn x:'a => let signature S = sig val x:'a end
	       in [structure struct val x = x end as S ]
	       end;

signature Fail = sig val x:'a->'a end;

val ok = fn x:'a => 
    let signature S = sig val x:'a end
    in [structure struct val x = x end as S ]
    end;

val fail = fn x:'a => 
    let signature S = sig val 'a x:'a end
    in [structure struct val x = x end as S ]
    end;

val fail = fn x:'a => 
    let signature S = sig val 'b x:'b end
    in [structure struct val x = x end as S ]
    end;

val ok = fn x:'a => 
    let signature S = sig val x:'a -> 'a end
    in [structure struct val x = fn x => x end as S ]
    end;

val ok = fn x:'a => 
    let signature S = sig val 'a x:'a -> 'a end
    in [structure struct val x = fn x => x end as S ]
    end;

val ok = fn x:'a => 
    let signature S = sig val 'b x:'b -> 'b end
    in [structure struct val x = fn x => x end as S ]
    end;


functor fail = 
    (functor(X:sig type t end)=>struct datatype u = C of X.t end)
    :functor X:sig type t end -> sig type u end;

(* tricky scope tests that check levels are maintained correctly *)

local val z = 1;val l = 2 ; val x1 = ref []  val x2: 'a list ref = ref [] in val y1 = x1 val y2 = x2 end;  
val fail : 'a list ref = y1;
val fail : 'a list ref = y2;

(* ok: the single topdec is correctly rejected *)
structure S = struct val x = 1; val r = ref [] end (* NB: no semicolon! *)
structure R : sig val r: 'a list ref end = S;

structure S = struct val x = 1; val y  = 2; val r = ref [] end 
structure Fail : sig val r: 'a list ref end = S;

structure U = struct val x = 1; val r = ref [] end; 
structure Fail : sig val r: 'a list ref end = U;

structure W = struct val r = ref [] end; 
structure Fail : sig val  r: 'a list ref end = W;

structure Y =
    struct  structure S = struct val x = 1; val r = ref [] end 
	    structure Fail : sig val 'a r: 'a list ref end = S;
    end;


(* a single topdec *)
val x = ref [] 
datatype t = C
val fail = x:=[C];

(* a sequence of topdecs *)
val x = ref [];
datatype t = C;
val fail = x:=[C];


structure Z = 
 struct val x = ref [];
        datatype t = C;
	val fail = x:=[C];
 end;

structure Ok = 
 struct datatype t = C;
        val x = ref [];
	val ok = x:=[C];
 end;

structure N = struct val x = ref [];
		     datatype t = C;
	       end;
val fail = N.x := [N.C];


structure M = 
 struct structure X = struct val a = 1; val b = 2; val x = ref [] end;
        datatype t = C;
	val fail = X.x:=[C];
 end;

structure O = 
 struct structure X = struct val x = ref [] end;
        datatype t = C;
	val fail = X.x:=[C];
 end;

local val x = ref [] in val y =  x val z = ref [] end;
val fail : 'a list ref = x;
val fail : 'a list ref = z;

(* fail should be rejected since X.x must be monomorphic *)
structure P = let val x = ref [] in struct val x = x end end 
val fail : 'a list ref = X.x;


(* Unlike the Definition, Mosml treats non-generalizable explicit variables arising from 
   non-expansive definitions as unification variables *)

val ok = let val x : 'a list ref = ref [] in x end;
val ok = let val a =1 val b = 2 val c = 3 val x : 'a list ref = ref [] in x end;

local val x : 'a list ref = ref [] in 
      val ('b) fail : 'b list ref = x
end;



local val a =1 val b = 2 val c = 3 val x : 'a list ref = ref []
in
      val ('b) fail : 'b list ref = x
end;

local val a =1 val b = 2 val c = 3 val x : 'a list ref = ref []
in
      val ('b) fail : 'b list ref = (fn y => y) x
end;

local 
      val a = 1 val b = 2 val c = 3 val x : 'a list ref = ref []
in
      val  ok : int list ref = (fn y => y) x
end;


val ok = 
    fn _ :'b =>
    let 
	val a = 1 val b = 2 val c = 3 val x : 'a list ref = ref []
    in
	x  : 'b list ref
    end;

local 
	val a = 1 val b = 2 val c = 3 val x : 'a list ref = ref []
in
	val fail = fn _ :'b => x : 'b list ref
end;

val ok = 
    fn _ :'b =>
    let 
	val a = 1 val b = 2 val c = 3 val x : 'a list ref = ref []
	val f = fn _ :'b => x : 'b list ref
    in
	x
    end;




