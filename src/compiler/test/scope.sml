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










