structure X = struct exception e of int
	      end;
structure Y = X : sig val e : int -> exn end;

(raise (Y.e 0)) handle X.e _ => "OK" | _ => "FAIL";


functor F() = struct exception e of int end;

structure X = F();

structure Y = F();

(raise (Y.e 0)) handle Y.e _=> "OK" | _ => "FAIL";
(raise (Y.e 0)) handle X.e _=> "FAIL" | _ => "OK";


(* static and dyamic exception rebindings *)

exception s;
let exception d; exception t = s and e = d in (t,s) end; 



	             