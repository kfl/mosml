(* Susp -- support for lazy evaluation 1995-05-22 *)

local 
    datatype 'a thunk = VAL of 'a | THUNK of unit -> 'a
    prim_val magic : 'a -> 'b = 1 "identity"
in
    type 'a susp = 'a thunk ref

    fun delay (f : unit -> 'a) = 
	magic (ref (THUNK (magic f))) : 'a susp

    fun force (su : 'a susp) : 'a = 
	case !su of
	    VAL v   => v 
	  | THUNK f => let val v = f () 
		       in su := VAL v; v end
end
