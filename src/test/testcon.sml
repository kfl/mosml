(* Rebinding and respecification as constructors of the identifiers
 *	true, false, it, nil, ::, ref 
 * is no longer permitted. 
 *)

datatype t = true;
datatype t = false;
datatype t = it;
datatype t = nil;
datatype t = op ::;
datatype t = ref;

datatype t = true of int;
datatype t = false of int;
datatype t = it of int;
datatype t = nil of int;
datatype t = op :: of int;
datatype t = ref of int; 

exception true;
exception false;
exception it;
exception nil;
exception op ::;
exception ref;

exception true = Div;
exception false = Div;
exception it = Div;
exception nil = Div;
exception op :: = Div;
exception ref = Div;

exception true of int;
exception false of int;
exception it of int;
exception nil of int;
exception op :: of int;
exception ref of int; 

abstype t = true of int with end;
abstype t = false of int with end;
abstype t = it of int with end;
abstype t = nil of int with end;
abstype t = op :: of int with end;
abstype t = ref of int with end;  
