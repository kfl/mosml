structure Dynlib :> Dynlib =
struct

(* Ken Larsen (kla@it.dtu.dk) and sestoft@dina.kvl.dk 1998-01-12 1999-01-07 *)
	
prim_type dlHandle_			(* A pointer outside the ML heap *)
prim_type symHandle_			(* A pointer outside the ML heap *)

type dlHandle  = { hdl    : dlHandle_,  closed : bool ref, lib : string }
type symHandle = { symhdl : symHandle_, closed : bool ref }

exception Closed

datatype flag = RTLD_LAZY | RTLD_NOW

prim_val dlopen_  : string -> int -> dlHandle_        = 2 "dynlib_dlopen"
prim_val dlsym_   : dlHandle_ -> string -> symHandle_ = 2 "dynlib_dlsym"
prim_val dlclose_ : dlHandle_ -> unit                 = 1 "dynlib_dlclose"

prim_val var_  : symHandle_ -> 'b                             = 1 "c_var"
prim_val app1_ : symHandle_ -> 'a1 -> 'b                      = 2 "cfun_app1"
prim_val app2_ : symHandle_ -> 'a1 -> 'a2 -> 'b               = 3 "cfun_app2"
prim_val app3_ : symHandle_ -> 'a1 -> 'a2 -> 'a3 -> 'b        = 4 "cfun_app3"
prim_val app4_ : symHandle_ -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b = 5 "cfun_app4"
prim_val app5_ : symHandle_ -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b = 6 "cfun_app5"

fun dlopen { lib : string, flag : flag, global : bool } = 
    let val opencode = (case flag of RTLD_LAZY => 0 | RTLD_NOW => 1)
	               + 2 * (if global then 1 else 0)
    in { hdl = dlopen_ lib opencode, closed = ref false, lib = lib } end
    handle Fail msg => raise Fail (msg ^ " while loading C library " ^ lib)

fun dlsym { hdl : dlHandle_, closed : bool ref, lib : string } sym =
    if !closed then raise Closed
    else { symhdl = dlsym_ hdl sym, closed = closed }
	 handle Fail msg => 
	     raise Fail (msg ^ " when binding " ^ sym ^ " in " ^ lib)

fun dlclose { hdl : dlHandle_, closed : bool ref, lib } =
    if !closed then ()
    else (dlclose_ hdl; closed := true)
	 handle Fail msg => raise Fail (msg ^ " when closing " ^ lib)

fun var { symhdl : symHandle_, closed : bool ref } =
    if !closed then raise Closed
    else var_ symhdl

fun app1 { symhdl : symHandle_, closed : bool ref } x1 =
    if !closed then raise Closed
    else app1_ symhdl x1

fun app2 { symhdl : symHandle_, closed : bool ref } x1 x2 =
    if !closed then raise Closed
    else app2_ symhdl x1 x2

fun app3 { symhdl : symHandle_, closed : bool ref } x1 x2 x3 =
    if !closed then raise Closed
    else app3_ symhdl x1 x2 x3

fun app4 { symhdl : symHandle_, closed : bool ref } x1 x2 x3 x4 =
    if !closed then raise Closed
    else app4_ symhdl x1 x2 x3 x4

fun app5 { symhdl : symHandle_, closed : bool ref } x1 x2 x3 x4 x5 =
    if !closed then raise Closed
    else app5_ symhdl x1 x2 x3 x4 x5

end

