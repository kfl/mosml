(* Callback -- registering ML values with C, e.g. to facilitate callbacks.
   1999-08-09, sestoft@dina.kvl.dk.  
*)

fun fail fcn msg = 
    raise Fail ("Callback." ^ fcn ^ ": " ^ msg)

local 
    prim_val sml_init_register : (string -> 'a option ref option) -> unit 
                             = 1 "sml_init_register"
    prim_val alloc_mlvalptr_ : 'a -> 'a ref = 1 "alloc_valueptr"
    
    prim_val magic : 'a -> 'b = 1 "identity"

    open Polyhash

    val mltable = mkPolyTable(17, Subscript) 
(* cvr: we omit the type constraint because it causes a scope violation later
	        : (string, '_a option ref) hash_table  
*)

    (* The function called from C to look up the ML values: *)

    fun get_valueptr nam = 
	case Polyhash.peek mltable nam of
	    res as SOME (ref (SOME _)) => res
	  | _                          => NONE
in
    (* To register a value v under name nam, insert ref(SOME v) in 
        the hash table.  The ref cell must be in the old heap. *)

    fun register (nam : string) (v : 'a) : unit = 
	case peekInsert mltable (nam, alloc_mlvalptr_ (SOME (magic v))) of
	    NONE                 => ()
	  | SOME r => (case r of 
			   ref NONE     => r := SOME (magic v)
			 | ref (SOME _) => 
			       fail "register" (nam ^ " already registered"))

    (* To unregister a registered value, overwrite its reference with
       NONE to prevent future accesses.  Note that since the reference
       is still reachable from the mltable, it will not be removed by
       the garbage collector. *)

    fun unregister nam = 
	case Polyhash.peek mltable nam of
	    NONE   => fail "unregister" (nam ^ " never registered")
	  | SOME r => r := NONE

    (* A value is registered if it is in the hash table, and its
       reference is not NONE *)
    
    fun isRegistered (nam : string) =
	case Polyhash.peek mltable nam of
	    SOME (ref (SOME _)) => true
	  | _                   => false

    val _ = (register "Callback.register" register;
	     register "Callback.unregister" unregister;
	     sml_init_register get_valueptr)
end



prim_type cptr; (* A naked C pointer; this is OK if to a static
		   variable or a function *)

local 
    open Polyhash

    val cfuntable = mkPolyTable(17, Subscript) : (string, cptr) hash_table

    (* To register a C pointer cptr, insert it in the hash table: *)

    fun registercptr (nam : string) (cptr : cptr) : unit = 
	case peekInsert cfuntable (nam, cptr) of
	    NONE   => ()
	  | SOME _ => fail "registercptr" (nam ^ " already registered")
in
    fun getcptr nam : cptr = 
	case peek cfuntable nam of
	    NONE      => fail "getcptr" (nam ^ " not registered")
	  | SOME cptr => cptr

    prim_val var  : cptr -> 'b                             = 1 "c_var"
    prim_val app1 : cptr -> 'a1 -> 'b                      = 2 "cfun_app1"
    prim_val app2 : cptr -> 'a1 -> 'a2 -> 'b               = 3 "cfun_app2"
    prim_val app3 : cptr -> 'a1 -> 'a2 -> 'a3 -> 'b        = 4 "cfun_app3"
    prim_val app4 : cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'b = 5 "cfun_app4"
    prim_val app5 : cptr -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b 
                                                           = 6 "cfun_app5"

    (* Initialization: pass the registercptr function to C code: *)

    val _ = register "Callback.registercptr" registercptr;
end
