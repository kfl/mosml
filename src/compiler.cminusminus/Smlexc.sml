(* Predefined SML exceptions *)

local
    open Obj Const Fnlib Config Mixture Types
in
    (* The exn names and types of SML Basis Library exceptions *)

    prim_val syserr_ref : string ref = 0 "exn_syserr"
    prim_val io_ref     : string ref = 0 "exn_io"

(* ps: temporary fix when bootstrapping:
    val syserr_ref = ref "Fix1";
    val io_ref = ref "Fix2";
*)
    val type_of_syserror_exn = (* Must match actual type of OS.SysErr *)
	type_pair type_string (type_option type_syserror);

    val type_of_io_exn =       (* Must match actual type of IO.Io     *)
	type_rigid_record 
	[(STRINGlab "cause",    type_exn), 
	 (STRINGlab "function", type_string),
	 (STRINGlab "name",     type_string)];

    fun decode_string (v : obj) = (magic_obj v : string)
    fun decode_real (v : obj) = (magic_obj v : real);
    prim_val sml_string_of_float : real -> string = 1 "sml_string_of_float";

    fun exnArgType (strref : string ref) (arg : obj) =
	if strref = syserr_ref then 
	    SOME type_of_syserror_exn
	else if strref = io_ref then 
	    SOME type_of_io_exn
	else if is_block arg then 
	    if obj_tag arg = stringTag then SOME type_string
	    else if obj_tag arg = realTag then SOME type_real
	    else NONE
	else (* may be int, char, bool, word8, ...  *)
	    NONE

    fun getExnStrref (v : obj) : string ref = 
	if is_block v andalso is_block(obj_field v 0) then 
	    magic_obj (obj_field v 0) : string ref
	else
	    fatalError "getExnName"
end

