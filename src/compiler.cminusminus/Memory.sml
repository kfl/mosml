(* To control the runtime system and bytecode interpreter *)

local
  open Obj;
in

prim_val global_data : obj Vector.vector = 0 "global_data"
prim_val realloc_global_data : int -> unit = 1 "realloc_global";
prim_val static_alloc : int -> string = 1 "static_alloc";
prim_val static_free : string -> unit = 1 "static_free";
prim_val static_resize : string -> int -> string = 2 "static_resize";
prim_val gc_full_major : unit -> unit = 1 "gc_full_major";
prim_val interprete : bool -> string -> int -> int -> obj = 4 "start_interp";
prim_val available_primitives : unit -> string Vector.vector
                                          = 1 "available_primitives";

(* The following primitives are not implemented by *)
(* the `standard' Caml Light system. *)

prim_val sml_int_of_string : string -> int = 1 "sml_int_of_string";
prim_val sml_hex_of_string : string -> int = 1 "sml_int_of_hex";
prim_val sml_float_of_string : string -> real = 1 "sml_float_of_string";
prim_val sml_string_of_int     : int -> string  = 1 "sml_string_of_int";
prim_val sml_hexstring_of_word : word -> string = 1 "sml_hexstring_of_word";
prim_val sml_string_of_float : real -> string = 1 "sml_string_of_float";
prim_val sml_makestring_of_char : char -> string
                                            = 1 "sml_makestring_of_char";
prim_val sml_makestring_of_string : string -> string
                                            = 1 "sml_makestring_of_string";

end;
