(* Obj.sml *)

prim_type obj;

prim_val repr          : 'a -> obj   = 1 "identity";
prim_val magic_obj     : obj -> 'a   = 1 "identity";
prim_val magic         : 'a -> 'b    = 1 "identity";
prim_val is_block      : obj -> bool = 1 "obj_is_block"
prim_val obj_tag       : obj -> int  = 1 "tag_of"
prim_val obj_size      : obj -> int  = 1 "vect_length"
prim_val obj_field     : obj -> int -> obj         = 2 "get_vect_item"
prim_val set_obj_field : obj -> int -> obj -> unit = 3 "set_vect_item"
prim_val obj_block     : int -> int -> obj         = 2 "obj_block"
prim_val update        : obj -> obj -> unit        = 2 "update"
