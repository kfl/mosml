local
  open BasicIO Const;
in

val literal_table : (int * StructConstant) list ref;
val get_slot_for_variable : QualifiedIdent * int -> int;
val get_slot_for_defined_variable : QualifiedIdent * int -> int;
val get_slot_for_literal : StructConstant -> int;
val number_of_globals : unit -> int;
val get_num_of_prim : string -> int;
val intOfTag : BlockTag -> int;
val exportPublicNames :
  string -> (QualifiedIdent * (QualifiedIdent * int)) list ->
  (string * int) list -> unit;
val reset_linker_tables : unit -> unit;
val save_linker_tables : outstream -> unit;
val init_linker_tables : unit -> unit;
val protect_linker_tables : (unit -> 'a) -> unit;

end;
