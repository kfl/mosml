local
  open Mixture Const;
in

(* Relocation information *)

datatype reloc_info =
    Reloc_getglobal of (QualifiedIdent * int)  (* reference to a global *)
  | Reloc_literal of StructConstant            (* structured constant *)
  | Reloc_primitive of string                  (* C primitive number *)
  | Reloc_setglobal of (QualifiedIdent * int)  (* definition of a global *)
;

type compiled_phrase =
{
  cph_pos: int,                        (* Position of start of code *)
  cph_len: int,                        (* Length of code *)
				       (* What to patch *)
  cph_reloc: (StructConstant * int list) list * (reloc_info * int) list, 
  cph_pure: bool                       (* Can be omitted or not *)
};

type compiled_unit_tables =
{
  cu_phrase_index: compiled_phrase list,
  cu_exc_ren_list: (QualifiedIdent * (QualifiedIdent * int)) list,
  cu_val_ren_list: (string * int) list,
  cu_sig_stamp:    SigStamp,
  cu_mentions:     (string, SigStamp) Hasht.t
};

end;
