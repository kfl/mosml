local
  open Const Code_dec Buffcode;
in

type reloc_table = (reloc_info * int) list;

val reloc_info = ref ([] : reloc_table);

fun reloc_reset () =
  reloc_info := []
;

fun enter info =
  reloc_info := (info, !out_position) :: !reloc_info
;

fun slot_for_literal sc =
  (enter (Reloc_literal sc); out_short 0)
;

fun slot_for_get_global uid =
  (enter (Reloc_getglobal uid); out_short 0)
;

fun slot_for_set_global uid =
  (enter (Reloc_setglobal uid); out_short 0)
;

fun slot_for_c_prim name =
  (enter (Reloc_primitive name); out_short 0)
;

fun get_reloc_info () =
  let val res = !reloc_info in
    reloc_info := [];
    List.rev res
  end
;

end;
