local
  open Const Code_dec Buffcode;
in

type reloc_table = 
    { literals  : (StructConstant, int list ref) Hasht.t, 
      reloclist : (reloc_info * int) list ref };

val reloc_info : reloc_table = 
    { literals = Hasht.new 17, reloclist = ref [] }

fun reloc_reset () =
    let val { literals, reloclist } = reloc_info 
    in Hasht.clear literals; reloclist := [] end

fun enter info =
    let val { reloclist, ... } = reloc_info
    in reloclist := (info, !out_position) :: !reloclist end

fun slot_for_literal sc =
    let val { literals, reloclist } = reloc_info 
    in
	(case Hasht.peek literals sc of
	     SOME addrs => addrs := !out_position :: !addrs
	   | NONE       => Hasht.insert literals sc (ref [!out_position]));
	out_short 0
    end

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
    let val { literals, reloclist } = reloc_info 
	fun getlitaddrs sc (ref addrs) acc = (sc, addrs) :: acc
	val res = (Hasht.fold getlitaddrs [] literals, 
		   List.rev (!reloclist))
    in
	reloc_reset();
	res
    end
end;
