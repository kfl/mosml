local

  open Code_dec Symtable;

  prim_val set_nth_char_ : string -> int -> char -> unit = 3 "set_nth_char";
  prim_val andb_      : int -> int -> int = 2 "and";
  prim_val rshiftsig_ : int -> int -> int = 2 "shift_right_signed";
  prim_val rshiftuns_ : int -> int -> int = 2 "shift_right_unsigned";


  fun patch_short buff pos v =
  (
    (* `set_nth_char` must not check the length of buff, *)
    (* because buff may be allocated outside the heap! *)
    set_nth_char_ buff pos (Char.chr (andb_ v 255));
    set_nth_char_ buff (pos+1) (Char.chr (rshiftuns_ v 8))
  );

in

(* To relocate a block of object bytecode *)

fun patch_object buff offset (stringlist, otherlist) =
    let fun relliteral (lit, poss) =
	    let val slot = get_slot_for_literal lit
		fun patchlit pos = patch_short buff (pos + offset) slot
	    in List.app patchlit poss end
	fun relother (Reloc_literal sc, pos) =
	    patch_short buff (pos + offset) (get_slot_for_literal sc)
	  | relother (Reloc_getglobal uid, pos) =
	    patch_short buff (pos + offset) (get_slot_for_variable uid)
	  | relother (Reloc_setglobal uid, pos) =
	    patch_short buff (pos + offset) (get_slot_for_defined_variable uid)
	  | relother (Reloc_primitive name, pos) =
	    patch_short buff (pos + offset) (get_num_of_prim name)
    in
	List.app relliteral stringlist;
	List.app relother  otherlist
    end
end;
