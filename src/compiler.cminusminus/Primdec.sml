(* Concrete syntax for primitive declarations *)

local
  open Const Prim Smlprim;

(* Must be sorted for efficient binary search lookup *)

val primitive_names = 
#[
  ("!=", Ptest Pnoteq_test),
  ("*int", Psmlmulint),
  ("*intunsig", Pmulint),
  ("*real", Pfloatprim Psmlmulfloat),
  ("+int", Psmladdint),
  ("+intunsig", Paddint),
  ("+real", Pfloatprim Psmladdfloat),
  ("-int", Psmlsubint),
  ("-intunsig", Psubint),
  ("-real", Pfloatprim Psmlsubfloat),
  ("/", Pfloatprim Psmldivfloat),
  ("<=int", Ptest (Pint_test PTle)),
  ("<=real", Ptest (Pfloat_test PTle)),
  ("<=string", Ptest (Pstring_test PTle)),
  ("<>int", Ptest (Pint_test PTnoteq)),
  ("<>real", Ptest (Pfloat_test PTnoteq)),
  ("<>string", Ptest (Pstring_test PTnoteq)),
  ("<int", Ptest (Pint_test PTlt)),
  ("<real", Ptest (Pfloat_test PTlt)),
  ("<string", Ptest (Pstring_test PTlt)),
  ("==", Ptest Peq_test),
  ("=int", Ptest (Pint_test PTeq)),
  ("=real", Ptest (Pfloat_test PTeq)),
  ("=string", Ptest (Pstring_test PTeq)),
  (">=int", Ptest (Pint_test PTge)),
  (">=real", Ptest (Pfloat_test PTge)),
  (">=string", Ptest (Pstring_test PTge)),
  (">int", Ptest (Pint_test PTgt)),
  (">real", Ptest (Pfloat_test PTgt)),
  (">string", Ptest (Pstring_test PTgt)),
  ("and", Pandint),
  ("div", Psmldivint),
  ("divunsig", Pdivint),
  ("field0", Pfield 0),
  ("field1", Pfield 1),
  ("field10", Pfield 10),
  ("field11", Pfield 11),
  ("field12", Pfield 12),
  ("field2", Pfield 2),
  ("field3", Pfield 3),
  ("field4", Pfield 4),
  ("field5", Pfield 5),
  ("field6", Pfield 6),
  ("field7", Pfield 7),
  ("field8", Pfield 8),
  ("field9", Pfield 9),
  ("get_nth_char", Pgetstringchar),
  ("get_vect_item", Pgetvectitem),
  ("identity", Pidentity),
  ("int_of_float", Pintoffloat),
  ("make_ref_vect", Pmakerefvector),
  ("make_vect", Pmakevector),
  ("mod", Psmlmodint),
  ("modunsig", Pmodint),
  ("not", Pnot),
  ("or", Porint),
  ("pred", Psmlpredint),
  ("quot", Psmlquotint),
  ("raise", Praise),
  ("real_of_int", Pfloatprim Pfloatofint),
  ("rem", Psmlremint),
  ("set_nth_char", Psetstringchar),
  ("set_vect_item", Psetvectitem),
  ("setfield0", Psetfield 0),
  ("setfield1", Psetfield 1),
  ("setfield10", Psetfield 10),
  ("setfield11", Psetfield 11),
  ("setfield12", Psetfield 12),
  ("setfield2", Psetfield 2),
  ("setfield3", Psetfield 3),
  ("setfield4", Psetfield 4),
  ("setfield5", Psetfield 5),
  ("setfield6", Psetfield 6),
  ("setfield7", Psetfield 7),
  ("setfield8", Psetfield 8),
  ("setfield9", Psetfield 9),
  ("shift_left", Pshiftleftint),
  ("shift_right_signed", Pshiftrightintsigned),
  ("shift_right_unsigned", Pshiftrightintunsigned),
  ("string_length", Pstringlength),
  ("succ", Psmlsuccint),
  ("tag_of", Ptag_of),
  ("update", Pupdate),
  ("vect_length", Pvectlength),
  ("xor", Pxorint),
  ("~int", Psmlnegint),
  ("~real", Pfloatprim Psmlnegfloat)
];

in

fun findPrimitive arity name =
  if arity = 0 then
    MLPgv { qual="General", id=[name] }
  else
    (MLPprim(arity, Fnlib.binlookup name primitive_names)
     handle Subscript =>
       MLPccall(arity, name))
;

end;
