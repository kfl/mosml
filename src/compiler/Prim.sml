(* The type of primitives *)

local
  open Const;
in

datatype primitive =
    Pidentity
  | Pget_global of QualifiedIdent * int
  | Pset_global of QualifiedIdent * int
  | Pdummy of int
  | Pupdate
  | Ptest of bool_test
  | Pmakeblock of BlockTag
  | Ptag_of
  | Pfield of int
  | Psetfield of int
  | Pccall of string * int
  | Praise
  | Pnot
  (* The next five are unsigned operations: *)
  | Paddint | Psubint | Pmulint | Pdivint | Pmodint
  | Pandint | Porint | Pxorint
  | Pshiftleftint | Pshiftrightintsigned | Pshiftrightintunsigned
  | Pintoffloat
  | Pfloatprim of float_primitive
  | Pstringlength | Pgetstringchar | Psetstringchar
  | Pmakevector | Pvectlength | Pgetvectitem | Psetvectitem
  | Psmlnegint | Psmlsuccint | Psmlpredint
  | Psmladdint | Psmlsubint | Psmlmulint | Psmldivint | Psmlmodint
  | Pmakerefvector
  | Patom of int
  | Psmlquotint | Psmlremint
  | Pclosure of int * int
  | Pswap

and float_primitive =
    Pfloatofint
  | Psmlnegfloat | Psmladdfloat | Psmlsubfloat | Psmlmulfloat | Psmldivfloat

and bool_test =
    Peq_test
  | Pnoteq_test
  | Pint_test of int prim_test
  | Pfloat_test of real prim_test
  | Pstring_test of string prim_test
  | Pword_test of word prim_test
  | Pnoteqtag_test of BlockTag

and 'a prim_test =
    PTeq
  | PTnoteq
  | PTnoteqimm of 'a
  | PTlt
  | PTle
  | PTgt
  | PTge
;

end;
