(* Opcodes for the simple primitives. *)

local open Fnlib Prim Opcodes in

val opcode_for_primitive = fn
    Pupdate => UPDATE
  | Praise => RAISE
  | Pnot => BOOLNOT
  | Ptag_of => TAGOF
  | Paddint => ADDINT
  | Psubint => SUBINT
  | Pmulint => MULINT
  | Pdivint => DIVINT
  | Pmodint => MODINT
  | Pandint => ANDINT
  | Porint => ORINT
  | Pxorint => XORINT
  | Pshiftleftint => SHIFTLEFTINT
  | Pshiftrightintsigned => SHIFTRIGHTINTSIGNED
  | Pshiftrightintunsigned => SHIFTRIGHTINTUNSIGNED
  | Pintoffloat => INTOFFLOAT
  | Pstringlength => STRINGLENGTH
  | Pgetstringchar => GETSTRINGCHAR
  | Psetstringchar => SETSTRINGCHAR
  | Pmakevector => MAKEVECTOR
  | Pvectlength => VECTLENGTH
  | Pgetvectitem => GETVECTITEM
  | Psetvectitem => SETVECTITEM
  | Psmlnegint => SMLNEGINT
  | Psmlsuccint => SMLSUCCINT
  | Psmlpredint => SMLPREDINT
  | Psmladdint => SMLADDINT
  | Psmlsubint => SMLSUBINT
  | Psmlmulint => SMLMULINT
  | Psmldivint => SMLDIVINT
  | Psmlmodint => SMLMODINT
  | Pmakerefvector => MAKEREFVECTOR
  | Psmlquotint => SMLQUOTINT
  | Psmlremint   => SMLREMINT
  | Pswap => SWAP
  | _ => fatalError "opcode_for_primitive"
;

val opcode_for_float_primitive = fn
    Pfloatofint => FLOATOFINT
  | Psmlnegfloat => SMLNEGFLOAT
  | Psmladdfloat => SMLADDFLOAT
  | Psmlsubfloat => SMLSUBFLOAT
  | Psmlmulfloat => SMLMULFLOAT
  | Psmldivfloat => SMLDIVFLOAT
;

end;
