(* SML primitives *)

local
  open Obj Const Prim;
in

datatype SMLPrim =
    MLPeq
  | MLPeq_c
  | MLPnoteq
  | MLPnoteq_c
  | MLPref
  | MLPsetref
  | MLPsetref_c
  | MLPadd_int
  | MLPadd_int_c
  | MLPsub_int
  | MLPsub_int_c
  | MLPmul_int
  | MLPmul_int_c
  | MLPdiv_int
  | MLPdiv_int_c
  | MLPmod_int
  | MLPmod_int_c
  | MLPquot_int
  | MLPquot_int_c
  | MLPrem_int
  | MLPrem_int_c
  | MLPeq_int
  | MLPeq_int_c
  | MLPnoteq_int
  | MLPnoteq_int_c
  | MLPlt_int
  | MLPlt_int_c
  | MLPgt_int
  | MLPgt_int_c
  | MLPle_int
  | MLPle_int_c
  | MLPge_int
  | MLPge_int_c
  | MLPadd_real
  | MLPadd_real_c
  | MLPsub_real
  | MLPsub_real_c
  | MLPmul_real
  | MLPmul_real_c
  | MLPdiv_real
  | MLPdiv_real_c
  | MLPlt_real
  | MLPlt_real_c
  | MLPgt_real
  | MLPgt_real_c
  | MLPle_real
  | MLPle_real_c
  | MLPge_real
  | MLPge_real_c
  | MLPlt_string
  | MLPlt_string_c
  | MLPgt_string
  | MLPgt_string_c
  | MLPle_string
  | MLPle_string_c
  | MLPge_string
  | MLPge_string_c
  | MLPadd_word
  | MLPadd_word_c
  | MLPsub_word
  | MLPsub_word_c
  | MLPmul_word
  | MLPmul_word_c
  | MLPdiv_word
  | MLPdiv_word_c
  | MLPmod_word
  | MLPmod_word_c
  | MLPeq_word
  | MLPeq_word_c
  | MLPnoteq_word
  | MLPnoteq_word_c
  | MLPlt_word
  | MLPlt_word_c
  | MLPgt_word
  | MLPgt_word_c
  | MLPle_word
  | MLPle_word_c
  | MLPge_word
  | MLPge_word_c
  | MLPconcat
  | MLPconcat_c
  | MLPprim of int * primitive
  | MLPccall of int * string
  | MLPgv of QualifiedIdent
  | MLPgvt of QualifiedIdent * obj ref
;

end;
