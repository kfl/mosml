(* The intermediate language: extended lambda-calculus in de
    Bruijn's notation *)

local
  open Const Prim;
in

datatype Lambda =
    Lvar of int
  | Lconst of StructConstant
  | Lapply of Lambda * Lambda list
  | Lfn of Lambda
  | Llet of Lambda list * Lambda
  | Lletrec of Lambda list * Lambda
  | Lprim of primitive * Lambda list
  | Lcase of Lambda * (SCon * Lambda) list
  | Lswitch of int * Lambda * (BlockTag * Lambda) list
  | Lstaticfail
  | Lstatichandle of Lambda * Lambda
  | Lhandle of Lambda * Lambda
  | Lif of Lambda * Lambda * Lambda
  | Lseq of Lambda * Lambda
  | Lwhile of Lambda * Lambda
  | Landalso of Lambda * Lambda
  | Lorelse of Lambda * Lambda
  | Lunspec
  | Lshared of Lambda ref * int ref
  | Lassign of int * Lambda
;

fun shared_lambda lam =
  Lshared( ref lam, ref Instruct.Nolabel )
;


fun Lstruct [] = Lconst(BLOCKsc(CONtag(0,1),[]))
|   Lstruct lams = Lprim(Pmakeblock(CONtag(0,1)), lams)

end;
