(* The type of the instructions of the abstract machine *)

(* 1996.07.05 -- e *)

open Config Const Prim;

datatype ZamInstruction =
    Kquote of StructConstant
  | Kget_global of QualifiedIdent * int
  | Kset_global of QualifiedIdent * int
  | Kaccess of int
  | Kenvacc of int                  (* new *)
  | Kassign of int                  (* newer *)
  | Kgetfield of int                (* new *)
  | Ksetfield of int                (* new *)
  | Kpush
  | Kpop of int                     (* added arg *)
  | Krestart                        (* new *)
  | Kgrab of int                    (* added arg *)
  | Kapply of int                   (* added arg *)
  | Kappterm of int * int           (* added args and renamed *)
  | Kpush_retaddr of int            (* new *)
  | Kcheck_signals
  | Kreturn of int                  (* added arg *)
  | Kclosure of int * int           (* added arg *)
  | Kclosurerec of int * int        (* new *)
  | Kraise                          (* new *)
  | Kmakeblock of BlockTag * int
  | Kprim of primitive
  | Kpushtrap of int
  | Kpoptrap
  | Klabel of int
  | Kbranch of int
  | Kbranchif of int
  | Kbranchifnot of int
  | Kstrictbranchif of int
  | Kstrictbranchifnot of int
  | Ktest of bool_test * int
  | Kbranchinterval of int * int * int * int
  | Kswitch of int Array.array
;

type ZamPhrase =
{
  kph_funcs:   ZamInstruction list,     (* code for functions *)
  kph_inits:   ZamInstruction list,     (* initialization code *)
  kph_is_pure: bool                     (* pure = no side effects *)
};

val Nolabel = ~1;
