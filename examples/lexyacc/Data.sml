(* Data.sml  -- the abstract syntax datatype *)

datatype baseval = 
    Int of int

datatype sourceexpr = 
    AppS of sourceexpr * sourceexpr
  | VarS of string
  | LamS of string * sourceexpr
  | LetS of (string * sourceexpr) list * sourceexpr
  | LetrecS of (string * sourceexpr) list * sourceexpr
  | ConS of int * sourceexpr list
  | CaseS of sourceexpr * alt list
  | CstS of baseval                             
  | Op2S of (baseval * baseval -> baseval) * sourceexpr * sourceexpr
  | IfS  of sourceexpr * sourceexpr * sourceexpr
withtype alt = int * string list * sourceexpr
