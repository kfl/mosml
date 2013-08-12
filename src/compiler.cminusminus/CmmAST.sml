(* Abstract syntax for C--, nicked from Fermin Reig and heavily modified.    *)
(* Ken Friis Larsen <ken@friislarsen.net>                                             *)
structure CmmAST =
struct

datatype toplevel = Imports  of typ * (name option * name) list
                  | Exports  of typ option * (name * name option) list
                  | Typedef  of name * name
                  | Data     of sectionname * pseudoop list
                  | Target   of memsize option * byteorder 
                  | Function of { conv               : conv
                                , name               : name
                                , formals            : formal list
                                , stmts              : stmts
                                }

and byteorder = Little | Big

and typ = Bits8 | Bits16 | Bits32 | Bits64 

and stmt = DeclReg       of formal
         | DeclStackData of StackDatum list
         | Assign        of lvalue * expr
         | If            of expr * stmts * stmts option
         | Switch        of expr * range option * swt list
         | Label         of label
         | Goto          of label
         | ComputedGoto  of expr * label list 
         | Jump          of conv * expr * actual list * target list
         | Call          of reg list * conv * expr * actual list * 
                            target list * flow list
         | Return        of conv * actual list
         | Continuation  of name * reg list
         | Cut           of expr * actual list * flow list
         | Comment       of string
         | Block         of stmt list   (* For easier assembly *)
         | SafePoint
         | MarkStmt      of stmt * srcpos       

and lvalue = Var of name 
           | Mem of typ * expr * align option

and expr = LitInt       of literalInt
         | LitFloat     of literalFloat
         | LitChar      of literalChar
         | Fetch        of lvalue
         | Prim         of name * actual list
         | EComment     of expr * string 


and flow = Aborts
         | Cuts    of name list
         | Unwinds of name list
         | Returns of name list

and pseudoop  = DataLabel     of label
              | DataExports   of name list
(*              | DataWord      of typesize * int * const_expr list
                                            (* repeat, constant exprs *)
              | DataFloat     of typesize * int * literalFloat list
 *)                                           (* repeat *)
              | DataString    of string
              | DataAlign     of align  (* # of Bytes *)
              | DataComm      of name * const_expr * align option 
                                          (* size, align *)  
              | DataLcomm     of name * const_expr * align option 
                                          (* size, align *)

and StackDatum = StackLabel    of label
(*               | StackSpace    of typesize * int (* size, repeat *)
*)               | StackAlign    of align

and swt = Swt           of (int list) * stmts
        | SwtDefault    of stmts

(* Compile-time expressions. Addr, but never a Reg, StackL, etc *)
and const_expr = ConstExpr of expr  

(* Calling Convention *)
and conv = Cmm | Ccall                   

withtype
    program      = toplevel list
and filename     = string
and memsize      = int
and sectionname  = string
and name         = string
and label        = string
and target       = string
and reg          = string
and align        = int (* in bytes; a power of two *)
and range        = int * int 
and actual       = expr
and formal       = typ * string (*reg*)
and literalInt   = string
and literalFloat = string
and literalChar  = string
and srcpos       = int * int
and stmts        = stmt list
and exprs        = expr list
                  
(* An empty statement *)
val NOP = Block []
                  
end



