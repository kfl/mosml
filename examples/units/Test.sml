structure T = Eval(Reduce)
open Expr 
val t1 = T.test (Plus (Cst 244, Cst 0))
val t2 = T.test (Neg (Plus (Neg (Cst 140), Cst 0)))

