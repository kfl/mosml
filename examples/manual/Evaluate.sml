structure Evaluate :> Evaluate = struct
   local open Expr 
   in 
       fun eval (Cst n)         = n
         | eval (Neg e)         = ~ (eval e)
         | eval (Plus (e1, e2)) = eval e1 + eval e2;
       fun test e = (eval e = eval (Reduce.reduce e))
   end
end
