structure Reduce :> Reduce = struct 
   local open Expr
   in
       fun negate (Neg e) = e
         | negate e       = Neg e
       fun reduce (Neg (Neg e))      = e
         | reduce (Neg e)            = negate (reduce e)
         | reduce (Plus (Cst 0, e2)) = reduce e2
         | reduce (Plus (e1, Cst 0)) = reduce e1
         | reduce (Plus (e1, e2))    = Plus (reduce e1, reduce e2)
         | reduce e                  = e
   end
end
