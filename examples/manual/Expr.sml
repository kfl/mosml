structure Expr = struct
   datatype expr = Cst of int | Neg of expr | Plus of expr * expr

   fun show (Cst n)         = makestring n
     | show (Neg e)         = "(-" ^ show e ^ ")"
     | show (Plus (e1, e2)) = "(" ^ show e1 ^ "+" ^ show e2 ^ ")"
end
