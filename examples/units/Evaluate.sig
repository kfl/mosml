functor Eval: functor(R:Reduce) ->
              sig val eval: Expr.expr -> int 
                  val test: Expr.expr -> bool
              end
