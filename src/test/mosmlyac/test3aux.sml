datatype lam =
    Bas of int
  | Var of lam
  | Abs of lam -> lam
  | App of lam * lam

val id = Abs(fn x => Var x)
val t = Abs(fn x => App(Var x, Var x))

val a = App(t, id) 

fun red (App(e1, e2)) = 
    (case red e1 of
	 Abs f => red (f e2)
       | e1'   => App(e1', e2))
  | red (Var e) = red e
  | red e = e
