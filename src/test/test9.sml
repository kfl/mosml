type person = {name: string, age: int};
val p1 : person = {name = "Peter", age = 99};
val {name=name1, age=age1} = p1;
fun nameOfPerson {name=x, age=_} = x;
val name1 = nameOfPerson p1;
val {name,age} = p1;

val {name,...}:person = p1;
val {name : string as name1,...} = p1;

val f = fn {name=x,...}:person => x;

val name1 = #name p1;
val age1 = #age p1;

fun f {lab=v} = v+1;

fn _ =>					(* to cheat value polymorphism *) 
let
  fun eq r1 r2 = (#l1 r1) = (#l1 r2)
  val t = eq {l1 = 1, l2 = 3}
             {l1 = 1, l3 = "a"}
in
  (eq, t)
end;

(printVal (it ()); ());

fn _ =>
let
  val f = fn {l1 = x,...} => x 
  val g = fn {l1 = x, l2 = y} => (x,y)
  val h = fn x => (x=x, f x, g x)
in
  (f, g, h)
end;

(printVal (it ()); ());
