(* val it = fn : 'a -> (('a -> 'b) -> 'b) *)

fn x => let val g = (fn y => y x) in g end;

(* Type clash between (int -> 'a) and int *)

let
  val f = fn x => let val y = x in y 5 end
in
  f 3
end;

(* val g = fn : 'a list -> int *)

fun g [] = 1
  | g (x::xs) = 1 + g(xs);

(* val h = fn : int list -> int *)

fun h [] = 1
  | h (x::xs) = h(xs) + h [7,9];

(* Type clash between bool and int *)

fun k [] = 1
  | k (x::xs) = 1 + k[7,9] + k[true];

(* Wrong binding level for explicit type variable 'aaa *)

(fn x => let val y : 'aaa = x in y y end) 5;

(* Explicit type variable 'z can't be unified with ('z -> 'z) *)

val x = (let val Id : 'z -> 'z = fn z => z in Id Id end,
         fn z => z : 'z);

(* Type clash between bool and string *)

let
  val f = fn {lab1=x, ...} => x
in
  (f {lab1=88, lab2="a"}, f {lab1=99, lab2=true})
end;

(* Type clash between 'a and ('a -> 'b) *)

fun mcurry f k =
  let fun mcurryAcc 0 xs = f(rev xs)
        | mcurryAcc k xs = fn x => mcurryAcc (k-1) (x::xs)
  in mcurryAcc k [] end;
