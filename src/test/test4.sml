fun map f [] = []
  | map f (x :: xs) = f x :: map f xs;

map (fn n => n+1) [1,2,3];

infix 5 ++;

fun [] ++ ys = ys
  | (x :: xs) ++ ys = x :: (xs ++ ys);

[1,2,3] ++ [4,5,6];

fun reverse xs =
  let
    infix -->
    fun [] --> ys = ys
      | (x::xs) --> ys = xs --> (x::ys)
  in xs --> [] end;

reverse [1,2,3,4];
reverse [true,false];

infix 3 o;
fun (f o g) x = f(g x);

let 
    val I = reverse o reverse
in
    I [1,2,3]
end;

val zl = [(1,true),(2,false),(3,true)];

fun fst (x,_) = x;
fun snd (_,y) = y;

map fst zl;

fun split [] = ([],[])
  | split ((x,y) :: rest) =
      let val (xs,ys) = split rest in
        (x :: xs, y :: ys)
      end;

split zl;

fun member a [] = false
  | member a (x::xs) =
      case a=x
        of true => true
         | false => member a xs;
member 3 [1,2,3,4];
member 3 [1,2];
