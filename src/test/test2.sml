fun fact 0 = 1
  | fact n = n * fact(n-1);

fact 4;

fun append2 ([], ys) = ys
  | append2 (x::xs, ys) = x :: append2 (xs,ys);

append2( [1,2,3], [4,5,6] );

fun append [] ys = ys
  | append (x :: xs) ys = x :: append xs ys;

append [1,2,3] [4,5,6];

fun reverse xs =
  let fun loop [] ys = ys
        | loop (x::xs) ys = loop xs (x::ys)
  in loop xs [] end;

reverse [1,2,3,4];
reverse [true,false];

val op @ = append2;
infixr 5 @;
[1,2,3] @ [4,5,6];
