fun even 0 = true
  | even x = odd(x-1)
and odd  0 = false
  | odd  x = even(x-1);

even 10; even 11;

datatype 'a X = X of string;

val stripX = fn X u => u;
fn _ => stripX (X "OK");
it () : string;

val stripX666 = fn X "666" => X "000" | x => x;

fn _ => stripX666 (X "666");
it () : int X;

fn _ => stripX666 (X "OK");
it () : int X;

datatype XXX =
    A of int * int
  | B of int * int;

val a12 = A(1,2)
and b12 = B(1,2);

fun strip (A x) = x
  | strip (B x) = x;

a12 = b12;
strip a12 = strip b12;
