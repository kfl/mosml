(* ovlfail.sml 
   Test of failing  overloading resolution as per the New Definition.
 *  PS 1996-04-12
 *)

type word8 = Word8.word;

fun fail1 (x : 'a) = x + x;
fun fail2 (x as (a,b,c)) = x + x;
fun fail3 (x : bool -> bool) = x + x;
fun fail4 (x : bool) = x + x;
fun fail5 (x : int Array.array) = x + x;
fun fail6 x = abs x before ignore (x + 0w0);
fun fail7 x = ~ x before ignore (x + 0w0);
fun fail8 x y = x div y before ignore (y + 0.0);
fun fail9 x y = x mod y before ignore (y + 0.0); 
fun fail10  x y = x + y before ignore (y ^ "");
fun fail11  x y = x - y before ignore (y ^ "");
fun fail12 x y = x * y before ignore (y ^ "");
fun fail13 x y = x div y before ignore (y ^ "");
fun fail14 x y = x mod y before ignore (y ^ ""); 
fun fail15 x = abs x before ignore (x ^ "");
fun fail16 x = ~ x before ignore (x ^ "");
fun fail17  x y = x + y before ignore (ord y);
fun fail18  x y = x - y before ignore (ord y);
fun fail19 x y = x * y before ignore (ord y);
fun fail20 x y = x div y before ignore (ord y);
fun fail21 x y = x mod y before ignore (ord y); 
fun fail22 x = abs x before ignore (ord x);
fun fail23 x = ~ x before ignore (ord x);
fun fail24 x = abs x before ignore (x + 0w0 : word8);
fun fail25 x = ~ x before ignore (x + 0w0 : word8);
val fail26 = 0w1 : int;
val fail27 = 0w1 : real;
val fail28 = 0w1 : string;
val fail29 = 0w1 : char;
val fail30 = 0w1 : bool;
val fail31 = 0w1 : int Array.array;
fun fail32 (x as 0w256) = [x, x, x, 0w0 : word8]
  | fail32 _ = [];

