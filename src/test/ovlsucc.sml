(* ovlsucc.sml 
   Test of successful overloading resolution as per the New Definition.
 *  PS 1996-04-12, 1997-07-13
 *)

type word8 = Word8.word;

fun iii1 x y = x + y;
fun iii2 x y = x - y;
fun iii3 x y = x * y;
fun iii4 x y = x div y;
fun iii5 x y = x mod y; 
fun iii6 x y = x < y;
fun iib7 x y = x > y;
fun iib8 x y = x <= y;
fun iib9 x y = x >= y;
fun ii10 x = abs x;
fun ii11 x = ~ x;

fun www1 x y = x + y before ignore (y + 0w0);
fun www2 x y = x - y before ignore (y + 0w0);
fun www3 x y = x * y before ignore (y + 0w0);
fun www4 x y = x div y before ignore (y + 0w0);
fun www5 x y = x mod y before ignore (y + 0w0); 
fun www6 x y = x < y before ignore (y + 0w0);
fun wwb7 x y = x > y before ignore (y + 0w0);
fun wwb8 x y = x <= y before ignore (y + 0w0);
fun wwb9 x y = x >= y before ignore (y + 0w0);
 
fun www1 x y = x + y before ignore (y + 0w0 : word8);
fun www2 x y = x - y before ignore (y + 0w0 : word8);
fun www3 x y = x * y before ignore (y + 0w0 : word8);
fun www4 x y = x div y before ignore (y + 0w0 : word8);
fun www5 x y = x mod y before ignore (y + 0w0 : word8); 
fun www6 x y = x < y before ignore (y + 0w0 : word8);
fun wwb7 x y = x > y before ignore (y + 0w0 : word8);
fun wwb8 x y = x <= y before ignore (y + 0w0 : word8);
fun wwb9 x y = x >= y before ignore (y + 0w0 : word8);

fun rrr1 x y = x + y before ignore (y + 0.0);
fun rrr2 x y = x - y before ignore (y + 0.0);
fun rrr3 x y = x * y before ignore (y + 0.0);
fun rrb4 x y = x < y before ignore (y + 0.0);
fun rrb5 x y = x > y before ignore (y + 0.0);
fun rrb6 x y = x <= y before ignore (y + 0.0);
fun rrb7 x y = x >= y before ignore (y + 0.0);
fun rr8 x = abs x before ignore (x + 0.0);
fun rr9 x = ~ x before ignore (x + 0.0);

fun ssb1 x y = x < y before ignore (y ^ "");
fun ssb2 x y = x > y before ignore (y ^ "");
fun ssb3 x y = x <= y before ignore (y ^ "");
fun ssb4 x y = x >= y before ignore (y ^ "");

fun ccb1 x y = x < y before ignore (ord y);
fun ccb2 x y = x > y before ignore (ord y);
fun ccb3 x y = x <= y before ignore (ord y);
fun ccb4 x y = x >= y before ignore (ord y);

fun succ1 (x as 0w255) = [x, x, x, 0w0 : word8]
  | succ1 _ = [];
fun succ2 (x as 0w256) = [x, x, x, 0w0]
  | succ2 _ = [];

fun less (x, y) = x < y
fun minii(x, y) = if less(x, y) then x else y;

fun is12 x = makestring x;
fun ws10 x = makestring x before ignore (x + 0w0); 
fun ws10 x = makestring x before ignore (x + 0w0 : word8); 
fun rs10 x = makestring x before ignore (x + 0.0);
fun ss5 x = makestring x before ignore (x ^ "");
fun cs5 x = makestring x before ignore (ord x);

(* Testing optimized equality (int, char, word) *)

val eei1 = 11700 = 11700 andalso 11171 <> 11172;
val eew1 = 0wx12345 = 0wx12345 andalso 0wx12345 <> 0wx12354;
val eec1 = #"A" = #"A" andalso #"C" <> #"D";
val eeg1 = 
    "abc" = "abc" andalso "abc" <> "ABC" 
    andalso 117.0 = 117.0 andalso 117.1 <> 117.0
    andalso [1] = [1] andalso [0] <> [1]
    andalso [0w117] = [0w117] andalso [0w118] <> [0w117];

let fun loop1 n = if n=0 then 7 else loop1 (n-1) 
in Mosml.time loop1 5000000 end;

fun loop2 n = if n=0 then 7 else loop2 (n-1);
Mosml.time loop2 5000000;

(* Moscow ML 1.42 did not properly resolve the overloaded type variable 
   to int in this case, and hence complained about a free type variable:  *)

local
   fun f(i,a,(ind,max)) = if a > max then (i,a) else (ind,max) 
in
   fun maxi ar = Array.foldli f (0,Array.sub(ar,0)) (ar,0,NONE)
end;
