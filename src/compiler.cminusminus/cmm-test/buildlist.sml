fun decr x = x - 1




fun add1 x = let fun arg2 y = x + y in arg2 end 
fun add2 (x,y) = x+y

fun buildlist 0 = []
  | buildlist n = n :: buildlist(n-1) 


fun for n =
    let fun looploop f = 
            let fun loop i = if i = n then i
                             else loop (f i)
            in loop 0
            end
    in  looploop
    end 


fun loop 0 = ()
  | loop n = loop(n-1) 

fun repeat f =
    let fun loop x = loop(f x)
                     handle ?? => x
    in loop
    end


fun showint (i:int) = ()
fun showintnl i = (showint i);

fun f n =
  let
    fun loop () = (showint n; loop ())
  in
    case n
      of 0 => 0
       | _ => (loop() handle Interrupt => f (n-1))
  end;

fun silly x = raise x



fun toOrder n =
    case n of
        0 => LESS
      | 1 => EQUAL
      | _  => GREATER 


fun fromOrder order =
    case order of 
        LESS    => 100
      | GREATER => 200
      | _       => 300


datatype foo = A | B | C | D of foo | E of int

fun fromFoo foo =
    case foo of
        B       => 100
      | D (D A) => 200
      | E 42    => 300
      | _       => 400  

fun add5 a b c d e = a+b+c+d+e

fun five x = add5 x x x x x + 1

fun map f [] = []
  | map f (h::t) = f h :: map f t 


fun map1 f = 
    let fun map [] = []
          | map (h::t) = f h :: map t
    in  map
    end 

val zero = 0

val one = 1

fun length [] = zero
  | length (_::xs) = one + length xs

fun rev [] acc       = acc
  | rev (h :: t) acc = rev t (h::acc) 

fun map2 f ls = 
    let fun iter [] acc      = rev acc []
          | iter (x::xs) acc = iter xs (f x :: acc) 
        
    in  iter ls []
    end

fun K x y = x
