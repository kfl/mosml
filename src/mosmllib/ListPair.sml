(* ListPair *)

fun zip (xs, ys) = 
    let fun h (x::xr) (y::yr) res = h xr yr ((x, y) :: res)
	  | h _       _       res = List.rev res
    in h xs ys [] end;

fun unzip xys =
    let fun h ([],          xs, ys) = (List.rev xs, List.rev ys)
	  | h ((x, y)::xyr, xs, ys) = h (xyr, x::xs, y::ys)
    in h (xys, [], []) end;

fun map f (xs, ys) = 
    let fun h (x::xr) (y::yr) res = h xr yr (f(x, y) :: res)
	  | h _       _       res = List.rev res
    in h xs ys [] end;

fun app f (xs, ys) = 
    let fun h (x::xr) (y::yr) = (f (x, y); h xr yr)
	  | h _       _       = ()
    in h xs ys end;

fun all p (xs, ys) = 
    let fun h (x::xr) (y::yr) = p(x, y) andalso h xr yr
	  | h _       _       = true
    in h xs ys end;

fun exists p (xs, ys) = 
    let fun h (x::xr) (y::yr) = p(x, y) orelse h xr yr
	  | h _       _       = false
    in h xs ys end;

fun foldr f e (xs, ys) = 
    let fun h (x::xr) (y::yr) = f(x, y, h xr yr) 
	  | h _       _       = e
    in h xs ys end;

fun foldl f e (xs, ys) = 
    let fun h e (x::xr) (y::yr) = h (f(x, y, e)) xr yr
	  | h e _       _       = e
    in h e xs ys end;

(* The following is not a member of the Basis Library:

fun find p (xs, ys) = 
    let fun h (x::xr) (y::yr) = if p (x, y) then SOME (x, y) else h xr yr
	  | h _       _       = NONE
    in h xs ys end;
*)
