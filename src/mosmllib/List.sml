(* List -- as of 1995-03-08, 1996-04-19 *)

datatype list = datatype list

exception Empty;

fun null [] = true
  | null _  = false;

fun hd []      = raise Empty
  | hd (x::xr) = x;

fun tl []      = raise Empty
  | tl (x::xr) = xr;

fun last []      = raise Empty
  | last [x]     = x
  | last (x::xr) = last xr;

fun nth (xs, n) =
    let fun h []      _ = raise Subscript
	  | h (x::xr) n = if n=0 then x else h xr (n-1)
    in if n<0 then raise Subscript else h xs n end;

fun drop (xs, n) =
    let fun h xs      0 = xs
	  | h []      n = raise Subscript
	  | h (x::xr) n = h xr (n-1)
    in if n<0 then raise Subscript else h xs n end;

fun take (xs, n) =
    let fun h xs      0 = []
	  | h []      n = raise Subscript
	  | h (x::xr) n = x :: h xr (n-1)
    in if n<0 then raise Subscript else h xs n end;

fun length xs =
    let fun acc []      k = k
          | acc (x::xr) k = acc xr (k+1)
    in acc xs 0 end;

local
  fun revAcc [] ys = ys
    | revAcc (x::xs) ys = revAcc xs (x::ys)
in 
  fun rev xs = revAcc xs []

  fun revAppend (xs, ys) = revAcc xs ys
end

local
  fun append [] ys = ys
    | append (x::xs) ys = x :: append xs ys
in
  fun xs @ [] = xs
    | xs @ ys = append xs ys
end

fun concat []        = []
  | concat (xs::xsr) = xs @ concat xsr;

fun app f []      = ()
  | app f (x::xr) = (f x; app f xr);

fun map f [] = []
  | map f (x::xs) = f x :: map f xs

fun mapPartial f []      = []
  | mapPartial f (x::xr) = case f x of NONE   => mapPartial f xr
                                     | SOME r => r :: mapPartial f xr;

fun find p []      = NONE
  | find p (x::xr) = if p x then SOME x else find p xr;

fun filter p []      = []
  | filter p (x::xr) = if p x then x :: filter p xr else filter p xr;

fun partition p xs =
    let fun h []      are aren't = (rev are, rev aren't)
	  | h (x::xr) are aren't = if p x then h xr (x::are) aren't
				          else h xr are      (x::aren't)
    in h xs [] [] end;

fun foldr f e []      = e
  | foldr f e (x::xr) = f(x, foldr f e xr);

fun foldl f e []      = e
  | foldl f e (x::xr) = foldl f (f(x, e)) xr;

fun exists p []      = false
  | exists p (x::xr) = p x orelse exists p xr;

fun all p []      = true
  | all p (x::xr) = p x andalso all p xr;

fun tabulate (n, f) =
    let fun h i = if i<n then f i :: h (i+1) else []
    in if n<0 then raise Size else h 0 end;

fun getItem []        = NONE
  | getItem (x :: xr) = SOME (x, xr)
