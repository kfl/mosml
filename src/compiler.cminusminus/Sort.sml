(* Merging and sorting *)

fun merge order =
  let fun loop [] ys = ys
        | loop xs [] = xs
        | loop (xs as x::xr) (ys as y::yr) =
            if order x y then 
              x :: loop xr ys 
            else 
              y :: loop xs yr
  in loop end;

fun sort order l =
  let fun initList [] = []
        | initList [e] = [[e]]
        | initList (x1::x2::xs) =
            (if order x1 x2 then [x1, x2] else [x2, x1]) :: initList xs
      fun merge2 (xs1::xs2::xss) = merge order xs1 xs2 :: merge2 xss
        | merge2 x = x
      fun mergeAll [] = []
        | mergeAll [xs] = xs
        | mergeAll xss = mergeAll (merge2 xss)
  in mergeAll(initList l) end
;
