(* Listsort *)

(* Smooth Applicative Merge Sort, Richard O'Keefe 1982        *)
(* From L.C. Paulson: ML for the Working Programmer, CUP 1991 *)
(* Optimized for Moscow ML **)

fun sort ordr []          = []
  | sort ordr (xs as [_]) = xs
  | sort ordr (xs as [x1, x2]) =
    (case ordr(x1, x2) of
	 GREATER => [x2, x1]
       | _       => xs)
  | sort ordr xs =
    let fun merge []       ys = ys
	  | merge (x1::xr) ys = 
	    let fun take x1 xr []       = x1 :: xr
		  | take x1 xr (y1::yr) = 
	            (case ordr(x1, y1) of 
			 LESS    => x1 :: take y1 yr xr
		       | _       => y1 :: take x1 xr yr)
	    in take x1 xr ys end
        fun mergepairs l1  []              k = [l1]
          | mergepairs l1 (ls as (l2::lr)) k =
            if k mod 2 = 1 then l1::ls
            else mergepairs (merge l1 l2) lr (k div 2)
	fun nextrun run []      = (run, [])
	  | nextrun run (xs as (x::xr)) =
	    if ordr(x, List.hd run) = LESS then (run, xs)
	    else nextrun (x::run) xr
        fun sorting []      ls r = List.hd(mergepairs [] ls 0)
          | sorting (x::xs) ls r =
	    let val (revrun, tail) = nextrun [x] xs
	    in sorting tail (mergepairs (List.rev revrun) ls (r+1)) (r+1) end
    in sorting xs [] 0 end;

(* Check sortedness *)

fun sorted ordr []         = true
  | sorted ordr (y1 :: yr) = 
    let fun h x0 []       = true
	  | h x0 (x1::xr) = ordr(x0, x1) <> GREATER andalso h x1 xr
    in h y1 yr end;

(* Merge without duplicates *)

fun mergeUniq ordr ([],     ys) = ys
  | mergeUniq ordr (x1::xr, ys) = 
    let fun take x1 xr []       = x1 :: xr
	  | take x1 xr (y1::yr) = 
	    (case ordr(x1, y1) of 
		 LESS    => x1 :: take y1 yr xr
	       | GREATER => y1 :: take x1 xr yr
	       | EQUAL   => take x1 xr yr)
    in take x1 xr ys end

(* Merge with duplicates *)

fun merge ordr ([],     ys) = ys
  | merge ordr (x1::xr, ys) = 
    let fun take x1 xr []       = x1 :: xr
	  | take x1 xr (y1::yr) = 
	    (case ordr(x1, y1) of 
		 LESS    => x1 :: take y1 yr xr
	       | _       => y1 :: take x1 xr yr)
    in take x1 xr ys end

(* Find the equivalence classes of a sorted list *)

fun eqclasses ordr xs =
    let val xs = List.rev (sort ordr xs)
	fun group last rest cs1 css =
	    case rest of 
		[]     => cs1 :: css
	      | r1::rr => 
		    if ordr(r1, last) = EQUAL then 
			group r1 rr (r1 :: cs1) css
		    else
			group r1 rr [r1] (cs1 :: css)
    in 
	case xs of 
	    []     => []
	  | x1::xr => group x1 xr [x1] []
    end
