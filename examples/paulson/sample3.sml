(**** ML Programs from Chapter 3 of

  ML for the Working Programmer, 2nd edition
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1996)

Copyright (C) 1996 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.
****)


(*** Making change.  Thanks to Dr Martin Richard for this idea! ***)

(*Return first way of making change; NOT EXHAUSTIVE*)
fun change (coinvals, 0)         = []
  | change (c::coinvals, amount) =
      if amount<c then change(coinvals, amount)
                  else c :: change(c::coinvals, amount-c);

val gb_coins = [50,20,10,5,2,1] 
and us_coins = [25,10,5,1];

(*Return all ways of making change (not just the first way)  *)
fun allChange (coins, coinvals, 0)         = [coins]
  | allChange (coins, [],    amount)       = []
  | allChange (coins, c::coinvals, amount) =
      if amount<0 then []
      else allChange(c::coins, c::coinvals, amount-c) @
	   allChange(coins, coinvals, amount);


(*** Binary Arithmetic ***)

fun bincarry (0, ps) = ps
  | bincarry (1, []) = [1]
  | bincarry (1, p::ps) = (1-p) :: bincarry(p, ps);

(*sum of two binary numbers with carry*)
fun binsum (c, [], qs) = bincarry (c,qs)
  | binsum (c, ps, []) = bincarry (c,ps)
  | binsum (c, p::ps, q::qs) =
      ((c+p+q) mod 2) :: binsum((c+p+q) div 2, ps, qs);

(*product of two binary numbers*)
fun binprod ([], _) = []
  | binprod (0::ps, qs) = 0::binprod(ps,qs)
  | binprod (1::ps, qs) = binsum(0, qs, 0::binprod(ps,qs));


(*** Matrix transpose ***)

fun headcol []    = []
  | headcol ((x::_) :: rows) = x :: headcol rows;

fun tailcols []    = []
  | tailcols ((_::xs) :: rows) = xs :: tailcols rows;

(*LOOPS given empty list!*)
fun transp ([]::rows) = []
  | transp rows = headcol rows :: transp (tailcols rows);


(*** Matrix product ***)

fun dotprod([], []) = 0.0
  | dotprod(x::xs,y::ys) = x*y + dotprod(xs,ys);

fun rowprod(row, []) = []
  | rowprod(row, col::cols) =
        dotprod(row,col) :: rowprod(row,cols);

fun rowlistprod([], cols) = []
  | rowlistprod(row::rows, cols) =
        rowprod(row,cols) :: rowlistprod(rows,cols);

fun matprod(rowsA,rowsB) = rowlistprod(rowsA, transp rowsB);


(*** Gaussian Elimination (from Sedgewick, Chapter 5) ***)

(*the first row with absolutely greatest head*)
fun pivotrow [row] = row : real list
  | pivotrow (row1::row2::rows) =
      if abs(hd row1) >= abs(hd row2)
      then pivotrow(row1::rows)
      else pivotrow(row2::rows);

(*the matrix excluding the first row with head p*)
fun delrow (p, []) = []
  | delrow (p, row::rows) =
      if Real.==(p, hd row) then rows
      else row :: delrow(p, rows);

fun scalarprod(k, []) = [] : real list
  | scalarprod(k, x::xs) = k*x :: scalarprod(k,xs);

fun vectorsum ([], []) = [] : real list
  | vectorsum (x::xs,y::ys) = x+y :: vectorsum(xs,ys);

fun gausselim [row] = [row]
  | gausselim rows =
      let val p::prow = pivotrow rows
          fun elimcol [] = []
            | elimcol ((x::xs)::rows) =
                  vectorsum(xs, scalarprod(~x/p, prow))
                  :: elimcol rows
      in  (p::prow) :: gausselim(elimcol(delrow(p,rows)))
      end;

fun solutions [] = [~1.0]
  | solutions((x::xs)::rows) =
      let val solns = solutions rows
      in ~(dotprod(solns,xs)/x) :: solns  end;


(*** Dijkstra's problems ***)

(** Writing a number as the sum of two squares.
    A number is the sum of two squares iff
    its square-free odd prime factors all are congruent to 1 (mod 4) --
    thus 2, 5, 13, 17, 29, 37, 41,  53,  61, 73, 89, 97, 101, 109, 113...
    See H. Davenport (1952), chapter V.**)

fun squares r =
  let fun between (x,y) =  (*all pairs between x and y*)
        let val diff = r - x*x
            fun above y =  (*all pairs above y*)
                if y>x then []
                else if y*y<diff then above (y+1)
                else if y*y=diff then (x,y)::between(x-1,y+1)
                else (* y*y>diff *)  between(x-1,y)
        in above y  end;
      val firstx = floor(Math.sqrt(real r))
  in between (firstx, 0) end;

(** the next permutation **)

fun next(xlist, y::ys) : int list =
    if hd xlist <= y then  next(y::xlist, ys)  (*still increasing*)
    else  (*swap y with greatest xk such that x>=xk>y *)
      let fun swap [x] = y::x::ys
            | swap (x::xk::xs) =          (*x >= xk *)
                if xk>y then x::swap(xk::xs)
                else (y::xk::xs)@(x::ys)
                         (* x > y >= xk >= xs *)
      in swap(xlist) end;

fun nextperm (y::ys) = next([y], ys);


(*** Finite sets ***)

(*membership in a list*)
infix mem;
fun x mem []  =  false
  | x mem (y::l)  =  (x=y) orelse (x mem l);

(*insertion into list if not already there*)
fun newmem(x,xs) = if x mem xs then  xs   else  x::xs;

(*insert the list of xs into the ys, adding no duplicates*)
fun union([],ys) = ys
  | union(x::xs, ys) = newmem(x, union(xs, ys));

fun inter([],ys) = []
  | inter(x::xs, ys) = 
        if x mem ys then x::inter(xs, ys)
                    else    inter(xs, ys);

fun powset ([], base) = [base]
  | powset (x::xs, base) = powset(xs, base) @ powset(xs, x::base);

fun cartprod ([], ys) = []
  | cartprod (x::xs, ys) =
        let val rest = cartprod(xs,ys)
            fun pairx [] = rest
              | pairx(y::ytail) = (x,y) :: (pairx ytail)
        in  pairx ys  end;


(*** Graph algorithms ***)

fun nexts (a, []) = []
  | nexts (a, (x,y)::pairs) =
      if a=x then  y :: nexts(a,pairs)
             else       nexts(a,pairs);

fun depthf ([], graph, visited) = rev visited
  | depthf (x::xs, graph, visited) =
      if x mem visited then depthf (xs, graph, visited)
      else depthf (nexts(x,graph) @ xs, graph, x::visited);

(*Alternative, faster function for depth-first search*)
fun depth ([], graph, visited) = rev visited
  | depth (x::xs, graph, visited) =
      depth (xs, graph, 
             if x mem visited  then  visited
             else depth (nexts(x,graph), graph, x::visited));

fun topsort graph =
  let fun sort ([], visited) = visited
        | sort (x::xs, visited) =
            sort(xs, if x mem visited  then  visited
                     else x :: sort(nexts(x,graph), visited));
      val (xs,_) = ListPair.unzip graph
  in sort(xs, []) end;

fun pathsort graph =
  let fun sort ([], path, visited) = visited
        | sort (x::xs, path, visited) =
            if x mem path then hd[] (*abort!!*)
            else sort(xs, path,
                      if x mem visited  then  visited
                      else x :: sort(nexts(x,graph), x::path, visited))
      val (xs,_) = ListPair.unzip graph
  in sort(xs, [], []) end;


fun newvisit (x, (visited,cys)) = (x::visited, cys);

fun cyclesort graph =
  let fun sort ([], path, (visited,cys)) = (visited, cys)
        | sort (x::xs, path, (visited,cys)) =
            sort(xs, path, 
               if x mem path   then  (visited, x::cys)
               else if x mem visited then (visited, cys)
               else newvisit(x, sort(nexts(x,graph),
                                     x::path, (visited,cys))))
      val (xs,_) = ListPair.unzip graph
  in sort(xs, [], ([],[])) end;


(*** Sorting ***)

(** Random numbers, courtesy Stephen K. Park and Keith W. Miller, 
	CACM 31 (1988), 1192-1201.  **)
local val a = 16807.0  and  m = 2147483647.0
in  fun nextrandom seed =
          let val t = a*seed
          in  t - m * real(floor(t/m))  end

    (*truncate to integer from 1 to k*)
    and truncto k r = 1 + floor((r / m) * (real k))
end;

fun randlist (n,seed,seeds) =
    if n=0  then  (seed,seeds)  
    else  randlist(n-1, nextrandom seed, seed::seeds);


(** insertion sort: non-iterative is faster **)
fun ins (x, []): real list = [x]
  | ins (x, y::ys) = 
      if x<=y then x::y::ys    (*it belongs here*)
              else y::ins(x,ys);

fun insort [] = []
  | insort (x::xs) = ins(x, insort xs);


(*quicksort*)
fun quick [] = []
  | quick [x] = [x]
  | quick (a::bs) =  (*the head "a" is the pivot*)
      let fun partition (left,right,[]) : real list = 
                (quick left) @ (a :: quick right)
            | partition (left,right, x::xs) =
                if x<=a then partition (x::left, right, xs)
                        else partition (left, x::right, xs)
      in  partition([],[],bs)  end;


(** Top-down merge sort **)

fun merge([],ys) = ys : real list
  | merge(xs,[]) = xs
  | merge(x::xs, y::ys) =
      if x<=y then x::merge(xs,  y::ys)
              else y::merge(x::xs,  ys);

(*naive version -- like Bird and Wadler, following Sedgewick*)
fun tmergesort [] = []
  | tmergesort [x] = [x]
  | tmergesort xs =
      let val k = length xs div 2
      in  merge (tmergesort (List.take(xs,k)),
                 tmergesort (List.drop(xs,k)))
      end;

(*faster version*)
fun tmergesort' xs =
      let fun sort (0, xs) = ([], xs)
	    | sort (1, x::xs) = ([x], xs)
	    | sort (n, xs) =
		let val (l1, xs1) = sort ((n+1) div 2, xs)
		    val (l2, xs2) = sort (n div 2, xs1)
		in (merge (l1,l2), xs2)
		end
          val (l, _) = sort (length xs, xs)
      in l end;


(** Bottom-up merge sort **)

fun mergepairs([l], k) = [l]
  | mergepairs(l1::l2::ls, k) =
      if k mod 2 = 1 then l1::l2::ls
      else mergepairs(merge(l1,l2)::ls, k div 2);

fun sorting([], ls, r) = hd(mergepairs(ls,0))
  | sorting(x::xs, ls, r) = sorting(xs, mergepairs([x]::ls, r+1), r+1);

fun sort xs = sorting(xs, [[]], 0);

(*O'Keefe's samsort*)
fun nextrun(run, []) =       (rev run, []: real list)
  | nextrun(run, x::xs) =
        if  x < hd run then  (rev run, x::xs)
                       else  nextrun(x::run, xs);

fun samsorting([], ls, k) = hd(mergepairs(ls,0))
  | samsorting(x::xs, ls, k) = 
      let val (run, tail) = nextrun([x], xs)
      in  samsorting(tail, mergepairs(run::ls, k+1), k+1)
      end;

fun samsort xs = samsorting(xs, [[]], 0);


(*** Polynomial arithmetic -- as a structure ***)

(*An obscure built-in infix in some compilers; we need it nonfix*)
nonfix rem;

structure Poly =
  struct
  type t = (int*real)list;

  val zero = [];

  (** Sum of two polynomials **)
  fun sum ([], us)               = us : t
    | sum (ts, [])               = ts
    | sum ((m,a)::ts, (n,b)::us) =
	     if m>n then (m,a) :: sum (ts, (n,b)::us)
	else if n>m then (n,b) :: sum (us, (m,a)::ts)
	else (*m=n*) 
	     if Real.==(a+b, 0.0) then sum (ts, us)
			          else (m, a+b) :: sum (ts, us);

  (** Product of a term and a polynomial **)
  fun termprod ((m,a), [])        = [] : t
    | termprod ((m,a), (n,b)::ts) = 
	(m+n, a*b) :: termprod ((m,a), ts);

  (** Product of two polynomials **)

  (*Product by balanced merging; could improve speed, like with merge sort*)
  fun prod ([], us)      = []
    | prod ([(m,a)], us) = termprod ((m,a), us)
    | prod (ts, us)      =
	let val k = length ts div 2
	in  sum (prod (List.take(ts,k), us), prod (List.drop(ts,k), us))
	end;

  (** Division: quotient and remainder **)

  (*Division by zero -- empty polynomial -- raises exception Match*)
  fun quorem (ts, (n,b)::us) =
    let fun dividing ([],        qs) = (rev qs, [])
	  | dividing ((m,a)::ts, qs) =
	      if m<n then (rev qs, (m,a)::ts)
	      else dividing (sum (ts, termprod ((m-n, ~a/b), us)),
			(m-n, a/b) :: qs)
    in  dividing (ts, [])  end;

  fun quo (ts,us) = #1(quorem (ts,us))
  and rem (ts,us) = #2(quorem (ts,us));

  (*A bad gcd algorithm: results are not in primitive form*)
  fun gcd ([], us) = us
    | gcd (ts, us) = gcd (rem (us,ts), ts);

  end;


