(* N queens problem: counting the solutions		sestoft@dina.kvl.dk 

 Instead of storing the solutions, we just count them.
 See file queens.sml for explanation of the algorithm.

*)

local 
    fun diag' d1 d2 []       = true
      | diag' d1 d2 (q1::qr) = 
	d1<>q1 andalso d2<>q1 andalso diag' (d1+1) (d2-1) qr
    fun diag mid right = diag' (mid+1) (mid-1) right

    fun accuqueens []      tail res = res + 1
      | accuqueens (x::xr) tail res = cycle [] x xr tail res
    and cycle left mid [] tail res = 
	if diag mid tail then accuqueens left (mid :: tail) res else res
      | cycle left mid (right as r::rr) tail res = 
	cycle (mid::left) r rr tail
	      (if diag mid tail then accuqueens (left@right) (mid :: tail) res 
	       else res)
in
    fun countqueens n = accuqueens (List.tabulate(n, fn x => x+1)) [] 0
end
