(* N queens problem					sestoft@dina.kvl.dk 

 Observe that the legal placements of N rooks in an NxN chessboard are
 precisely the permutations of N numbers.  Therefore legal placements
 of N queens in an NxN chessboard can be obtained by generating
 permutations under additional constraints on the diagonals.

 This code is a straightforward specialization of the code for
 efficient generation of permutations; see file perms.sml.
*)

local 
    fun diag' d1 d2 []       = true
      | diag' d1 d2 (q1::qr) = 
	d1<>q1 andalso d2<>q1 andalso diag' (d1+1) (d2-1) qr
    fun diag mid right = diag' (mid+1) (mid-1) right

    fun accuqueens []      tail res = tail :: res
      | accuqueens (x::xr) tail res = cycle [] x xr tail res
    and cycle left mid [] tail res = 
	if diag mid tail then accuqueens left (mid :: tail) res else res
      | cycle left mid (right as r::rr) tail res = 
	cycle (mid::left) r rr tail
	      (if diag mid tail then accuqueens (left@right) (mid :: tail) res 
	       else res)
in
    fun queens n = accuqueens (List.tabulate(n, fn x => x+1)) [] []
end

(*		   N queens
	N 	   solutions	     permutations     ratio
   ------------------------------------------------------------
	0		1			1	1
	1		1			1	1
	2		0			2	0
	3		0			6	0
	4		2		       24	1/12
	5	       10		      120	1/12
	6		4		      720	1/180
	7	       40		     5040	1/126
	8	       92		    40320	1/438
	9	      352		   362880	1/1030
       10	      724		  3628800	1/5012
       11	     2680		 39916800	1/14894
       12	    14200		479001600	1/33732
       13	    73712	       6227020800	1/84477
       14	   365596	      87178291200	1/238455
       15	  2279184	    1307674368000	1/573746
       16	 14772512	   20922789888000	1/1416332
       17	 95815104	  355687428096000	1/3712227
       18       666090624        6402373705728000	1/9611865

Computing and storing the solutions to the 14-queens problem requires 
35-50 MB RAM with SML/NJ 109.30 and 42 MB RAM with Moscow ML 1.42.

Counting the solutions to the 14-queens problem requires 1144 KB RAM
and 1296 sec with Moscow ML and 3000-3800 KB RAM and 57 sec with
SML/NJ 109.30 on 250 MHz Ultrasparc.

Counting the solutions to the 15-queens problem requires 676 KB RAM,
180 min with Moscow ML and 5268 KB RAM, 30 min with SML/NJ 109.16 on
150 MHz Pentium.

Counting the solutions to the 17-queens problem requires 12720 KB RAM,
319:38 min with SML/NJ 109.30 on 250 MHz UltraSparc.

Counting the solutions to the 18-queens problem requires 13816 KB RAM,
2487:46 min with SML/NJ 109.30 on 250 MHz UltraSparc.  
*)
