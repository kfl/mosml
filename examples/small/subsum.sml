(* The subset sum problem				sestoft@dina.kvl.dk

   Given a set (list) of integers we can find the sum of its elements.
   The subset sum problem is: How many subsets of a set xs has sum s?

   The answer can be computed as 
	length (List.filter (fn ss => s = foldl (op+) 0 ss) (subsets4 xs))
   but this is inefficient, since all the subsets must be built before 
   their sums are computed and checked.

   A much faster solution is obtained by modifying Solution 4 in the
   subset computation (see file subsets.sml).
*)

local
    fun subsuma []      0       res = res + 1 
      | subsuma []      missing res = res
      | subsuma (x::xr) missing res = 
	subsuma xr missing (subsuma xr (missing-x) res)
in
    fun subsum xs sum = subsuma xs sum 0
end;

print "Try\n\
      \     subsum [1,2,3] 3;\n\
      \     subsum [1,2,3,~1,4] 3;\n\
      \     subsum [] 3;\n\
      \     subsum [] 0;\n\
      \     subsum [1,2,3,4,5,6,7,8,~8,0,~14] 6;\n\n"

