(* Efficient generation of permutations                 sestoft@dina.kvl.dk 

 Function accuperms generates permutations from the tail rather than
 the head of the list, and uses an accumulating parameter for the
 resulting list of permutations.  This considerably reduces the number
 of cons operations performed.  
*)

local 
    fun accuperms []      tail res = tail :: res
      | accuperms (x::xr) tail res = cycle [] x xr tail res
    and cycle left mid [] tail res = accuperms left (mid::tail) res
      | cycle left mid (right as r::rr) tail res = 
        cycle (mid::left) r rr tail (accuperms (left @ right) (mid::tail) res)
in
    fun perms xs = accuperms xs [] []
    fun permsn n = perms (List.tabulate(n, fn x => x+1))
end

(* Properties:
   * accuperms xs tail res = map (fn pm => pm @ tail) xs_perms @ res
        where xs_perms is a list of permutations of xs

   * cycle [] x1 [x2, ..., xn] tail res =
           accuperms (xs-xn) (xn::tail) 
                 (accuperms (xs-x(n-1)) (x(n-1)::tail) 
                      ...
                       (accuperms (xs-x1) (x1::tail) res) ...)
        where xs-x is the list xs with x removed.

   Invariants:
   * In every call (accuperms xs tail res), xs@tail is a permutation of the 
     original argument to perms.
   * In every call (cycle left mid right tail res), left@mid::right@tail is 
     a permutation of the original argument to perms.
*)
