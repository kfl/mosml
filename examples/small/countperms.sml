(* Count permutations					sestoft@dina.kvl.dk 

 Function accuperms counts permutations.
 See file perms.sml for explanations.
*)

local 
    fun accuperms []      tail res = res + 1
      | accuperms (x::xr) tail res = cycle [] x xr tail res
    and cycle left mid [] tail res = accuperms left (mid::tail) res
      | cycle left mid (right as r::rr) tail res = 
        cycle (mid::left) r rr tail (accuperms (left @ right) (mid::tail) res)
in
    fun countperms xs = accuperms xs [] 0
    fun countpermsn n = countperms (List.tabulate(n, fn x => x+1))
end
