(* Finding the subsets of a list's elements		sestoft@dina.kvl.dk

 The set of subsets of a set can be defined recursively as follows:

 * the empty set {} has a single subset, {}
 * a non-empty set S can be written as {x} U A, with x not in A.
   The subsets of S are all the subsets of A, plus all the subsets of A 
   with x added as an element.

 In symbols:

 subsets({}) = {{}}
 subsets({x} U A) = subsets(A)  U  { {x} U B  |  B in subsets(A) }

*)


(* Solution 1: A direct translation of the recursion formula *)

local
    fun add x ys = x :: ys
in
    fun subsets1 []      = [[]]
      | subsets1 (x::xr) = subsets1 xr @ map (add x) (subsets1 xr)
end


(* Solution 2: avoid recomputing the subsets of xr *)

local
    fun add x ys = x :: ys
in
    fun subsets2 []      = [[]]
      | subsets2 (x::xr) = 
	let val xrsubsets = subsets2 xr
	in
	    xrsubsets  @  map (add x) xrsubsets
	end
end


(* Solution 3: Use one accumulating parameter *)

local
    fun subsets3a []      base = [base]
      | subsets3a (x::xr) base = 
	subsets3a xr base @ subsets3a xr (x::base)
in
    fun subsets3 xs = subsets3a xs []
end


(* Solution 4: Use two accumulating parameters *)

local
    fun subsets4a []      base res = base :: res
      | subsets4a (x::xr) base res = 
	subsets4a xr base (subsets4a xr (x::base) res)
in
    fun subsets4 xs = subsets4a xs [] []
end;


(* Expected equivalences between the functions:

 subsets2 xs = subsets1 xs

 subsets3a xs base = map (fn ss => rev ss @ base) (subsets1 xs)

 subsets4a xs base res = subsets3a xs base @ res
*)


(* Time the four solutions *)

load "Mosml"; 
val time = Mosml.time;

print "Try\n\
      \      time (ignore o subsets1) [1,2,3,4,5,6,7,8,9,10,11,12,13];\n\
      \      time (ignore o subsets2) [1,2,3,4,5,6,7,8,9,10,11,12,13];\n\
      \      time (ignore o subsets3) [1,2,3,4,5,6,7,8,9,10,11,12,13];\n\
      \      time (ignore o subsets4) [1,2,3,4,5,6,7,8,9,10,11,12,13];\n\n"


