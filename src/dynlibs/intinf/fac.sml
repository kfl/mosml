app load ["IntInf", "PP", "Mosml"];

local 
    open IntInf;
in
    fun ppintinf pps (i : IntInf.int) = 
	let open PP 
	in
	    begin_block pps INCONSISTENT 0; 
	    add_string pps (toString i);
	    end_block pps
	end

    fun faci 0 res = res
      | faci n res = faci (Int.-(n, 1)) (fromInt n * res)

    fun fac n = faci n (fromInt 1)
end

val fac = fn n => Mosml.time fac n

val _ = installPP ppintinf;

(* Efficiency: factorial 10000:

   imperative C implementation:       9.5 sec user time
   imperative mosml implementation:  33.5 sec user time
   applicative mosml implementation: 68.6 sec user time
*)

