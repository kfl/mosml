(* CommandLine, 1997-03-07 *)

local 
    prim_val argv_   : string Vector.vector = 0 "command_line";
in

fun name () = 
    if Vector.length argv_ > 0 then Vector.sub(argv_, 0) 
    else raise Fail "failed in CommandLine.name"

fun arguments () = 
    let fun h i res = 
	if i >= 1 then h (i-1) (Vector.sub(argv_, i) :: res)
	else res
    in h (Vector.length argv_ - 1) [] end;
end
