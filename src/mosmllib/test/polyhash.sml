(* test/polyhash.sml
   PS 1998-04-03
*)

load "Polyhash";
use "auxil.sml";

local
    open Polyhash

in
    val test1 = 
	check'(fn _ => hash (~0.0) = hash 0.0 
	       andalso hash (~ 0.0) = hash (~0.0))

    val test2 = 
	check'(fn _ => hash ("abc" ^ "def") = hash "abcdef")
end
