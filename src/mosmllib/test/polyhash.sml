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

    local 
	datatype reflist = 
	    Empty
	  | Node of int * reflist ref 
	    
	val tail = ref Empty
	val cyclic = Node(117, tail)
	val _ = tail := cyclic
    in
	val test3 = 
	    check'(fn _ => hash cyclic = hash cyclic)
    end

    fun mkstr 0 s = ""
      | mkstr 1 s = s
      | mkstr n s = let val r = mkstr (n-1) s in r ^ r end	

    fun equal pref i = (hash (pref ^ mkstr i "abc") 
			= hash (pref ^ mkstr i "abc"));

    fun unequal pref i = (hash (pref ^ mkstr i "abc") 
			  <> hash (pref ^ mkstr i "def"));

    val blank128 = mkstr 8 " "

    val test4 = 
	check'(fn _ => List.all (equal "") [0,1,2,3,4,5,6,7,8,9]);

    val test5 = 
	check'(fn _ => List.all (unequal "") [1,2,3,4,5,6,7,8,9]);

    val test6 = 
	check'(fn _ => List.all (equal blank128) [0,1,2,3,4,5,6,7,8,9]);

    val test7 = 
	check'(fn _ => List.all (unequal blank128) [1,2,3,4,5,6,7]); 
end
