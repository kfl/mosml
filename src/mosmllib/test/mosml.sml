(* test/mosml.sml
   PS 1998-04-07, 1999-08-18
*)

use "auxil.sml";

local
    open Mosml
    fun d2w8 d = Word8Vector.foldr (op::) [] (doubleVec d);
    fun f2w8 f = Word8Vector.foldr (op::) [] (floatVec f);
	
    fun checkd2w2d d = vecDouble(doubleVec d) = d
    fun checkf2w2f f = 
	(f = 0.0 andalso vecFloat(floatVec f) = f
	 orelse abs((vecFloat(floatVec f) - f)/f) < 1E~6)
	andalso floatVec(vecFloat(floatVec f)) = floatVec f
	
in
    
val test1 =
    check'(fn _ => 
	   d2w8 0.0 = [0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0]
	   andalso d2w8 1.0 = 
	   [0wx3F, 0wxF0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0]
	   andalso d2w8 ~1.0 = 
	   [0wxBF, 0wxF0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0, 0wx0]
	   andalso d2w8 0.1 = 
	   [0wx3F, 0wxB9, 0wx99, 0wx99, 0wx99, 0wx99, 0wx99, 0wx9A] 
	   andalso d2w8 ~0.1 = 
	   [0wxBF, 0wxB9, 0wx99, 0wx99, 0wx99, 0wx99, 0wx99, 0wx9A]
	   andalso d2w8 3.14159 = 
	   [0wx40, 0wx9, 0wx21, 0wxF9, 0wxF0, 0wx1B, 0wx86, 0wx6E] 
	   andalso d2w8 3.14159E297 =  
	   [0wx7D, 0wxB3, 0wx36, 0wxF7, 0wxE, 0wx3D, 0wx61, 0wxD7] 
	   andalso d2w8 3.14159E~297 =  
	   [0wx2, 0wx60, 0wx6F, 0wxCD, 0wx8, 0wx36, 0wx66, 0wxBB]
	   andalso d2w8 ~3.14159E297 =  
	   [0wxFD, 0wxB3, 0wx36, 0wxF7, 0wxE, 0wx3D, 0wx61, 0wxD7]
	   andalso d2w8 ~3.14159E~297 =  
	   [0wx82, 0wx60, 0wx6F, 0wxCD, 0wx8, 0wx36, 0wx66, 0wxBB])
	   
val test2 = 
    check'(fn _ =>
	   List.all checkd2w2d [0.0, 1.0, ~1.0, 0.1, ~0.1, 3.14159,
				3.14159e297, 3.14159e~297, 
				~3.14159e297, ~3.14159e~297])

val test3 =
    check'(fn _ => 
	   f2w8 0.0 = 
	   [0wx0, 0wx0, 0wx0, 0wx0] 
	   andalso f2w8 1.0 = 
	   [0wx3F, 0wx80, 0wx0, 0wx0] 
	   andalso f2w8 ~1.0 = 
	   [0wxBF, 0wx80, 0wx0, 0wx0]
	   andalso f2w8 0.1 = 
	   [0wx3D, 0wxCC, 0wxCC, 0wxCD]
	   andalso f2w8 ~0.1 = 
	   [0wxBD, 0wxCC, 0wxCC, 0wxCD]
	   andalso f2w8 3.14159 = 
	   [0wx40, 0wx49, 0wxF, 0wxD0]
	   andalso f2w8 3.14159E29 =  
	   [0wx70, 0wx7D, 0wxC6, 0wx8F]
	   andalso f2w8 3.14159E~29 =  
	   [0wx10, 0wx1F, 0wx4C, 0wx2B] 
	   andalso f2w8 ~3.14159E29 =  
	   [0wxF0, 0wx7D, 0wxC6, 0wx8F]
	   andalso f2w8 ~3.14159E~29 =  
	   [0wx90, 0wx1F, 0wx4C, 0wx2B]) 
	   
val test4 = 
    check'(fn _ =>
	   List.all checkf2w2f [0.0, 1.0, ~1.0, 0.1, ~0.1, 3.14159,
				3.14159e29, 3.14159e~29, 
				~3.14159e29, ~3.14159e~29])

val test5a = 
    (case run "mosmlc" ["testrun.sml", "-o mosmltestrun"] "" of
	 Success _ => "OK"
       | Failure s => "WRONG: " ^ s)
    handle _ => "EXN";

val test5b = 
    (case run "./mosmltestrun" [] "01234\n56789" of
	 Success "[01234\n56789]\n<01234\n56789>\n---===" => "OK"
       | Success _ => "WRONG"
       | Failure s => "WRONG: " ^ s)
	 handle _ => "EXN";

end
