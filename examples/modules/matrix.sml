(* Example: implementing dynamically sized, functional matrices 
            using first-class modules *)

use "array.sml";

signature  Matrix  =
  sig  type 'a matrix;
       val init:'a  ->  ('a matrix);
       val sub: ('a matrix)  ->  int list ->  'a;
       val update: ('a matrix)  ->  int list ->  'a  ->  ('a matrix)
  end;

(* mkMatrix [d1, ..., dn] constructs a package
   implementing n-dimensional matrices of size 2^d1 * ... * 2^dn
*) 

fun mkMatrix [d] = 
	let structure A as Array = mkArray d
        in	
	    [structure 
		 struct 
		     type 'a matrix = 'a A.array
		     fun init a = A.init a
                     fun sub m [d] = A.sub m d
                     fun update m [d] a = A.update m d a
		 end
	     as Matrix]
	end
  |  mkMatrix (d::ds) = 
	let structure M as Matrix = mkMatrix ds
	    structure A as Array = mkArray d	    
	in  [structure
		 struct
		     type 'a matrix = ('a M.matrix) A.array
 		     fun init a = A.init (M.init a)
                     fun sub m (d::ds) = M.sub (A.sub m d) ds
                     fun update m (d::ds) a = 
			 A.update m d (M.update (A.sub m d) ds a)
		 end
             as Matrix]
	end
;

structure M as Matrix = mkMatrix [2,4];
val m = M.init 0;
M.sub m [1,1];
val m = M.update m [1,1] 2;
M.sub m [1,1]; 





