(* Example: implementing dynamically sized, functional arrays 
            using first-class modules *)

(* the  signature  "Array" specifies a structure implementing fixed-size arrays.
*)

signature  Array  =
  sig  type 'a array;
       val init:'a  ->  ('a array);
       val sub: ('a array)  ->  int  ->  'a;
       val update: ('a array)  ->  int  ->  'a  ->  ('a array)
  end;

(* the structure "ArrayZero" implements arrays of size 2^0 = 1*)

structure  ArrayZero =
    struct
         type 'a array = 'a;
         fun init x = x;
         fun sub a i = a
         fun update a i x = x
    end;


(* The  functor  "ArraySucc"  maps  an  implementation  
   of arrays of size 2^n  to an implementation of arrays of size 2^(n+1)
*)
functor ArraySucc (A:Array) =
      struct
             type 'a array  =  ('a A.array)  *  ('a A.array);            
             fun init x =  (A.init  x, A.init  x)
             fun sub (even,odd) i =
                   if  (i mod  2) = 0
                   then A.sub  even  (i div  2)
                   else A.sub  odd  (i div 2);
             fun update  (even,odd) i x = 
   		   if  (i mod  2) = 0
		   then (A.update even (i div  2)  x, odd)
		   else (even, A.update  odd (i div 2)  x)
      end;

(* The  Core function  "mkArray n" (n >=0)  returns an package implementating
   arrays of size 2^n.
*)

fun  mkArray 0 = [structure  ArrayZero  as  Array]        (* base case *)
  |  mkArray n = let structure A as Array = mkArray (n-1) (* inductive step *)
		 in
                     [structure  ArraySucc(A)  as  Array]
                 end
;


(* tests *)
(* create a package implementing arrays of size 2^10 = 1024 and unpack it *)
structure A as Array = mkArray 10; 
val a = A.init "yes"; (* an array of 1024 strings *)
val test1 = A.sub a 864 = "yes";
val test2 = A.sub (A.update a 864 "no") 864 = "no";





	
    
    
    
