(* tricky type inference tests *)
(* the type inferred for i must be int. *)

(* integer: int -> int *)

val integer = fn 0 => 0 | i => i;

val ok = fn i => let structure X = struct val x = integer i end in X.x end;;

(* the type inferred for i must be int *)
val ok = fn i => let structure X = struct type t = Y.u 
                                          where Y = struct type u = int;
							   val x = integer i 
						    end;
					  val y = 1
				   end
                  in X.y end;

(* the type inferred for i must be int *)

val ok = fn i => let structure X = struct val x = i end :  sig val x: int end
		 in X.x end;

(* should type-check because i is declared in the scope of the type A.a *)
functor OK(A:sig type a; val a: a end) = 
struct val x = fn i => 
	        let structure Y = 
		    (functor (B:sig type b; val b:b end) =>
			 struct val y = fn j => if true then i
						else A.a
			 end)
			 struct type b = int; val b = 1 end
		in Y.y 
		end
end;

(* should fail to type-check because the type B.b is declared 
   within the scope of i *)
functor Fail(A:sig  type  a;  val  a:a  end)= 
struct val x = fn i =>
	       let structure Y =
		   (functor(B:sig type b; val b:b end) =>
			struct val y = fn j => if true then i
					       else  B.b
			end)
			struct type b = int; val b = 1 end
	       in Y.y
	       end
end;

(* should type-check by equating the types of i and j *)
functor OK(A:sig  type  a;  val  a:a  end) = 
struct val x = fn i => 
               let structure Y = 
		   (functor(B:sig type b; val b:b end) =>
                            struct val y = fn j => if true then i
                                                       else j
                                             end)
                         struct type b = int; val b = 1 end
	       in Y.y
	       end
end;;

(* should  fail to type-check because the type B.b is 
   declared within the scope of i and hence cannot unify
   with the type of i *)

functor Fail(A:sig type a; val a:a end) =
struct val x = fn i => 
               let structure Y = 
		   (functor(B:sig type b; val b:b end) =>
			struct val y = fn j => ((if true then i else j),
            		                     (if true then j else B.b))
			end)
                         struct type b = int; val b = 1 end
	       in Y.y
	       end
end;







