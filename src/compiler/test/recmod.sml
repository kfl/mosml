(* recursive signatures and structures *)

(* failure tests *)

signature Fail1 = rec(Fail: sig type a; type b end)
    sig 
    end;

signature Fail2 = rec(Fail:functor(Y:sig end)->sig end)
    sig 
    end;

signature Fail3 = rec(Fail:sig end)
    functor(Fail:sig end)->sig end;


(* occur check violations *)
signature Fail4 = rec(Fail: sig type a; type b end)
                sig type a = Fail.a
                    type b = Fail.b
                 end;

signature Fail5 = rec(Fail: sig type a; type b end)
                sig type a = Fail.b
                    type b = Fail.a
                 end;

(* equality type violations *)

signature Fail4 = rec(Fail: sig type a; eqtype b end)
                 sig datatype a = Aa | Ba of Fail.b 
                     datatype b = Ab | Bb of Fail.a
                 end;

(* success tests *)
signature Ok1 = rec(X: sig type a; type b end)
                sig datatype a = Aa | Ba of X.b 
                    datatype b = Ab | Bb of X.a
                end;

signature Ok2 = rec(X: sig type a; type b end)
                 sig datatype a = Aa | Ba of X.b 
                     datatype b = Ab | Bb of a
                 end;

signature Ok3 = rec(X: sig type a; type b end)
                 sig 
                     datatype b = Ab | Bb of X.a
		     datatype a = Aa | Ba of b 
                 end;

signature OK4 = rec(X: sig eqtype a; eqtype b end)
                 sig datatype a = Aa | Ba of X.b 
                     datatype b = Ab | Bb of X.a
                 end;

signature Ok5 = rec(X: sig type a; type b end)
                sig type a
                    type b                
		end;

signature Ok6 = rec(X: sig type a = int end)
                sig type a = int
		end;

signature OK7 = rec(X: sig datatype a = Aa | Ba of b  
			        and b = Ab | Bb of a
		       end)
                 sig datatype a = Aa | Ba of X.b 
                     datatype b = Ab | Bb of X.a
                 end;

signature OK8 = rec(X: sig datatype a = Aa | Ba of b  
			        and b = Ab | Bb of a
		       end)
                 sig datatype a = Aa | Ba of b 
                          and b = Ab | Bb of a
                 end;

structure Ok = rec(X:sig end) struct end;

structure Even = rec(X:sig val even:int -> bool end) 
    struct 
	val even = fn x => if x = 0 then true else not(X.even (x-1))
    end;

val test1 = Even.even 10;

structure EvenOdd = rec(X:sig val even:int -> bool val odd:int->bool end) 
    struct 
	val even = fn x => if x = 0 then true else (X.odd (x-1))
	val odd = fn x => if x = 0 then false else (X.even (x-1))
    end;

val test2 = EvenOdd.even 10;
val tes3 = EvenOdd.odd 10;

signature U = rec(X: sig structure A: sig type a end;
                         structure B: sig type b end
                     end)
                 sig structure A: sig datatype a = Aa | Ba of X.B.b end
                     structure B: sig datatype b = Ab | Bb of X.A.a end
                 end;

structure U = rec(X:U)
    struct structure A = struct datatype a = datatype X.A.a end
	   structure B = struct datatype b = datatype X.B.b end
    end;
		       
signature V = rec(X: sig structure A: sig type a end;
                         structure B: sig type b end
                     end)
                 sig structure A: sig datatype a = Aa | Ba of X.B.b
                                   val height: a -> int
                               end;
                     structure B: sig datatype b = Ab | Bb of X.A.a 
                                   val height: b -> int
                               end
                 end;

structure V = rec(X:V)
    struct structure A = 
	      struct datatype a = datatype X.A.a 
		     fun height Aa = 0
		     |   height (Ba b) = 1 + X.B.height b
	      end
           structure B = 
              struct datatype b = datatype X.B.b
		      fun height Ab = 0
		      |   height (Bb a) = 1 + X.A.height a
	      end
    end;

val test4 = V.A.height (V.A.Ba (V.B.Bb V.A.Aa));
val test5 = V.B.height (V.B.Bb(V.A.Ba (V.B.Bb V.A.Aa)));

structure V1 = V : V;
structure V2 = V :> V;

(* Y exports more components than specified in V *)
structure Y = rec(X:V)
    struct structure A = 
	      struct datatype a = datatype X.A.a 
		     fun height Aa = 0
		     |   height (Ba b) = 1 + X.B.height b
		     val foo = 1
	      end
           structure B = 
	       struct datatype b = datatype X.B.b
		      fun height Ab = 0
		      |   height (Bb a) = 1 + X.A.height a
		      val bar = true
	       end
    end;

structure Y1 = Y : V;
structure Y2 = Y :> V;


(* test equivalence of recursive signatures *)

val test6 = if true 
		then [structure Y1 as V] 
	    else [structure Y1 as V];



(* a subtle case thats fails if we only realize, but
   don't substitute, type names in the forward
   spec with their matching type names in the body of the signature
*)

signature S = rec(X: sig type a end)
                sig datatype a = C of X.a
		end;

structure A = rec(X:S)
    struct
	datatype a = datatype X.a
    end;

structure B = A :> S;

structure C = B :> S;


signature S = rec(X: sig type ('a,'b) a end)
                sig datatype ('a,'b) a = C of ('a,'b) X.a | D of 'a | E of 'b
		end;

structure A = rec(X:S)
    struct
	datatype a = datatype X.a
    end;

structure B = A :> S;

structure C = B :> S;


(* one last wild test ... *)
signature S = rec(X: sig datatype ('a,'b) a = C of ('a,'b) a | D of 'a | E of 'b end)
                sig datatype ('a,'b) a = C of ('a,'b) X.a | D of 'a | E of 'b
		end;

structure A = rec(X:S)
    struct
	datatype a = datatype X.a
    end;

structure B = A :> S;

structure C = B :> S;





