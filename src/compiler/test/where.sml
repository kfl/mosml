structure A = struct val a = 1 
                     structure B = struct val b = true
					  val c = "c"
				   end
                     val d = nil
                     type t = int
                     exception e;
	      end;

type u = X.t where X = A;
type u = X.v where X = struct type v = int datatype t = C of t end;

functor A = functor X:sig end =>struct datatype v = C end;
type u = X.v where X = A();
datatype u = datatype X.v where X = A();


type fail = X.t where X = struct type v = int datatype t = C of t end;
type fail = X.v where X = struct datatype t = C of t type v = t end;
functor G = functor(X:sig end)=>struct datatype v = C end;
type fail = X.v where X = G();










