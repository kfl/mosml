functor F(A:sig val a : unit end)(B:sig val b: unit end)(C:sig val c: unit end) = struct end;
structure A = struct val a = () end
and B = struct val b = () end 
and C = struct val c = () end;

functor F = F;
functor FA = F A;
functor FAB = F A B;
structure FABC = F A B C;

functor H(F:functor(X:sig end)->sig end)= op F;
structure X =H(functor(X:sig end)=>X)(struct end);

signature A = sig type a val a : a end;
signature B = sig type b val b : b end;
signature C = sig type c val c : c end;

functor X = functor(X:A)=>X;
functor X:functor(X:A)->A = functor(X:A)=>X;
functor X:>functor(X:A)->A = functor(X:A)=>X;
functor X(A:A)(B:B)(C:C) = A;
functor X(A:A)(B:B)(C:C):>A = A;
functor X(A:A)(B:B)(C:C):>A = A;


signature F = functor(X:A)->B;
signature G = functor(F:F)->C;
signature H = functor(G:G)->G;


structure A = struct type a = int val a = 1 end;
structure B = struct type b = int val b = 2 end;
structure C = struct type c = int val c = 3 end;

functor F (X:A) = B;
structure FA = F(A);

functor G (F:F) = let structure B = F(A) in C end;
structure GF = G(F);


functor F (X:sig end) = struct end;
structure FA = F(A);

functor F (X:sig type a val a : a end) = struct end;
structure FA = F(A);


functor F (X:sig type a val a : a end) = struct end;
structure FA = F(A);


(* derived forms *)
signature A = sig type a val a : a end;
signature B = sig type b val b : b end;
signature C = sig type c val c : c end;

signature S = sig functor F1 : functor(X:A)->B;
                  functor F2 (X:A):B;
                  functor F3 (X:A)(B:B):C;
(*                functor F4 X:A :C; 
                  functor F5 X:A X:B : C; 
                  functor F6 (X:A) X:B : C;
                  functor F7 X:A (X:B) : C *)
              end;

(* applicative functors and signatures *)
signature F = functor X:sig type t end ->sig end;
signature F = functor X:sig  end ->sig type u end;
signature F = functor X:sig type t end ->sig type u end;
functor F  X:sig type t type w type x end= struct datatype u = C datatype v = D end;
functor F  X:sig type t type w type x end= struct datatype 'a u = C of 'a datatype 'b v = D of 'b end;



