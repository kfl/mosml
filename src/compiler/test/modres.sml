(* test longmodid resolution *)

signature S = sig end;
signature F = functor(X:sig end)->sig end;

functor Ok(X:S) = X;
functor Fail(X:S) = op X;

functor Ok(X:F) = op X;
functor Fail(X:F) = X;

functor Ok(X:S) = op X : S;
functor OK(X:S) = X : S;

functor Ok(X:F) = op X : F;
functor OK(X:F) = X : F;

functor Ok(X:S) = op X :> S;
functor OK(X:S) = X :> S;

functor Ok(X:F) = op X :> F;
functor OK(X:F) = X :> F;

structure S = struct end;
functor F(X:S) = X;
functor H(X:F) = op X;

structure Ok = F(S);
structure Ok = F(op S);

functor Ok = H(F);
functor Ok = H(op F);

structure S = struct end;
functor F = functor(X:sig end)=>struct end;
functor G X:S = X;
functor H X:F = op X;

structure Ok = G(S);
structure Ok = G(op S);

functor Ok = H(F);
functor Ok = H(op F);

functor Ok(X:S) = let in X end;
functor Fail(X:S) = let in op X end;

functor Ok(X:F) = let in op X end;
functor Fail(X:F) = let in X end;

functor Ok(X:S) = let in op X end : S;
functor OK(X:S) = let in X end : S;

functor Ok(X:F) = let in op X end : F;
functor OK(X:F) = let in X end : F;






