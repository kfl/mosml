functor F X:sig type t end = struct datatype u = C of X.t ref end

structure  A = F (struct type t = unit end);
val ok = op = : A.u * A.u -> bool;

structure  B = F (struct type t = unit -> unit end);
val ok = op = : B.u * B.u -> bool;

functor G X:sig type t end = struct datatype u = C of X.t end;
structure C = G(struct type t = unit end);
val fail = op = : C.u * C.u -> bool;
structure D = G(struct type t = unit -> unit end);
val fail = op = : D.u * D.u -> bool;

functor H X:sig eqtype t end = struct datatype u = C of X.t end;
structure E = H(struct type t = unit end);
val ok = op = : E.u * E.u -> bool;

functor I X:sig type s eqtype t end = struct datatype u = C of X.t end;
structure F = I(struct type s = unit type t = unit end);
val ok = op = : F.u * F.u -> bool;
structure G = I(struct type s = unit -> unit type t = unit end);
val ok = op = :G.u * G.u -> bool;


functor ok(F:functor X:sig type t end -> sig eqtype u end) =
    op F:functor X:sig type t end -> sig eqtype u end;

(* test contra-variant domain *)
functor ok(F:functor X:sig type t end -> sig eqtype u end) =
    op F:functor X:sig eqtype t end -> sig eqtype u end;

(* test co-variant range *)
functor ok(F:functor X:sig type t end -> sig eqtype u end) =
    op F:functor X:sig type t end -> sig type u end;

(* test contra-variant domain and  co-variant range *)

functor ok(F:functor X:sig type t end -> sig eqtype u end) =
    op F:functor X:sig eqtype t end -> sig type u end;

(* test co-variant domain fails *) 

functor fail(F:functor X:sig eqtype t end -> sig type u end) =
    op F:functor X:sig type t end -> sig type u end;

(* test contra-variant range fails *) 
functor fail(F:functor X:sig eqtype t end -> sig type u end) =
    op F:functor X:sig eqtype t end -> sig eqtype u end;





