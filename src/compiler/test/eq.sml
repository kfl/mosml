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


(* test equality attribute matching of type abbreviations with free variables *)

fun f x = let structure X as sig type t = 'a end = x in x end;
val g = f : [sig type t = int -> int end] -> [sig type t = int -> int end];
val p = [structure struct type t = int -> int end as sig type t = int -> int end];
val ok = f p;
val ok = g p;

(* test equality attribute matching of type abbreviations with free equality type variables *)
fun f x = let structure X as sig type t = ''a end = x in x end;
val g = f : [sig type t = int * int end] -> [sig type t = int * int end];
val p = [structure struct type t = int * int end as sig type t = int * int end];
val q = [structure struct type t = ''b * ''b end as sig type t = ''b * ''b end];
val r = [structure struct type t = ''b * 'c  end as sig type t = ''b * 'c end];
val t = [structure struct type t = int -> int end as sig type t = int -> int end];
val ok = f p;
val ok = g p;
val ok = f q;
val ok = f r;
val fail = f t;

(* ok to realise equality type constructor by equality type *)
fun ok x = let structure X as sig eqtype t end where type t = ''a = x in x end;
fun ok x = let structure X as sig eqtype t end where type t = ''a * ''b = x in x end;
fun ok x = let structure X as sig eqtype 'b t end where type 'b t = ''a * 'b = x in x end;

(* wrong to realise equality type constructor by nonequality type *)
fun fail x = let structure X as sig eqtype t end where type t = 'a = x in x end;
fun fail x = let structure X as sig eqtype t end where type t = ''a -> ''b  = x in x end;
fun fail x = let structure X as sig eqtype 'b t end where type 'b t = 'a -> 'b = x in x end;



