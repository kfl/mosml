(**** ML Programs from Chapter 4 of

  ML for the Working Programmer, 2nd edition
  by Lawrence C. Paulson, Computer Laboratory, University of Cambridge.
  (Cambridge University Press, 1996)

Copyright (C) 1996 by Cambridge University Press.
Permission to copy without fee is granted provided that this copyright
notice and the DISCLAIMER OF WARRANTY are included in any copy.

DISCLAIMER OF WARRANTY.  These programs are provided `as is' without
warranty of any kind.  We make no warranties, express or implied, that the
programs are free of error, or are consistent with any particular standard
of merchantability, or that they will meet your requirements for any
particular application.  They should not be relied upon for solving a
problem whose incorrect solution could result in injury to a person or loss
of property.  If you do use the programs or functions in such a manner, it
is at your own risk.  The author and publisher disclaim all liability for
direct, incidental or consequential damages resulting from your use of
these programs or functions.
****)


(*** King and his subjects ***)

datatype person = King
                | Peer of string*string*int
                | Knight of string
                | Peasant of string;

fun title King = "His Majesty the King"
  | title (Peer(deg,terr,_)) = "The " ^ deg ^ " of " ^ terr
  | title (Knight name)  = "Sir " ^ name
  | title (Peasant name) = name;

fun sirs [] = []
  | sirs ((Knight s) :: ps) = s :: (sirs ps)
  | sirs (p :: ps)        = sirs ps;

fun superior (King, Peer _) = true
  | superior (King, Knight _) = true
  | superior (King, Peasant _) = true
  | superior (Peer _, Knight _) = true
  | superior (Peer _, Peasant _) = true
  | superior (Knight _, Peasant _) = true
  | superior _ = false;

datatype degree = Duke | Marquis | Earl | Viscount | Baron;

fun lady Duke     = "Duchess"
  | lady Marquis  = "Marchioness"
  | lady Earl     = "Countess"
  | lady Viscount = "Viscountess"
  | lady Baron    = "Baroness";


(*** Making change: backtracking version, using exceptions*)

exception Change;
fun backChange (coinvals, 0)      = []
  | backChange ([],  amount)      = raise Change
  | backChange (c::coinvals, amount) =
      if amount<0 then raise Change
                  else c :: backChange(c::coinvals, amount-c)
		       handle Change => backChange(coinvals, amount);


(*** Binary trees ***)

datatype 'a tree = Lf
                 | Br of 'a * 'a tree * 'a tree;

structure Tree =
  struct
  fun size Lf = 0
    | size (Br(v,t1,t2)) = 1 + size t1 + size t2;

  fun depth Lf = 0
    | depth (Br(v,t1,t2)) = 1 + Int.max (depth t1, depth t2);

  fun reflect Lf = Lf
    | reflect (Br(v,t1,t2)) = 
	  Br(v, reflect t2, reflect t1);

  fun preord (Lf, vs) = vs
    | preord (Br(v,t1,t2), vs) =
	  v :: preord (t1, preord (t2, vs));

  fun inord (Lf, vs) = vs
    | inord (Br(v,t1,t2), vs) =
	  inord (t1, v::inord (t2, vs));

  fun postord (Lf, vs) = vs
    | postord (Br(v,t1,t2), vs) =
	  postord (t1, postord (t2, v::vs));

  fun balpre  []    = Lf
    | balpre(x::xs) =
	let val k = length xs div 2
	in  Br(x, balpre(List.take(xs,k)),  balpre(List.drop(xs,k)))
	end;

  fun balin [] = Lf
    | balin xs =
	let val k = length xs div 2
	    val y::ys = List.drop(xs,k)
	in  Br(y, balin (List.take(xs,k)), balin ys)
	end;

  fun balpost xs = reflect (balpre (rev xs));
  end;



(*** Dictionaries as Binary search trees ***)

signature DICTIONARY = 
  sig
  type key				(*type of keys*)
  type 'a t				(*type of tables*)
  exception E of key			(*errors in lookup, insert*)
  val empty: 'a t			(*the empty dictionary*)
  val lookup: 'a t * key -> 'a
  val insert: 'a t * key * 'a -> 'a t
  val update: 'a t * key * 'a -> 'a t
  end;


(*Structure Order can vary; Tree avoids referring to a free structure. *)
structure Dict : DICTIONARY = 
  struct

  type key = string;
  type 'a t = (key * 'a) tree;

  exception E of key;

  val empty = Lf;

  fun lookup (Lf, b) = raise E b
    | lookup (Br ((a,x),t1,t2), b) =
	(case String.compare(a,b) of
	     GREATER => lookup(t1, b)
	   | EQUAL   => x
	   | LESS    => lookup(t2, b));

  fun insert (Lf, b, y) = Br((b,y), Lf, Lf)
    | insert (Br((a,x),t1,t2), b, y) =
	(case String.compare(a,b) of
	     GREATER => Br ((a,x),  insert(t1,b,y),  t2)
	   | EQUAL   => raise E b
	   | LESS    => Br ((a,x),  t1,  insert(t2,b,y)));

  fun update (Lf, b, y) = Br((b,y), Lf, Lf)
    | update (Br((a,x),t1,t2), b, y) =
	(case String.compare(a,b) of
	     GREATER => Br ((a,x),  update(t1,b,y),  t2)
	   | EQUAL   => Br ((a,y),  t1,  t2)
	   | LESS    => Br ((a,x),  t1,  update(t2,b,y)));

  end;


(*** Functional and flexible arrays ***)

(*Braun trees*)
structure Braun = 
  struct
  fun sub (Lf, _) = raise Subscript
    | sub (Br(v,t1,t2), k) =
	if k = 1 then v
	else if k mod 2 = 0 
	     then sub (t1, k div 2)
	     else sub (t2, k div 2);

  fun update (Lf, k, w) = 
	if k = 1 then Br (w, Lf, Lf)
	else raise Subscript
    | update (Br(v,t1,t2), k, w) =
	if k = 1 then Br (w, t1, t2)
	else if k mod 2 = 0 
	     then Br (v,  update(t1, k div 2, w),  t2)
	     else Br (v,  t1,  update(t2, k div 2, w));

  fun delete (Lf, n) = raise Subscript
    | delete (Br(v,t1,t2), n) =
	if n = 1 then Lf
	else if n mod 2 = 0 
	     then Br (v,  delete(t1, n div 2),  t2)
	     else Br (v,  t1,  delete(t2, n div 2));

  fun loext (Lf, w) = Br(w, Lf, Lf)
    | loext (Br(v,t1,t2), w) = Br(w, loext(t2,v), t1);

  fun lorem Lf = raise Size
    | lorem (Br(_,Lf,Lf)) = Lf 	(*No evens, therefore no odds either*)
    | lorem (Br(_, t1 as Br(v,_,_), t2)) = Br(v, t2, lorem t1);

  end;


(** Flexible arrays as abstract type **)

signature FLEXARRAY = 
  sig
  type 'a array
  val empty:  'a array
  val length: 'a array -> int
  val sub:    'a array * int -> 'a
  val update: 'a array * int * 'a -> 'a array
  val loext:  'a array * 'a -> 'a array
  val lorem:  'a array -> 'a array
  val hiext:  'a array * 'a -> 'a array
  val hirem:  'a array -> 'a array
  end;

(*These arrays are based from ZERO for compatibility with arrays in
  the Basis Library.  They check bounds and raise standard exceptions.*)
structure Flex : FLEXARRAY = 
  struct
  datatype 'a array = Array of 'a tree * int;

  val empty = Array(Lf,0);

  fun length (Array(_,n)) = n;

  fun sub (Array(t,n), k) = 
      if 0<=k andalso k<n then Braun.sub(t,k+1) 
      else raise Subscript;

  fun update (Array(t,n), k, w) = 
      if 0<=k andalso k<n then Array(Braun.update(t,k+1,w), n)
      else raise Subscript;

  fun loext (Array(t,n), w) = Array(Braun.loext(t,w), n+1);

  fun lorem (Array(t,n)) = 
      if n>0 then Array(Braun.lorem t, n-1)
      else raise Size;

  fun hiext (Array(t,n), w) = Array(Braun.update(t,n+1,w), n+1);

  fun hirem (Array(t,n)) = 
      if n>0 then Array(Braun.delete(t,n) , n-1)
      else raise Size;

  end;


(*** Priority queues ***)

signature PRIORITY_QUEUE =
  sig
  type item
  type t
  val empty    : t
  val null     : t -> bool
  val insert   : item * t -> t
  val min      : t -> item
  val delmin   : t -> t
  val fromList : item list -> t
  val toList   : t -> item list
  val sort     : item list -> item list
  end;

structure Heap : PRIORITY_QUEUE = 
  struct
  type item = real;
  type t = item tree;

  val empty = Lf;

  fun null Lf = true
    | null (Br _) = false;

  fun min (Br(v,_,_)) = v;

  fun insert(w: real, Lf) = Br(w, Lf, Lf)
    | insert(w, Br(v, t1, t2)) =
	if w <= v then Br(w, insert(v, t2), t1)
		  else Br(v, insert(w, t2), t1);

  fun leftrem (Br(v,Lf,Lf)) = (v, Lf)
    | leftrem (Br(v,t1,t2)) = 
        let val (w, t) = leftrem t1
	in  (w, Br(v,t2,t))  end;

  fun siftdown (w:real, Lf, Lf) = Br(w,Lf,Lf)
    | siftdown (w, t as Br(v,Lf,Lf), Lf) =
        if w <= v then Br(w, t, Lf)
                  else Br(v, Br(w,Lf,Lf), Lf)
    | siftdown (w, t1 as Br(v1,p1,q1), t2 as Br(v2,p2,q2)) =
        if w <= v1 andalso w <= v2 then Br(w,t1,t2)
        else if v1 <= v2 then Br(v1, siftdown(w,p1,q1), t2)
           (* v2 < v1 *) else Br(v2, t1, siftdown(w,p2,q2));

  fun delmin Lf = raise Size
    | delmin (Br(v,Lf,_)) = Lf
    | delmin (Br(v,t1,t2)) = 
        let val (w,t) = leftrem t1
	in  siftdown (w,t2,t)  end;

  fun heapify (0, vs) = (Lf, vs)
    | heapify (n, v::vs) =
	let val (t1, vs1) = heapify (n div 2, vs)
	    val (t2, vs2) = heapify ((n-1) div 2, vs1)
	in  (siftdown (v,t1,t2), vs2)  end;

  fun fromList vs = #1 (heapify (length vs, vs));

  fun toList (t as Br(v,_,_)) = v :: toList(delmin t)
    | toList Lf = [];

  fun sort vs = toList (fromList vs);
  end;


(*** Propositional logic -- tautology checker ***)

(*REQUIRES: inter from chapter 3*)



datatype prop = 
    Atom of string
  | Neg  of prop
  | Conj of prop * prop
  | Disj of prop * prop;

fun show (Atom a) = a
  | show (Neg p) = "(~ " ^ show p ^ ")"
  | show (Conj(p,q)) = "(" ^ show p ^ " & " ^ show q ^ ")"
  | show (Disj(p,q)) = "(" ^ show p ^ " | " ^ show q ^ ")";

(*naive version*)
fun nnf (Atom a) = Atom a
  | nnf (Neg (Atom a)) = Neg (Atom a)
  | nnf (Neg (Neg p)) = nnf p
  | nnf (Neg (Conj(p,q))) = nnf (Disj(Neg p, Neg q))
  | nnf (Neg (Disj(p,q))) = nnf (Conj(Neg p, Neg q))
  | nnf (Conj(p,q)) = Conj(nnf p, nnf q)
  | nnf (Disj(p,q)) = Disj(nnf p, nnf q);

fun nnfpos (Atom a) = Atom a
  | nnfpos (Neg p) = nnfneg p
  | nnfpos (Conj(p,q)) = Conj(nnfpos p, nnfpos q)
  | nnfpos (Disj(p,q)) = Disj(nnfpos p, nnfpos q)
and nnfneg (Atom a) = Neg (Atom a)
  | nnfneg (Neg p) = nnfpos p
  | nnfneg (Conj(p,q)) = Disj(nnfneg p, nnfneg q)
  | nnfneg (Disj(p,q)) = Conj(nnfneg p, nnfneg q);

fun distrib (p, Conj(q,r)) = Conj(distrib(p,q), distrib(p,r))
  | distrib (Conj(q,r), p) = Conj(distrib(q,p), distrib(r,p))
  | distrib (p, q) = Disj(p,q)   (*no conjunctions*) ;

fun cnf (Conj(p,q)) = Conj (cnf p, cnf q)
  | cnf (Disj(p,q)) = distrib (cnf p, cnf q)
  | cnf p = p    (*a literal*) ;

exception NonCNF;

fun positives (Atom a)      = [a]
  | positives (Neg(Atom _)) = []
  | positives (Disj(p,q))   = positives p @ positives q
  | positives  _            = raise NonCNF;

fun negatives (Atom _)      = []
  | negatives (Neg(Atom a)) = [a]
  | negatives (Disj(p,q))   = negatives p @ negatives q
  | negatives  _            = raise NonCNF;

fun taut (Conj(p,q)) = taut p andalso taut q
  | taut p = not (null (inter (positives p, negatives p)));

