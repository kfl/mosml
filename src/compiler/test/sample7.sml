(**** ML Programs from Chapter 7 of

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

(**** Structures ****)

(*** Three representations of queues ***)

(** Queues as lists **)
structure Queue1 = 
  struct
  type 'a t = 'a list;
  exception E;

  val empty = [];
  fun enq(q,x) = q @ [x];

  fun null(x::q) = false
    | null _ = true;

  fun hd(x::q) = x
    | hd [] = raise E;

  fun deq(x::q) = q
    | deq [] = raise E;
  end;


(** Queues as a new datatype **)
structure Queue2 = 
  struct
  datatype 'a t = empty 
		| enq of 'a t * 'a;
  exception E;

  fun null (enq _) = false
    | null empty = true;

  fun hd (enq(empty,x)) = x
    | hd (enq(q,x)) = hd q
    | hd empty = raise E;

  fun deq (enq(empty,x)) = empty
    | deq (enq(q,x)) = enq(deq q, x)
    | deq empty = raise E;
  end;


(*Applicative queues represented by (heads, reversed tails).*)
structure Queue3 = 
  struct
  datatype 'a t = Queue of ('a list * 'a list);
  exception E;

  val empty = Queue([],[]);

  (*Normalized queue, if nonempty, has nonempty heads list*)
  fun norm (Queue([],tails)) = Queue(rev tails, [])
    | norm q = q;

  (*norm has an effect if input queue is empty*)
  fun enq(Queue(heads,tails), x) = norm(Queue(heads, x::tails));

  fun null(Queue([],[])) = true
    | null _ = false;

  fun hd(Queue(x::_,_)) = x
    | hd(Queue([],_)) = raise E;

  (*normalize in case heads become empty*)
  fun deq(Queue(x::heads,tails)) = norm(Queue(heads,tails))
    | deq(Queue([],_)) = raise E;
  end;


signature QUEUE = 
  sig
  type 'a t			(*type of queues*)
  exception E			(*for errors in hd, deq*)
  val empty: 'a t		(*the empty queue*)
  val enq: 'a t * 'a -> 'a t    (*add to end*)
  val null: 'a t -> bool	(*test for empty queue*)
  val hd: 'a t -> 'a		(*return front element*)
  val deq: 'a t -> 'a t		(*remove element from front*)
  end;

structure Queue : QUEUE = 
  struct
  abstype 'a t = Queue of ('a list * 'a list)
    with
    exception E;

    val empty = Queue([],[]);

    (*Normalized queue, if nonempty, has nonempty heads list*)
    fun norm (Queue([],tails)) = Queue(rev tails, [])
      | norm q = q;

    (*norm has an effect if input queue is empty*)
    fun enq(Queue(heads,tails), x) = norm(Queue(heads, x::tails));

    fun null(Queue([],[])) = true
      | null _ = false;

    fun hd(Queue(x::_,_)) = x
      | hd(Queue([],_)) = raise E;

    (*normalize in case heads become empty*)
    fun deq(Queue(x::heads,tails)) = norm(Queue(heads,tails))
      | deq(Queue([],_)) = raise E;
    end
  end;



(*** Abstype declarations ***)


(** Queues as lists **)
abstype 'a queue1 = Q1 of 'a list
  with
  val empty = Q1 [];

  fun enq(Q1 q, x) = Q1 (q @ [x]);

  fun qnull(Q1(x::q)) = false
    | qnull _ = true;

  fun qhd(Q1(x::q)) = x;

  fun deq(Q1(x::q)) = Q1 q;
  end;


(** Queues as a new datatype **)
abstype 'a queue2 = Empty 
		  | Enq of 'a queue2 * 'a
  with
  val empty = Empty
  and enq   = Enq

  fun qnull (Enq _) = false
    | qnull Empty = true;

  fun qhd (Enq(Empty,x)) = x
    | qhd (Enq(q,x)) = qhd q;

  fun deq (Enq(Empty,x)) = Empty
    | deq (Enq(q,x)) = Enq(deq q, x);
  end;



(**** Functors ****)

(** Testing/benchmarking of queues **)

functor TestQueue (Q: QUEUE) =
  struct
  fun fromList l = foldl (fn (x,q) => Q.enq(q,x)) Q.empty l;
  fun toList q = if Q.null q then []
                 else Q.hd q :: toList (Q.deq q);
  end;


(** Queues and Breadth-First Search **)

functor BreadthFirst (Q: QUEUE) =
  struct
  fun enqlist q xs = foldl (fn (x,q) => Q.enq(q,x)) q xs;

  fun search next x =
    let fun bfs q =
	  if Q.null q then Nil
	  else let val y = Q.hd q
	       in  Cons(y, fn()=> bfs (enqlist (Q.deq q) (next y)))
	       end
    in  bfs (Q.enq(Q.empty, x))  end;
  end;

structure Breadth = BreadthFirst (Queue);
fun brQueen n = Seq.filter (isFull n) (Breadth.search (nextQueen n) []);

structure Breadth1 = BreadthFirst (Queue1);
fun brQueen1 n = Seq.filter (isFull n) (Breadth1.search (nextQueen n) []);



(*** Matrix operations ***)

signature ZSP =
   sig
   type t
   val zero : t
   val sum  : t * t -> t
   val prod : t * t -> t
   end;


functor MatrixZSP (Z: ZSP) : ZSP =
  struct

  type t = Z.t list list;

  val zero = [];

  fun sum (rowsA,[]) = rowsA
    | sum ([],rowsB) = rowsB
    | sum (rowsA,rowsB) = ListPair.map (ListPair.map Z.sum) (rowsA,rowsB);

  fun dotprod pairs = foldl Z.sum Z.zero (ListPair.map Z.prod pairs);

  fun transp ([]::_) = []
    | transp rows = map hd rows :: transp (map tl rows);

  fun prod (rowsA,[]) = []
    | prod (rowsA,rowsB) = 
	let val colsB = transp rowsB 
	in  map (fn row => map (fn col => dotprod(row,col)) colsB) rowsA
	end;

  end;


structure IntZSP =
  struct
  type t = int;
  val zero = 0;
  fun sum   (x,y) = x+y: t;
  fun prod  (x,y) = x*y: t;
  end;

structure BoolZSP =
  struct
  type t = bool;
  val zero = false;
  fun sum   (x,y) = x orelse y;
  fun prod  (x,y) = x andalso y;
  end;

(** All-Pairs Shortest Paths -- Chapter 26 of Cormen et al. **)

(*Requires a maximum integer for INFINITY*)
structure PathZSP =
  struct
  type t = int;
  val SOME zero = Int.maxInt;
  val sum  = Int.min
  fun prod(m,n) = if m=zero orelse n=zero then zero
                  else m+n;
  end;

structure PathMatrix = MatrixZSP (PathZSP);

fun fast_paths mat =
  let val n = length mat
      fun f (m,mat) = if n-1 <= m then mat
                       else f(2*m, PathMatrix.prod(mat,mat))
  in  f (1, mat)  end;

val zz = PathZSP.zero;

val mat = [[0,  3,  8,  zz, ~4], 
	   [zz, 0,  zz, 1,   7], 
	   [zz, 4,  0,  zz,  zz], 
	   [2,  zz, ~5, 0,   zz], 
	   [zz, zz, zz, 6,   0]];




(*** Dictionaries -- as a functor ***)

(** Linearly ordered types **)

signature ORDER = 
  sig
  type t
  val compare: t*t -> order
  end;

structure StringOrder: ORDER =
  struct
  type t = string;
  val compare = String.compare
  end;


functor Dictionary (Key: ORDER) : DICTIONARY = 
  struct

  type key = Key.t;

  abstype 'a t = Leaf
               | Bran of key * 'a * 'a t * 'a t
    with

    exception E of key;

    val empty = Leaf;

    fun lookup (Bran(a,x,t1,t2), b) =
          (case Key.compare(a,b) of
               GREATER => lookup(t1, b)
             | EQUAL   => x
             | LESS    => lookup(t2, b))
      | lookup (Leaf, b) = raise E b;

    fun insert (Leaf, b, y) = Bran(b, y, Leaf, Leaf)
      | insert (Bran(a,x,t1,t2), b, y) =
          (case Key.compare(a,b) of
               GREATER => Bran(a, x, insert(t1,b,y), t2)
             | EQUAL   => raise E b
             | LESS    => Bran(a, x, t1, insert(t2,b,y)));

    fun update (Leaf, b, y) = Bran(b, y, Leaf, Leaf)
      | update (Bran(a,x,t1,t2), b, y) =
          (case Key.compare(a,b) of
               GREATER => Bran(a, x, update(t1,b,y), t2)
             | EQUAL   => Bran(a, y, t1, t2)
             | LESS    => Bran(a, x, t1, update(t2,b,y)));
    end
  end;


(*This instance is required by sample9.sml and sample10.sml*)
structure StringDict = Dictionary (StringOrder);


(*Differs from the other PRIORITY_QUEUE by having a substructure*)
signature PRIORITY_QUEUE =
  sig
  structure Item : ORDER
  type t
  val empty    : t
  val null     : t -> bool
  val insert   : Item.t * t -> t
  val min      : t -> Item.t
  val delmin   : t -> t
  val fromList : Item.t list -> t
  val toList   : t -> Item.t list
  val sort     : Item.t list -> Item.t list
  end;



(**** Building large systems using modules ****)

(** Association list implementation of Dictionaries
     Illustrates eqtype and functor syntax **)

functor AssocList (eqtype key) : DICTIONARY = 
  struct
  type key = key;
  type 'a t = (key * 'a) list;
  exception E of key;

  val empty = [];

  fun lookup ((a,x)::pairs, b) =
	if a=b then  x  else  lookup(pairs, b)
    | lookup ([], b) = raise E b;

  fun insert ((a,x)::pairs, b, y) =
	if a=b then  raise E b  else  (a,x)::insert(pairs, b, y)
    | insert ([], b, y) = [(b,y)];

  fun update (pairs, b, y) = (b,y)::pairs;

  end;


signature TREE = 
  sig
  datatype 'a tree = Lf  |  Br of 'a * 'a tree * 'a tree
  val size: 'a tree -> int
  val depth: 'a tree -> int
  val reflect: 'a tree -> 'a tree
  end;

signature BRAUN = 
  sig
  structure Tree: TREE
  val sub:    'a Tree.tree * int -> 'a
  val update: 'a Tree.tree * int * 'a -> 'a Tree.tree
  val delete: 'a Tree.tree * int -> 'a Tree.tree
  val loext:  'a Tree.tree * 'a -> 'a Tree.tree
  val lorem:  'a Tree.tree -> 'a Tree.tree
  end;

functor PriorityQueue(structure Item: ORDER
                      and       Tree: TREE) : PRIORITY_QUEUE = 
  let open Tree 
  in
    struct
    structure Item = Item;
    fun x <= y = (Item.compare(x,y) <> GREATER);

    abstype t = PQ of Item.t tree
    with

      val empty = PQ Lf;

      fun null (PQ Lf) = true
	| null  _      = false;

      fun min (PQ(Br(v,_,_))) = v;

      fun insert'(w, Lf) = Br(w, Lf, Lf)
	| insert'(w, Br(v, t1, t2)) =
	    if w <= v then Br(w, insert'(v, t2), t1)
		      else Br(v, insert'(w, t2), t1);

      fun insert (w, PQ t) = PQ (insert' (w, t));

      fun leftrem (Br(v,Lf,_)) = (v, Lf)
	| leftrem (Br(v,t1,t2)) = 
	    let val (w, t) = leftrem t1
	    in  (w, Br(v,t2,t))  end;

      fun siftdown (w, Lf, _) = Br(w,Lf,Lf)
	| siftdown (w, t as Br(v,_,_), Lf) =
	    if w <= v then Br(w, t, Lf)
		      else Br(v, Br(w,Lf,Lf), Lf)
	| siftdown (w, t1 as Br(v1,p1,q1), t2 as Br(v2,p2,q2)) =
	    if w <= v1 andalso w <= v2 then Br(w,t1,t2)
	    else if v1 <= v2 then Br(v1, siftdown(w,p1,q1), t2)
	       (* v2 < v1 *) else Br(v2, t1, siftdown(w,p2,q2));

      fun delmin (PQ Lf) = raise Size
	| delmin (PQ (Br(v,Lf,_))) = PQ Lf
	| delmin (PQ (Br(v,t1,t2))) = 
	    let val (w, t) = leftrem t1
	    in  PQ (siftdown (w,t2,t))  end;

      fun heapify (0, vs) = (Lf, vs)
	| heapify (n, v::vs) =
	    let val (t1, vs1) = heapify (n div 2, vs)
		val (t2, vs2) = heapify ((n-1) div 2, vs1)
	    in  (siftdown (v,t1,t2), vs2)  end;

      fun fromList vs = PQ (#1 (heapify (length vs, vs)));

      fun toList (d as PQ (Br(v,_,_))) = v :: toList(delmin d)
	| toList _ = [];

      fun sort vs = toList (fromList vs);

      end
    end
  end;


functor FlexArray (Braun: BRAUN) : FLEXARRAY = 
  struct
  datatype 'a array = Array of 'a Braun.Tree.tree * int;

  val empty = Array(Braun.Tree.Lf,0);

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


functor BraunFunctor (Tree: TREE) : BRAUN = 
  let open Tree in
    struct
    structure Tree = Tree;

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
      | lorem (Br(_,Lf,_)) = Lf 	(*No evens, therefore no odds either*)
      | lorem (Br(_, t1 as Br(v,_,_), t2)) = Br(v, t2, lorem t1);

    end
  end;


functor TreeFunctor () : TREE =
  struct
  datatype 'a tree = Lf  |  Br of 'a * 'a tree * 'a tree;

  fun size Lf = 0
    | size (Br(v,t1,t2)) =  1 + size t1 + size t2;

  fun depth Lf = 0
    | depth (Br(v,t1,t2)) =  1 + Int.max (depth t1, depth t2);

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


(** Application of the functors **)

structure Tree = TreeFunctor();
structure Braun = BraunFunctor (Tree);
structure Flex = FlexArray (Braun);
structure StringPQueue = 
   PriorityQueue(structure Item = StringOrder 
                 and       Tree = Tree);
