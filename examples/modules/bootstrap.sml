(* Example: implementing Chris Okasaki's "bootstrapped heaps"
     
   The example uses higher-order functors, applicative functors and
   recursive structures and signatures to good effect.

   Most of the action occurs in the definition of the functor
   "Bootstrap".

       
 * This code is adapted from:
 *
 *   Purely Functional Data Structures
 *   Chris Okasaki
 *   Cambridge University Press, 1998
 *
 * Copyright (c) 1998 Cambridge University Press

*)

signature ORDERED =
sig
  type T
  val leq : T * T -> bool 
end
;

signature HEAP =
sig
  structure Elem : ORDERED

  type Heap

  val empty     : Heap
  val isEmpty   : Heap -> bool

  val insert    : Elem.T * Heap -> Heap
  val merge     : Heap * Heap -> Heap

  exception Empty;
  val findMin   : Heap -> Elem.T   (* raises Empty if heap is empty *)
  val deleteMin : Heap -> Heap     (* raises Empty if heap is empty *)
end
;

(* "Bootstrap" is a higher-order functor *)
functor Bootstrap (GenHeap : functor (Element : ORDERED) ->
                             HEAP where type Elem.T = Element.T)
                   (Element : ORDERED) : HEAP =
struct
  (* "AppHeap" is a local, applicative version of 
      the generative functor "GenHeap"
  *)
  functor AppHeap Elem : ORDERED = GenHeap Elem 
 
  structure Elem = Element

  signature Rec =  (* "Rec" is a recursive signature *)
      rec (X:sig structure BootstrappedElem : ORDERED end)
      sig
	  structure PrimH :
	      sig type Heap = PrimH.Heap 
		              where PrimH = AppHeap(X.BootstrappedElem)
                              (* local type projection *)
	      end
	  structure BootstrappedElem:
	      sig datatype T = E | H of Elem.T * PrimH.Heap
		  val leq : T * T -> bool 
	      end
      end;

  structure Rec =
      rec (X:Rec) (* a recursive structure *)
      struct structure BootstrappedElem =
		  struct
		      datatype T = datatype X.BootstrappedElem.T
		      fun leq (H (x, _), H (y, _))= Elem.leq (x, y)
		  end
	      structure PrimH = AppHeap (BootstrappedElem)
      end

  open Rec
  open BootstrappedElem  (* expose E and H constructors *)

  type Heap = BootstrappedElem.T

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (E, h) = h
    | merge (h, E) = h
    | merge (h1 as H (x, p1), h2 as H (y, p2)) =
        if Elem.leq (x, y) 
	    then H (x, PrimH.insert (h2, p1))
        else H (y, PrimH.insert (h1, p2))
  fun insert (x, h) = merge (H (x, PrimH.empty), h)

  exception Empty

  fun findMin E = raise Empty
    | findMin (H (x, _)) = x
  fun deleteMin E = raise Empty
    | deleteMin (H (x, p)) =
        if PrimH.isEmpty p 
	    then E
        else let val (H (y, p1)) = PrimH.findMin p
                 val p2 = PrimH.deleteMin p
             in 
		 H (y, PrimH.merge (p1, p2)) 
	     end 
end;


(* an example *)

functor PairingHeap(Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Heap = E | T of Elem.T * Heap list

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge (h, E) = h
    | merge (E, h) = h
    | merge (h1 as T (x, hs1), h2 as T (y, hs2)) =
        if Elem.leq (x, y) then T (x, h2 :: hs1) else T (y, h1 :: hs2)
  fun insert (x, h) = merge (T (x, []), h)

  fun mergePairs [] = E
    | mergePairs [h] = h
    | mergePairs (h1 :: h2 :: hs) = merge (merge (h1, h2), mergePairs hs)

  exception Empty
  fun findMin E = raise Empty
    | findMin (T (x, hs)) = x
  fun deleteMin E = raise Empty
    | deleteMin (T (x, hs)) = mergePairs hs
end;


structure IntElem = 
struct
  type T = int
  val leq = op  <= : (int*int)->bool;
end;
   
structure IntHeap = Bootstrap(PairingHeap)(IntElem);

(* tests *)

fun mkHeap [] = IntHeap.empty
  | mkHeap (h::t) = IntHeap.insert(h,mkHeap t);

val heap = mkHeap[6,7,8,9,1,2,3,4,5];
val test1 = (1 = IntHeap.findMin(heap));
val test2 = (2 = IntHeap.findMin(IntHeap.deleteMin(heap)));



(* another example *)

functor SkewBinomialHeap (Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of int * Elem.T * Elem.T list * Tree list
  type Heap = Tree list

  val empty = []
  fun isEmpty ts = null ts

  fun rank (Node (r, x, xs, c)) = r
  fun root (Node (r, x, xs, c)) = x
  fun link (t1 as Node (r, x1, xs1, c1), t2 as Node (_, x2, xs2, c2)) =
        if Elem.leq (x1, x2) then Node (r+1, x1, xs1, t2 :: c1)
        else Node (r+1, x2, xs2, t1 :: c2)
  fun skewLink (x, t1, t2) =
        let val Node (r, y, ys, c) = link (t1, t2)
	in
	    if Elem.leq (x, y) then Node (r, x, y :: ys, c)
	    else Node (r, y, x :: ys, c)
        end
  fun insTree (t, []) = [t]
    | insTree (t1, t2 :: ts) =
        if rank t1 < rank t2 then t1 :: t2 :: ts 
        else insTree (link (t1, t2), ts)
  fun mergeTrees (ts1, []) = ts1
    | mergeTrees ([], ts2) = ts2
    | mergeTrees (ts1 as t1 :: ts1', ts2 as t2 :: ts2') =
        if rank t1 < rank t2 then t1 :: mergeTrees (ts1', ts2)
        else if rank t2 < rank t1 then t2 :: mergeTrees (ts1, ts2')
        else insTree (link (t1, t2), mergeTrees (ts1', ts2'))
  fun normalize [] = []
    | normalize (t :: ts) = insTree (t, ts)

  fun insert (x, ts as t1 :: t2 :: rest) =
        if rank t1 = rank t2 then skewLink (x, t1, t2) :: rest
        else Node (0, x, [], []) :: ts
    | insert (x, ts) = Node (0, x, [], []) :: ts
  fun merge (ts1, ts2) = mergeTrees (normalize ts1, normalize ts2)

  exception Empty

  fun removeMinTree [] = raise Empty
    | removeMinTree [t] = (t, [])
    | removeMinTree (t :: ts) =
        let val (t', ts') = removeMinTree ts
	in if Elem.leq (root t, root t') then (t, ts) else (t', t :: ts') end


  fun findMin ts = let val (t, _) = removeMinTree ts in root t end
  fun deleteMin ts =
        let val (Node (_, x, xs, ts1), ts2) = removeMinTree ts
	    fun insertAll ([], ts) = ts
	      | insertAll (x :: xs, ts) = insertAll (xs, insert (x, ts))
	in insertAll (xs, merge (rev ts1, ts2)) end
end


structure SkewHeap = Bootstrap(SkewBinomialHeap)(IntElem);

(* tests *)

fun mkHeap [] = SkewHeap.empty
  | mkHeap (h::t) = SkewHeap.insert(h,mkHeap t);

val heap = mkHeap[6,7,8,9,1,2,3,4,5];
val test1 = (1 = SkewHeap.findMin(heap));
val test2 = (2 = SkewHeap.findMin(SkewHeap.deleteMin(heap)));











