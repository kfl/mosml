(**** ML Programs from Chapter 8 of

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


(*** Control structures ***)

fun impFact n =
  let val resultp = ref 1
      and ip = ref 0
  in  while !ip < n do (ip := !ip + 1;  
                        resultp := !resultp * !ip);
      !resultp
  end;

fun pFact (n, resultp) =
  let val ip = ref 0
  in  resultp := 1;    
      while !ip < n do (ip := !ip + 1;  
                        resultp := !resultp * !ip)
  end;

(*polymorphic, imperative reverse*)
fun irev l =
  let val resultp = ref []
      and lp = ref l 
  in  while not (null (!lp)) do
          (resultp := hd(!lp) :: !resultp;
           lp := tl(!lp));
      !resultp
  end;



(**** Lazy sequences using references ****)

(*Note weak type variables in cons, @, map, filter, cycle... 
  everything that builds a sequence*)
signature IMP_SEQUENCE = 
  sig
  type 'a t
  exception Empty
  val empty: 'a t
  val cons: 'a * (unit -> 'a t) -> 'a t
  val null: 'a t -> bool
  val hd: 'a t -> 'a
  val tl: 'a t -> 'a t
  val take: 'a t * int -> 'a list
  val toList: 'a t -> 'a list
  val fromList: 'a list -> 'a t
  val @ : 'a t * 'a t -> 'a t
  val interleave : 'a t * 'a t -> 'a t
  val concat: 'a t t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val filter: ('a -> bool) -> 'a t -> 'a t
  val cycle: ((unit -> 'a t) -> 'a t) -> 'a t
  end;

(*Note use of :> for abstraction*)
structure ImpSeq :> IMP_SEQUENCE =
  struct
  datatype 'a t  = Nil
		 | Cons of 'a * ('a t) ref
		 | Delayed of unit -> 'a t;

  exception Empty;

  fun delay xf = ref(Delayed xf);

  (*sequence "constructors" for export*)
  val empty = Nil;

  fun cons(x,xf) = Cons(x, delay xf);

  (*gets tail value, perhaps with the side effect of storing it*)
  fun force xp = case !xp of
                     Delayed f => let val s = f()
		      	          in  xp := s;  s  end
     	           | s => s;

  (** these functions do not expect Delayed -- it is only permissible
      in the tail of a Cons, where it is enclosed in a reference **)

  fun null Nil = true
    | null (Cons _) = false;

  fun hd Nil = raise Empty
    | hd (Cons(x,_)) = x;

  fun tl Nil = raise Empty
    | tl (Cons(_,xp)) = force xp;

  fun take (xq, 0) = []
    | take (Nil, n) = []
    | take (Cons(x,xp), n) = x :: take (force xp, n-1);

  fun toList Nil = []
    | toList (Cons(x,xp)) = x :: toList (force xp);

  fun fromList [] = Nil
    | fromList (x::xs) = cons(x, fn()=> fromList xs);

  fun Nil @ yq = yq
    | (Cons(x,xp)) @ yq =
	  Cons(x, delay(fn()=> (force xp) @ yq));

  fun interleave (Nil,    yq) = yq
    | interleave (Cons(x,xp), yq) = 
	  Cons(x, delay (fn()=> interleave(yq, force xp)));

  (*concatenate a sequence of sequences, taking care not to loop! *)
   fun concat xqq =
     if null xqq then empty
     else if null(hd xqq) then concat(tl xqq)
	  else cons(hd(hd xqq),  
		    fn()=> tl(hd xqq) @ concat(tl xqq));

  (** functionals for sequences **)
  fun map f Nil  = Nil
    | map f (Cons(x,xp)) = 
          Cons(f x, delay(fn()=> map f (force xp)));

  fun filter pred Nil = Nil
    | filter pred (Cons(x,xp)) =
	  if pred x 
          then Cons(x, delay(fn()=> filter pred (force xp)))
	  else filter pred (force xp);

  (*idea thanks to C. Reade, see Appendix 3 of his book;
    seqfn must not call its argument outside of a closure;
    lest it get Nil rather than the cycle. *)
  fun cycle seqfn =
      let val knot = ref Nil
      in  knot := seqfn (fn()=> !knot);  !knot  end;
  end;



(** Fibonacci sequence **)

fun pairs (xq,yq) = 
      ImpSeq.cons((ImpSeq.hd xq, ImpSeq.hd yq), 
                fn()=>pairs(ImpSeq.tl xq, ImpSeq.tl yq));

val add = ImpSeq.map Int.+ o pairs;

val fib = ImpSeq.cycle(fn fibf =>
    ImpSeq.cons(1, fn()=> 
         ImpSeq.cons(1, fn()=> 
              add(fibf(), ImpSeq.tl(fibf())))));

(*if it returns quickly, sharing works;
  if not shared, computation time is EXPONENTIAL in length of sequence *)
ImpSeq.take(fib,40);



(*** Ring Buffers, or Doubly Linked Lists ***)

signature RINGBUF = 
  sig
  eqtype 'a t
  exception Empty
  val empty: unit -> 'a t
  val null: 'a t -> bool
  val label: 'a t -> 'a
  val moveLeft: 'a t -> unit
  val moveRight: 'a t -> unit
  val insert: 'a t * 'a -> unit
  val delete: 'a t -> 'a
  end;

(*Note use of :> for abstraction*)
structure RingBuf :> RINGBUF =
  struct
  datatype 'a buf = Nil
		  | Node of 'a buf ref * 'a * 'a buf ref;
  datatype 'a t = Ptr of 'a buf ref;

  exception Empty;

  fun left (Node(lp,_,_)) = lp
    | left Nil = raise Empty;

  fun right (Node(_,_,rp)) = rp
    | right Nil = raise Empty;

  (*Must be a function, as each ring buffer needs a separate reference.
     Also the type checker would reject the polymorphic reference.*)
  fun empty() = Ptr(ref Nil);

  fun null (Ptr p) = case !p of Nil => true
			      | Node(_,x,_) => false;

  fun label (Ptr p) = case !p of Nil => raise Empty
			       | Node(_,x,_) => x;

  fun moveLeft (Ptr p) = (p := !(left(!p)));

  fun moveRight (Ptr p) = (p := !(right(!p)));

  (*Insert to left of the window, which is unchanged unless empty. *)
  fun insert (Ptr p, x) =
      case !p of
	  Nil => 
	      let val lp = ref Nil
		  and rp = ref Nil
		  val new = Node(lp,x,rp)
	      in  lp := new;  rp := new;  p := new  end
	| Node(lp,_,_) =>
	      let val new = Node(ref(!lp), x, ref(!p))
	      in  right(!lp) := new;  lp := new  end;

  (*Delete the current node, raising Empty if there is none.
    Observe the comparison of left refs to see whether they are identical. *)
  fun delete (Ptr p) =
      case !p of
	  Nil => raise Empty
	| Node(lp,x,rp) =>
	     (if left(!lp) = lp then p := Nil
	      else (right(!lp) := !rp;  left (!rp) := !lp;
		    p := !rp);
              x)

  end;


(**** V-arrays ****)

signature VARRAY = 
  sig
  type 'a t
  val array: int * 'a -> 'a t
  val reroot: 'a t -> 'a t
  val sub: 'a t * int -> 'a
  val justUpdate: 'a t * int * 'a -> 'a t
  val update: 'a t * int * 'a -> 'a t
  end;

(*Note use of :> for abstraction*)
structure Varray :> VARRAY =
  struct
  datatype 'a t = Modif of {limit: int, 
			   index: int ref,
			   elem: 'a ref, 
			   next: 'a t ref}
	       | Main of 'a Array.array;

  (*create a new array containing x in locations 0 to n-1. *)
  fun array (n,x) = 
	if n < 0  then  raise Size
	else  Modif{limit=n, index=ref 0, elem=ref x, 
		    next=ref(Main(Array.array(n,x)))};

  (*rerooting operation*)
  fun reroot (va as Modif{index, elem, next,...}) =
      case !next of
	 Main _ => va  (*have reached root*)
       | Modif _ => 
	   let val Modif{index=bindex,elem=belem,next=bnext,...} =
		     reroot (!next)
	       val Main ary = !bnext
	   in  bindex := !index;  
	       belem := Array.sub(ary, !index);
	       Array.update(ary, !index, !elem);
	       next := !bnext;
	       bnext := va;
	       va
	   end;

  (*subscripting*)
  fun sub (Modif{index,elem,next,...}, i) =
       case !next of
	  Main ary => Array.sub(ary,i)
	| Modif _ =>    if !index = i then !elem  
				      else sub(!next,i);

  (*plain update, no rerooting*)
  fun justUpdate(va as Modif{limit,...}, i, x) = 
	if  0<=i andalso i<limit  
	then Modif{limit=limit, index= ref i, 
		   elem=ref x, next=ref va}
	else raise Subscript;

  (*update and reroot*)
  fun update(va,i,x) = reroot(justUpdate(va,i,x));

  end;



(**** Input and Output ****)

(*** String conversions ***)

val months = ["JAN", "FEB", "MAR", "APR", "MAY", "JUN", 
	      "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"];

(*An example of using exception Bind -- forward reference from Chapter 4?*)
fun dateFromString s =
  let val sday::smon::syear::_ = String.tokens (fn c => c = #"-") s
      val SOME day = Int.fromString sday
      val mon      = String.substring (smon, 0, 3)
      val SOME year = Int.fromString syear
  in  if List.exists (fn m => m=mon) months 
      then SOME (day, mon, year)
      else NONE
  end
  handle Subscript => NONE
       | Bind      => NONE;


(*** Stream Input/Output ***)

(** Initial letters of words in each line **)

fun firstChar s = String.sub(s,0);

val initials = implode o (map firstChar) o (String.tokens Char.isSpace);

initials "My ransom is this frail and worthless trunk";

fun batchInitials (is, os) =
  while not (TextIO.endOfStream is) 
  do TextIO.output(os, initials (TextIO.inputLine is) ^ "\n");

fun promptInitials (is, os) =
  while (TextIO.output(os, "Input line? ");  TextIO.flushOut os;
	 not (TextIO.endOfStream is)) 
  do TextIO.output(os, "Initials:   " ^ initials(TextIO.inputLine is) ^ "\n");


(*** Conversion to HTML ***)

fun firstLine s =
    let val (name,rest) = 
	        Substring.splitl (fn c => c <> #".") (Substring.all s)
    in  "\n<P><EM>" ^ Substring.string name ^
	"</EM>"     ^ Substring.string rest
    end;

fun htmlCvt fileName =
    let val is = TextIO.openIn fileName
	and os = TextIO.openOut (fileName ^ ".html")
        fun cvt _ "" = ()
	  | cvt _ "\n" = cvt true (TextIO.inputLine is)
	  | cvt first s =
		(TextIO.output (os, 
				if first then firstLine s
				else "<BR>" ^ s);
		 cvt false (TextIO.inputLine is));
    in  cvt true "\n";  TextIO.closeIn is;  TextIO.closeOut os  end;



(**** Pretty printing ****)

signature PRETTY = 
  sig
   type t
   val blo : int * t list -> t
   val str : string -> t
   val brk : int -> t
   val pr  : TextIO.outstream * t * int -> unit
   end;

structure Pretty : PRETTY =
  struct
  (*Printing items: compound phrases, strings, and breaks*)
  datatype t = 
      Block of t list * int * int 	(*indentation, length*)
    | String of string
    | Break of int;			(*length*)

  (*Add the lengths of the expressions until the next Break; if no Break then
    include "after", to account for text following this block. *)
  fun breakdist (Block(_,_,len)::es, after) = len + breakdist(es, after)
    | breakdist (String s :: es, after) = size s + breakdist (es, after)
    | breakdist (Break _ :: es, after) = 0
    | breakdist ([], after) = after;

  fun pr (os, e, margin) =
   let val space = ref margin

       fun blanks n = (TextIO.output(os, StringCvt.padLeft #" " n "");  
		       space := !space - n)

       fun newline () = (TextIO.output(os,"\n");  space := margin)

       fun printing ([], _, _) = ()
	 | printing (e::es, blockspace, after) =
	  (case e of
	       Block(bes,indent,len) =>
		  printing(bes, !space-indent, breakdist(es,after))
	     | String s => (TextIO.output(os,s);   space := !space - size s)
	     | Break len => 
		 if len + breakdist(es,after) <= !space 
		 then blanks len
		 else (newline();  blanks(margin-blockspace));
	    printing (es, blockspace, after))
   in  printing([e], margin, 0);  newline()  end;

  fun length (Block(_,_,len)) = len
    | length (String s) = size s
    | length (Break len) = len;

  val str = String  and  brk = Break;

  fun blo (indent,es) =
    let fun sum([], k) = k
	  | sum(e::es, k) = sum(es, length e + k)
    in  Block(es,indent, sum(es,0))  end;
  end;



local open Pretty
  in

  fun prettyshow (Atom a) = str a
    | prettyshow (Neg p) = 
	blo(1, [str"(~", prettyshow p, str")"])
    | prettyshow (Conj(p,q)) = 
	blo(1, [str"(", prettyshow p, str" &",  
		brk 1, prettyshow q, str")"])
    | prettyshow (Disj(p,q)) =
	blo(1, [str"(", prettyshow p, str" |",  
		brk 1, prettyshow q, str")"]);

  end;
