(**** ML Programs from Chapter 10 of

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


(*TACTIC-BASED PROVER FOR CLASSICAL FIRST-ORDER LOGIC*)

(*variables  
    a,b,c: string     qnt: string (quantifier name)  
    i,j: int    (Bound indices)
    t,u: term    p,q: form
    x,y: any     f,g: functions *)

(**** Terms and Formulae ****)
signature FOL =
  sig
  datatype term = Var   of string
		| Param of string * string list
		| Bound of int
		| Fun   of string * term list
  datatype form = Pred  of string * term list
		| Conn  of string * form list
		| Quant of string * string * form
  type goal = form list * form list
  val precOf: string -> int
  val abstract: int -> term -> form -> form
  val subst: int -> term -> form -> form
  val termVars: term * string list -> string list
  val goalVars: goal * string list -> string list
  val termParams: term * (string * string list) list 
                  -> (string * string list) list
  val goalParams: goal * (string * string list) list 
                  -> (string * string list) list
  end;
  
structure Fol : FOL =
  struct
  datatype term = Var   of string
		| Param of string * string list
		| Bound of int
		| Fun   of string * term list;
  datatype form = Pred  of string * term list
		| Conn  of string * form list
		| Quant of string * string * form;

  type goal = form list * form list;

  (*Replace the term u1 by u2 throughout a term t*)
  fun replace (u1,u2) t =
      if t=u1 then u2 else
	  case t of Fun(a,ts) => Fun(a, map (replace(u1,u2)) ts)
		  | _         => t;

  (*Abstraction of a formula over the atomic term t. *)
  fun abstract i t (Pred(a,ts)) = Pred(a, map (replace (t, Bound i)) ts)
    | abstract i t (Conn(b,ps)) = Conn(b, map (abstract i t) ps)
    | abstract i t (Quant(qnt,b,p)) = Quant(qnt, b, abstract (i+1) t p);

  (*Replace (Bound i) by t in formula. t may contain no bound vars *)
  fun subst i t (Pred(a,ts)) = Pred(a, map (replace (Bound i, t)) ts)
    | subst i t (Conn(b,ps)) = Conn(b, map (subst i t) ps)
    | subst i t (Quant(qnt,b,p)) = Quant(qnt, b, subst (i+1) t p);

  (*Precedence table: used by parsing AND display! *)
  fun precOf "~"   = 4
    | precOf "&"   = 3
    | precOf "|"   = 2
    | precOf "<->" = 1
    | precOf "-->" = 1
    | precOf _     = ~1    (*means not an infix*);

  (*Accumulate a term function over all terms in a formula*)
  fun accumForm f (Pred(_,ts), z) = foldr f z ts
    | accumForm f (Conn(_,ps), z) = foldr (accumForm f) z ps
    | accumForm f (Quant(_,_,p), z) = accumForm f (p,z);

  (*Accumulate a form function over all formulae in a goal [POLYMORHPIC]*)
  fun accumGoal f ((ps,qs), z) = foldr f (foldr f z qs) ps;

  (*insertion into ordered list of strings*)
  fun insert (a,[]) = [a]
    | insert (a,b::bs) = case String.compare (a,b) of
                             LESS => a::b::bs
			   | EQUAL => b::bs
			   | GREATER => b::insert(a,bs);

  (*Accumulate all Vars in the term (except in a Param).*)
  fun termVars (Var a, bs) = insert(a,bs)
    | termVars (Fun(_,ts), bs) = foldr termVars bs ts
    | termVars (_, bs) = bs;
  val goalVars = accumGoal (accumForm termVars);

  (*Accumulate all Params*)
  fun termParams (Param(a,bs), pairs) = (a,bs) :: pairs
    | termParams (Fun(_,ts), pairs) = foldr termParams pairs ts
    | termParams (_, pairs) = pairs;
  val goalParams = accumGoal (accumForm termParams);
  end;

(**** Syntax: scanning, parsing, and display ****)

structure FolKey = 
    struct val alphas = ["ALL","EX"]
           and symbols = ["(", ")", ".", ",", "?", "~", "&", "|", 
                          "<->", "-->", "|-"]
    end;
structure FolLex = Lexical (FolKey);
structure FolParsing = Parsing (FolLex);





(*** Parsing of First-order terms & formulae ***)
signature PARSE_FOL =
  sig
  val read: string -> Fol.form
  end;

structure ParseFol : PARSE_FOL =
  struct
  local 

    open FolParsing 
    (*One or more phrases separated by commas*)
    fun list ph = ph -- repeat ("," $-- ph) >> (op::);

    (*Either (ph,...,ph) or empty. *)
    fun pack_ ph =    "(" $-- list ph -- $")" >> #1
		  || empty;

    (*Make a quantifier from parsed information*)
    fun makeQuant ((qnt,b),p) = 
	Fol.Quant(qnt, b, Fol.abstract 0 (Fol.Fun(b,[])) p);

    (*Make a connective*)
    fun makeConn a p q = Fol.Conn(a, [p,q]);
    fun makeNeg p = Fol.Conn("~", [p]);

    fun term toks =
      (   id   -- pack_ term		>> Fol.Fun
       || "?" $-- id			>> Fol.Var   ) toks;

    fun form toks =
      (   $"ALL" -- id -- "." $-- form 	>> makeQuant
       || $"EX"  -- id -- "." $-- form 	>> makeQuant
       || infixes (primary, Fol.precOf, makeConn)  ) toks
    and primary toks =
      (   "~" $-- primary      		>> makeNeg
       || "(" $-- form -- $")" 		>> #1
       || id -- pack_ term		>> Fol.Pred   )  toks;
    in
        val read = reader form
    end
  end;


(*** Pretty Printing of terms, formulae, and goals ***)

signature DISPLAY_FOL =
  sig
  val form: Fol.form -> unit
  val goal: int -> Fol.goal -> unit
  end;

structure DisplayFol : DISPLAY_FOL =
  struct
  fun enclose sexp = Pretty.blo(1, [Pretty.str"(", sexp, Pretty.str")"]); 

  fun commas [] = []
    | commas (sexp::sexps) = Pretty.str"," :: Pretty.brk 1 :: 
			     sexp :: commas sexps;

  fun list (sexp::sexps) = Pretty.blo(0, sexp :: commas sexps);  

  fun term (Fol.Param(a,_)) = Pretty.str a 
    | term (Fol.Var a) = Pretty.str ("?"^a)
    | term (Fol.Bound i) = Pretty.str "??UNMATCHED INDEX??"
    | term (Fol.Fun (a,ts)) = Pretty.blo(0, [Pretty.str a, args ts])
  and args [] = Pretty.str""
    | args ts = enclose (list (map term ts));

  (*display formula in context of operator with precedence k *)
  fun formp k (Fol.Pred (a,ts)) = Pretty.blo(0, [Pretty.str a, args ts])
    | formp k (Fol.Conn("~", [p])) = 
          Pretty.blo(0, [Pretty.str "~", formp (Fol.precOf "~") p])
    | formp k (Fol.Conn(C, [p,q])) =
	  let val pf = formp (Int.max(Fol.precOf C, k))
	      val sexp = Pretty.blo(0, [pf p, 
					Pretty.str(" "^C), 
					Pretty.brk 1, 
					pf q])
	  in  if (Fol.precOf C <= k) then (enclose sexp) else sexp
	  end
    | formp k (Fol.Quant(qnt,b,p)) =
	  let val q = Fol.subst 0 (Fol.Fun(b,[])) p
	      val sexp = Pretty.blo(2, 
				    [Pretty.str(qnt ^ " " ^ b ^ "."), 
				     Pretty.brk 1,  
				     formp 0 q])
	  in  if  k>0  then  (enclose sexp)  else sexp  end
    | formp k _ = Pretty.str"??UNKNOWN FORMULA??";

  fun formList [] = Pretty.str"empty"
    | formList ps = list (map (formp 0) ps);

  fun form p = Pretty.pr (TextIO.stdOut, formp 0 p, 50);

  fun goal (n:int) (ps,qs) = 
     Pretty.pr (TextIO.stdOut, 
		Pretty.blo (4, [Pretty.str(" " ^ Int.toString n ^  ". "), 
				formList ps,
				Pretty.brk 2,
				Pretty.str"|-  ",
				formList qs]),
	 50);
  end;


(**** UNIFICATION ****)

signature UNIFY =
  sig
  exception Failed
  val atoms: Fol.form * Fol.form -> Fol.term StringDict.t
  val instTerm: Fol.term StringDict.t -> Fol.term -> Fol.term
  val instForm: Fol.term StringDict.t -> Fol.form -> Fol.form
  val instGoal: Fol.term StringDict.t -> Fol.goal -> Fol.goal
  end;

structure Unify : UNIFY =
  struct
  exception Failed;

  (*Naive unification of terms containing no bound variables*)
  fun unifyLists env =
    let fun chase (Fol.Var a) =  (*Chase variable assignments*)
	      (chase(StringDict.lookup(env,a)) 
	       handle StringDict.E _ => Fol.Var a)
	  | chase t = t
	fun occurs a (Fol.Fun(_,ts)) = occsl a ts
	  | occurs a (Fol.Param(_,bs)) = occsl a (map Fol.Var bs)
	  | occurs a (Fol.Var b) = 
	        (a=b) orelse (occurs a (StringDict.lookup(env,b))  
			      handle StringDict.E _ => false)
	  | occurs a _ = false
	and occsl a = List.exists (occurs a)
	and unify (Fol.Var a, t) = 
	      if t = Fol.Var a then env 
              else if occurs a t then raise Failed  
	                         else StringDict.update(env,a,t)
	  | unify (t, Fol.Var a) = unify (Fol.Var a, t)
	  | unify (Fol.Param(a,_), Fol.Param(b,_)) =
	      if a=b then env  else  raise Failed
	  | unify (Fol.Fun(a,ts), Fol.Fun(b,us)) =  
	      if a=b then unifyl(ts,us) else raise Failed
	  | unify _ =  raise Failed
       and unifyl ([],[]) = env
	 | unifyl (t::ts, u::us) =
               unifyLists (unify (chase t, chase u)) (ts,us)
	 | unifyl _ =  raise Failed
      in  unifyl  end

  (*Unification of atomic formulae*)
  fun atoms (Fol.Pred(a,ts), Fol.Pred(b,us)) =  
	  if a=b then unifyLists StringDict.empty (ts,us)  
	         else raise Failed
    | atoms _ =  raise Failed;

  (*Instantiate a term by an environment*)
  fun instTerm env (Fol.Fun(a,ts)) = Fol.Fun(a, map (instTerm env) ts)
    | instTerm env (Fol.Param(a,bs)) =
	Fol.Param(a, foldr Fol.termVars [] 
		           (map (instTerm env o Fol.Var) bs))
    | instTerm env (Fol.Var a) = (instTerm env (StringDict.lookup(env,a))
			      handle StringDict.E _ => Fol.Var a)
    | instTerm env t = t;

  (*Instantiate a formula*)
  fun instForm env (Fol.Pred(a,ts))   = Fol.Pred(a, map (instTerm env) ts)
    | instForm env (Fol.Conn(b,ps))   = Fol.Conn(b, map (instForm env) ps)
    | instForm env (Fol.Quant(qnt,b,p)) = Fol.Quant(qnt, b, instForm env p);

  fun instGoal env (ps,qs) = 
        (map (instForm env) ps, map (instForm env) qs);
  end;



(**** Rules and proof states -- the abstract type "state" ****)

signature RULE =
  sig
  type state
  type tactic = state -> state ImpSeq.t
  val main: state -> Fol.form
  val subgoals: state -> Fol.goal list
  val initial: Fol.form -> state
  val final: state -> bool
  val basic: int -> tactic
  val unify: int -> tactic
  val conjL: int -> tactic
  val conjR: int -> tactic
  val disjL: int -> tactic
  val disjR: int -> tactic
  val impL: int -> tactic
  val impR: int -> tactic
  val negL: int -> tactic
  val negR: int -> tactic
  val iffL: int -> tactic
  val iffR: int -> tactic
  val allL: int -> tactic
  val allR: int -> tactic
  val exL: int -> tactic
  val exR: int -> tactic
  end;


(*Note use of :> for abstraction*)
structure Rule :> RULE =
  struct

  (*A state contains subgoals, main goal, variable counter *)
  datatype state = State of Fol.goal list * Fol.form * int;

  type tactic = state -> state ImpSeq.t;

  fun main (State(gs,p,_)) = p
  and subgoals (State(gs,p,_)) = gs;

  (*initial state has one subgoal *)
  fun initial p = State([ ([],[p]) ], p, 0);

  (*final state has no subgoals.*)
  fun final (State(gs,_,_)) = null gs;

  (*add the goals "newgs" to replace subgoal i*)
  fun spliceGoals gs newgs i = List.take(gs,i-1) @ newgs @ List.drop(gs,i);

  (*goalF maps goal -> goal list; operator makes a deterministic tactic*)
  fun propRule goalF i (State(gs,p,n)) =
      let val gs2 = spliceGoals gs (goalF (List.nth(gs,i-1))) i
      in  ImpSeq.fromList [State(gs2, p, n)]  end
      handle _ => ImpSeq.empty;

  (*for basic sequents: with identical formulae on opposite sides*)
  val basic = propRule
	       (fn (ps,qs) => 
		if List.exists (fn p => List.exists (fn q => p=q) qs) ps
		then [] else raise Match);

  (*Solves goal p|-q by unifying p with q.
    Returns list of successful environments. *)
  fun unifiable ([], _) = ImpSeq.empty
    | unifiable (p::ps, qs) = 
	let fun find [] = unifiable (ps,qs)
	      | find (q::qs) = ImpSeq.cons(Unify.atoms(p,q), fn() => find qs)
			       handle Unify.Failed => find qs
	in  find qs  end;

  fun inst env (gs,p,n) =
	State (map (Unify.instGoal env) gs,  Unify.instForm env p, n);

  (*for solvable goals with unifiable formulae on opposite sides*)
  fun unify i (State(gs,p,n)) =
    let val (ps,qs) = List.nth(gs,i-1)
	fun next env = inst env (spliceGoals gs [] i, p, n)
    in  ImpSeq.map next (unifiable(ps,qs))  end
    handle Subscript => ImpSeq.empty;


  (*** Tactics for propositional rules ***)

  (* return & delete first formula of the form Conn(a,_,_) *)
  fun splitConn a qs =
    let fun get [] = raise Match
	  | get (Fol.Conn(b,ps) :: qs) = if a=b then ps else get qs
	  | get (q::qs) = get qs;
	fun del [] = []
	  | del ((q as Fol.Conn(b,_)) :: qs) = if a=b then qs 
					       else q :: del qs
	  | del (q::qs) = q :: del qs
    in (get qs, del qs)  end;

  (*leftF transforms left-side formulae to new subgoals*)
  fun propL a leftF = propRule (fn (ps,qs) => leftF (splitConn a ps, qs));

  (*rightF transforms right-side formulae to new subgoals*)
  fun propR a rightF = propRule (fn (ps,qs) => rightF (ps, splitConn a qs));

  val conjL = propL "&" (fn (([p1,p2], ps), qs) => [(p1::p2::ps, qs)]);

  val conjR = propR "&" 
      (fn (ps, ([q1,q2], qs)) => [(ps, q1::qs),  (ps, q2::qs)]);

  val disjL = propL "|" 
      (fn (([p1,p2], ps), qs) => [(p1::ps, qs),  (p2::ps, qs)]);

  val disjR = propR "|" (fn (ps, ([q1,q2], qs)) => [(ps, q1::q2::qs)]);

  val impL = propL "-->" 
      (fn (([p1,p2], ps), qs) => [(p2::ps, qs),  (ps, p1::qs)]);

  val impR = propR "-->" (fn (ps, ([q1,q2], qs)) => [(q1::ps, q2::qs)]);

  val negL = propL "~" (fn (([p], ps), qs) => [(ps, p::qs)]);

  val negR = propR "~" (fn (ps, ([q], qs)) => [(q::ps, qs)]);

  val iffL = propL "<->" 
      (fn (([p1,p2], ps), qs) => [(p1::p2::ps, qs),  (ps, p1::p2::qs)]);

  val iffR = propR "<->"
      (fn (ps, ([q1,q2], qs)) => [(q1::ps, q2::qs),  (q2::ps, q1::qs)]);


  (*** Tactics for quantifier rules ***)

  (* return & delete first formula of the form Quant(qnt,_,_) *)
  fun splitQuant qnt qs =
    let fun get [] = raise Match
	  | get ((q as Fol.Quant(qnt2,_,p)) :: qs) = if qnt=qnt2 then q
						     else get qs
	  | get (q::qs) = get qs;
	fun del [] = []
	  | del ((q as Fol.Quant(qnt2,_,p)) :: qs) = if qnt=qnt2 then qs
						     else q :: del qs
	  | del (q::qs) = q :: del qs
    in (get qs, del qs)  end;

  fun letter n = String.substring("abcdefghijklmnopqrstuvwxyz", n, 1)

  fun gensym n = (* the "_" prevents clashes with user's variable names*)
     if n<26 then "_" ^ letter n
     else gensym(n div 26) ^ letter(n mod 26);

  fun quantRule goalF i (State(gs,p,n)) =
      let val gs2 = spliceGoals gs (goalF (List.nth(gs,i-1), gensym n)) i
      in  ImpSeq.fromList [State(gs2, p, n+1)]  end
      handle _ => ImpSeq.empty;

  val allL = quantRule (fn ((ps,qs), b) =>
      let val (qntForm as Fol.Quant(_,_,p), ps') = splitQuant "ALL" ps
	  val px = Fol.subst 0 (Fol.Var b) p
      in  [(px :: ps' @ [qntForm], qs)]  end);

  val allR = quantRule (fn ((ps,qs), b) =>
      let val (Fol.Quant(_,_,q), qs') = splitQuant "ALL" qs
	  val vars = Fol.goalVars ((ps,qs), [])
	  val qx = Fol.subst 0 (Fol.Param(b, vars)) q
      in  [(ps, qx::qs')]  end);

  val exL = quantRule (fn ((ps,qs), b) =>
      let val (Fol.Quant(_,_,p), ps') = splitQuant "EX" ps
	  val vars = Fol.goalVars ((ps,qs), [])
	  val px = Fol.subst 0 (Fol.Param(b, vars)) p
      in  [(px::ps', qs)]  end);

  val exR = quantRule (fn ((ps,qs), b) =>
      let val (qntForm as Fol.Quant(_,_,q), qs') = splitQuant "EX" qs
	  val qx = Fol.subst 0 (Fol.Var b) q
      in  [(ps, qx :: qs' @ [qntForm])]  end);

  end;


(**** Commands to modify the top-level proof state 
  Each level of goal stack includes a proof state and alternative states,
  the output of the tactic applied to the preceeding level. 
      EXERCISE: design and implement better undo facilities. ****)

signature COMMAND =
  sig
  val goal: string -> unit
  val by: Rule.tactic -> unit
  val pr: Rule.state -> unit
  val getState: unit -> Rule.state
  end;

structure Command : COMMAND =
  struct

  val currState = ref (Rule.initial (Fol.Pred("No goal yet!",[])));

  fun question (s,z) = " ?" :: s :: z;
  fun printParam (a,[]) = ()     (*print a line of parameter table*)
    | printParam (a,ts) = 
       print (String.concat (a :: " not in " :: 
			     foldr question ["\n"] ts));

  fun printGoals (_, []) = ()
    | printGoals (n, g::gs) = (DisplayFol.goal n g;  printGoals (n+1,gs));

  fun pr st =  (*print a proof state*)
      let val p  = Rule.main st  
	  and gs = Rule.subgoals st
      in  DisplayFol.form p;
	  if Rule.final st then print "No subgoals left!\n"
	  else (printGoals (1,gs);
	        app printParam (foldr Fol.goalParams [] gs))
      end;

  (*print new state, then set it*)
  fun setState state = (pr state;  currState := state);

  val goal = setState o Rule.initial o ParseFol.read;

  fun by tac = setState (ImpSeq.hd (tac (!currState)))
               handle ImpSeq.Empty => print "** Tactic FAILED! **\n"

  fun getState() = !currState;
  end;



(*** Tacticals ***)
infix 0 |@|;

signature TACTICAL =
  sig
  type ('a,'b) multifun		(*should be = 'a -> 'b ImpSeq.t*)
  val -- :   ('a,'b) multifun * ('b,'c) multifun -> ('a,'c) multifun
  val || : ('a,'b) multifun * ('a,'b) multifun -> ('a,'b) multifun
  val |@| : ('a,'b) multifun * ('a,'b) multifun -> ('a,'b) multifun
  val all : ('a,'a) multifun
  val no :  ('a,'b) multifun
  val try :    ('a,'a) multifun -> ('a,'a) multifun
  val repeat : ('a,'a) multifun -> ('a,'a) multifun
  val repeatDeterm : ('a,'a) multifun -> ('a,'a) multifun
  val depthFirst : ('a -> bool) -> ('a,'a) multifun -> ('a,'a) multifun
  val depthIter : ('a -> bool) * int -> ('a,'a) multifun -> ('a,'a) multifun
  val firstF : ('a -> ('b,'c)multifun) list -> 'a -> ('b,'c)multifun
  end;

structure Tactical : TACTICAL  =
  struct
  type ('a,'b)multifun = 'a -> 'b ImpSeq.t

  (*-- performs one tactic followed by another*)
  fun (tac1 -- tac2) x = ImpSeq.concat (ImpSeq.map tac2 (tac1 x));

  (*|| commits to the first successful tactic: no backtracking. *)
  fun (tac1 || tac2) x = 
      let val y = tac1 x 
      in  if ImpSeq.null y  then  tac2 x  else y  end;

  (*|@| combines the results of two tactics with backtracking.*)
  fun (tac1 |@| tac2) x = 
      ImpSeq.concat(ImpSeq.cons(tac1 x,  (*delay application of tac2!*)
				fn()=> ImpSeq.cons(tac2 x, 
						   fn()=> ImpSeq.empty)));

  (*accepts all states unchanged;  identity of --*)
  fun all x = ImpSeq.fromList [x];

  (*accepts no states;  identity of || and |@|*)
  fun no x = ImpSeq.empty;

  fun try tac = tac || all;

  (*Performs no backtracking: quits when it gets stuck*)
  fun repeat tac x = (tac -- repeat tac || all) x;

  fun repeatDeterm tac x = 
      let fun drep x = drep (ImpSeq.hd (tac x))
                       handle ImpSeq.Empty => x
      in  ImpSeq.fromList [drep x]  end;      

  (*Repeats again and again until "pred" reports proof tree as satisfied*)
  fun depthFirst pred tac x =
     (if pred x  then  all 
		 else  tac -- depthFirst pred tac) x;

  fun depthIter (pred,d) tac x =
   let val next = ImpSeq.toList o tac
       fun dfs i (y, sf) () = 
	    if i<0 then sf()
	    else if i<d andalso pred y
		 then ImpSeq.cons(y, foldr (dfs (i-1)) sf (next y))
	         else foldr (dfs (i-1)) sf (next y) ()
       fun deepen k = dfs k (x, fn()=> deepen (k+d)) ()
   in  deepen 0  end;

  fun orelseF (tac1, tac2) u = tac1 u || tac2 u;

  (*For combining tactic functions*)
  fun firstF ts = foldr orelseF (fn _ => no) ts;
  end;


signature TAC =
  sig
  val safeSteps: int -> Rule.tactic
  val quant: int -> Rule.tactic
  val step: int -> Rule.tactic
  val depth: Rule.tactic
  val depthIt: int -> Rule.tactic
  end;

structure Tac : TAC =
  struct
  local open Tactical Rule
    in
    (*Deterministic; applies one rule to the goal; no unification or variable
      instantiation; cannot render the goal unprovable.*)
    val safe =
          firstF [basic, 
		  conjL, disjR, impR, negL, negR, exL, allR, (*1 subgoal*) 
                  conjR, disjL, impL, iffL, iffR (*2 subgoals*)];

    fun safeSteps i = safe i -- repeatDeterm (safe i);

    (*expand a quantifier on the left and the right (if possible!) *)
    fun quant i = (allL i -- try (exR i)) || exR i;

    val depth = depthFirst final (safeSteps 1 || unify 1 || quant 1);

    fun step i = safeSteps i || (unify i |@| allL i |@| exR i);

    fun depthIt d = depthIter (final, d) (step 1);
    end
  end;


