(* Match.sml : Compile matches to decision trees, then to lambda code
   1996-07-09, 1997-02-03, 2000-02-16

   See P. Sestoft: ML pattern match compilation and partial
   evaluation.  In Danvy, Glück, and Thiemann (editors): Dagstuhl
   Seminar on Partial Evaluation, February 1996.  Lecture Notes in
   Computer Science 1110, pages 446-464.  Springer-Verlag 1996.
   ftp://ftp.dina.kvl.dk/pub/Staff/Peter.Sestoft/papers/match.ps.gz

   Some day the distinction between static and dynamic excons should
   be eradicated from mosml; this would lead to some simplification in
   the match compiler and the back-end.  It finally happened in
   January 2000.
 *)

open Asynt Lambda

fun splitPath n obj =
  let fun loop i oargs =
            if i < 0 then oargs else
            loop (i-1) (Lprim(Prim.Pfield i, [obj]) :: oargs)
  in loop (n-1) [] end;

(* To skip type constraints and aliases, and translate exnname accesses *)

(* cvr: TODO revise:
      translating a long exception name under 
      the pattern matching function can lead to a space leak --- 
      do it eagerly! 
*)
  
fun mkExnPat loc env ii arg = 
    let val exnname = Tr_env.translateExName env ii
    in RECpat(ref (TUPLErp [(loc, EXNAMEpat exnname), arg])) end

fun simplifyPat env (loc, pat') =
    case pat' of
	VARpat _         => WILDCARDpat
      | REFpat p         => RECpat(ref (TUPLErp [p]))
      | PARpat p         => simplifyPat env p
      | TYPEDpat(p,_)    => simplifyPat env p
      | LAYEREDpat(_, p) => simplifyPat env p
      | EXNILpat ii      => mkExnPat loc env ii (loc, WILDCARDpat)
      | EXCONSpat(ii, p) => mkExnPat loc env ii p 
      | _                => pat';

(* Constructors *)

datatype con = 
    SCon of Const.SCon
  | Tup of int				(* arity                *)
  | Vec of int				(* matching tag = arity *)
  | CCon of Const.BlockTag * int	(* arity                *)
  | EExn of Lambda.Lambda		(* dynamic excon        *)

fun span (SCon (Const.CHARscon _))         = 256
  | span (SCon _)                          = 0	   (* infinity *)
  | span (Tup _)                           = 1
  | span (Vec _)                           = 0	   (* infinity *)
  | span (CCon (Const.CONtag(_, span), _)) = span
  | span (EExn _)                          = 0	   (* infinity *)

fun arity (SCon _)          = 0
  | arity (Tup arity)       = arity
  | arity (Vec arity)       = arity
  | arity (CCon (_, arity)) = arity
  | arity (EExn _)          = 0

(* Term descriptions *)

datatype termd =
    Pos of con * termd list                (* All arguments in proper order *)
  | Neg of con list                        (* No duplicates                 *)

val Bot = Neg []                           (* The absence of information    *)

fun bots n = List.tabulate(n, fn _ => Bot)

(* Contexts, or inside-out partial term descriptions:
 * Example: The context [(c2, [a2, a1]), (c1, [b2, b1])] represents
 * a term description with a hole, of the form
 *           c1(b1, b2, c1(a1, a2, Bot, ..., Bot), Bot, ..., Bot) 
 * where the number of Bots is determined by the arity of c1 and c2.
 *) 

type context = (con * termd list) list

(* Static matching *)

datatype matchresult = Yes | No | Maybe

fun staticmatch pcon (Pos(scon, _)) = 
    if pcon = scon then Yes 
    else (case pcon of
	      EExn _ => Maybe	(* Different excons may have same name *)
	    | _      => No)
  | staticmatch pcon (Neg nonset)   =
    if Fnlib.member pcon nonset then 
        No
    else if span pcon = 1 + List.length nonset then 
        Yes
    else 
        Maybe

(* Managing partial terms and contexts *)

fun addneg (Neg nonset) con = Neg(con :: nonset)
  | addneg dsc            _ = dsc

fun apply []                  dsc = []
  | apply ((con, args)::rest) dsc = 
    if arity con = List.length args + 1 then 
        apply rest (Pos(con, List.rev(dsc :: args)))
    else
        (con, dsc :: args) :: rest

fun revappend []      res = res
  | revappend (x::xr) res = revappend xr (x::res)

fun builddsc []                  dsc []                      = dsc
  | builddsc ((con, args)::rest) dsc ((_, _, sargs) :: work) = 
    builddsc rest (Pos(con, revappend args (dsc :: sargs))) work
  | builddsc _                   _   _ = Fnlib.fatalError "Match.builddsc"

(* Runtime data access and matching actions *)

type access = Lambda.Lambda

datatype dec =
    Failure
  | Success of Lambda			(* right-hand side *)
  | IfEq of access * con * decision * decision
withtype decision = 
    {tree : dec, refs : int ref, lamRef : Lambda option ref} ref

fun shared (ref {refs as ref count, ...}   : decision) = count > 1
fun used   (ref {refs as ref count, ...}   : decision) = count > 0
fun incrnode (ref {refs as ref count, ...} : decision) = refs := 1 + count
fun mkDecision t = ref {tree = t, refs = ref 0, lamRef = ref NONE}


(* Hash-consing, to get a decision dag rather than a decision tree *)

val table = Hasht.new 37 : (dec, decision) Hasht.t

fun unique (node as IfEq(_, _, t1, t2)) = 
    if t1 = t2 then t1
    else (Hasht.find table node
	  handle Subscript => 
	      let val rnode = mkDecision node
	      in 
		  incrnode t1; incrnode t2; 
		  Hasht.insert table node rnode;
		  rnode
	      end)
  | unique _ = Fnlib.fatalError "Match.unique";

fun makedag env failure ([] : (Asynt.Pat list * decision) list) : decision = 
    Fnlib.fatalError "Match.makedag: no rules"
  | makedag env failure (allmrules as (pats1, _) :: _) = 
let 
val noOfPats = List.length pats1
val objs1 = List.rev (List.tabulate(noOfPats, Lvar))

val topCon = Tup noOfPats		(* Hack to handle top-level pat list *)
val topctx = [(topCon, [])] : context

fun fail _              []                          = failure
  | fail (Pos(_, dscs)) ((pats1, rhs1) :: rulerest) =
    succeed topctx [(pats1, objs1, dscs)] rhs1 rulerest
  | fail _ _ = Fnlib.fatalError "Match.fail"

and succeed ctx []                rhs rules = rhs
  | succeed ctx (work1::workrest) rhs rules = 
    case work1 of 
	([], [], []) => succeed ctx workrest rhs rules
      | (pat1::patrest, obj1::objrest, dsc1::dscrest) =>
	    match pat1 obj1 dsc1 ctx 
	    ((patrest, objrest, dscrest) :: workrest) rhs rules
      | _ => Fnlib.fatalError "Match.succeed"

and mktest pcon obj dsc ctx work rhs rules conequal =
    case staticmatch pcon dsc of
	Yes   => conequal dsc
      | No    => fail (builddsc ctx dsc work) rules
      | Maybe => 
	    unique(IfEq(obj, pcon, 
			conequal (Pos(pcon, bots (arity pcon))),
			fail (builddsc ctx (addneg dsc pcon) work) rules))

and match pat obj dsc ctx work rhs rules =
    case simplifyPat env pat of
	SCONpat (scon, _) => 
	    let fun conequal newdsc = 
		succeed (apply ctx newdsc) work rhs rules
	    in mktest (SCon scon) obj dsc ctx work rhs rules conequal end

      | VECpat pats =>
	    let val arity = List.length pats
		val pcon = Vec arity
		fun getsargs (Neg _)           = bots arity
		  | getsargs (Pos(con, sargs)) = sargs
		fun conequal newdsc =
		    case pats of
			[] => succeed (apply ctx newdsc) work rhs rules
		      | _  => succeed ((pcon, []) :: ctx) 
			              ((pats, splitPath arity obj, getsargs dsc) 
				       :: work)
				      rhs rules
	    in 
		mktest pcon (Lprim(Prim.Pvectlength, [obj])) dsc ctx work rhs 
		       rules conequal
	    end

      | WILDCARDpat =>
	    succeed (apply ctx dsc) work rhs rules
	    
      | NILpat ii =>
	    let val ci = !(Asyntfn.getConInfo ii)
		val pcon = CCon(Const.CONtag(#conTag ci, #conSpan ci), 0)
		fun conequal newdsc = 
		    succeed (apply ctx newdsc) work rhs rules
	    in mktest pcon obj dsc ctx work rhs rules conequal end

      | CONSpat (ii, pat) =>
	    let val ci = !(Asyntfn.getConInfo ii)
		val pcon = CCon(Const.CONtag(#conTag ci, #conSpan ci), 1)
		val oarg = if #conIsGreedy ci orelse #conSpan ci = 1 then obj
			   else Lprim(Prim.Pfield 0, [obj])
		fun getsargs (Neg _)           = [ Bot ]
		  | getsargs (Pos(con, sargs)) = sargs
		fun conequal newdsc =
		    succeed ((pcon, []) :: ctx) 
		            (([pat], [oarg], getsargs dsc) :: work)
			    rhs rules
	    in mktest pcon obj dsc ctx work rhs rules conequal end

      | EXNILpat ii         => Fnlib.fatalError "match EXNILpat"
      | EXCONSpat (ii, pat) => Fnlib.fatalError "match EXCONSpat"
      | EXNAMEpat ii =>
	    let fun conequal newdsc = 
		    succeed (apply ctx newdsc) work rhs rules
	    in mktest (EExn ii) obj dsc ctx work rhs rules conequal end

      | RECpat(ref (TUPLErp [])) =>	(* The irrefutable pattern () or {} *)
	    succeed (apply ctx dsc) work rhs rules

      | RECpat(ref (TUPLErp pats)) =>
	    let val arity = List.length pats
		val sargs = case dsc of 
		                  Neg _         => bots arity
				| Pos(_, sargs) => sargs
	    in 
		succeed ((Tup arity, []) :: ctx)
		        ((pats, splitPath arity obj, sargs) :: work) 
			rhs rules
	    end

      | RECpat(ref (RECrp _)) => Fnlib.fatalError "match 1"
      | _                     => Fnlib.fatalError "match 2"
in 
    fail (Pos(topCon, bots noOfPats)) allmrules
end

(* Switchify and compile decision nodes to Lambda-code.  Each shared
 * subdag is compiled once, to a Lambda.Lshared.  *)

fun tolambda (ref {tree, ...} : decision) (failLam : Lambda) : Lambda =
    let fun getSCon (SCon scon)      = scon
          | getSCon _                = Fnlib.fatalError "Match.getSCon"
	fun getCCon (CCon (ccon, _)) = ccon
          | getCCon _                = Fnlib.fatalError "Match.getCCon"
	fun getVec (Vec n)           = Const.INTscon n
	  | getVec _                 = Fnlib.fatalError "Match.getVec"

	fun collect getcon last cases
		 (otherwise as 
		  ref {tree = IfEq(obj, con, thenact, elseact), ...}) =
	    if obj = last andalso not (shared otherwise) then 
		collect getcon last ((getcon con, thenact) :: cases) elseact
	    else
		(cases, otherwise) 
	  | collect _ _ cases otherwise = 
		(cases, otherwise)

	fun revmap f xys = 
	    let fun loop []            res = res
		  | loop ((x, y)::xyr) res = loop xyr ((x, f y) :: res)
	    in loop xys [] end

	fun toseq Failure       = failLam
	  | toseq (Success rhs) = rhs
	  | toseq t = mkSwitch t	

	and share (node as ref {tree, lamRef as ref lamOpt, ...}) =
	    if shared node then
		case lamOpt of
		    NONE     => let val lam = shared_lambda (toseq tree)
				in lamRef := SOME lam; lam end
		  | SOME lam => lam
	    else 
		toseq tree

	and mkSwitch (IfEq(obj, SCon scon, thenact, elseact)) = 
	    let val (cases, otherwise) = collect getSCon obj [] elseact 
	    in 
		Lstatichandle(Lcase(obj, (scon, share thenact)
				         :: revmap share cases),
			      share otherwise)
	    end
	  | mkSwitch (IfEq(obj, con as Vec _, thenact, elseact)) = 
	    let val (cases, otherwise) = collect getVec obj [] elseact 
	    in 
		Lstatichandle(Lcase(obj, (getVec con, share thenact)
                                         :: revmap share cases),
			      share otherwise)
	    end

	  | mkSwitch (IfEq(obj, con as CCon _, thenact, elseact)) = 
	    let val (cases, otherwise) = collect getCCon obj [] elseact 
	    in 
		Lstatichandle(Lswitch(span con, obj, 
				      (getCCon con, share thenact)
				      ::revmap share cases),
			      share otherwise)
	    end
	  | mkSwitch (IfEq(obj, EExn exnname, thenact, elseact)) = 
	    Lif(Lprim(Prim.Ptest Prim.Peq_test, [obj, exnname]), 
		share thenact, 
		share elseact)
	  | mkSwitch tree = toseq tree

    in toseq tree end

(* The entry point *)

fun translateMatch (env : Tr_env.TranslEnv) failure_code loc mrules =
  let val failure = mkDecision Failure
      val uniqmrules = 
	  List.map (fn (pats, rhs) => (pats, mkDecision (Success rhs))) mrules
      val decdag = makedag env failure uniqmrules 
      val _ = incrnode decdag;
      val _ = Hasht.clear table		(* Discard memo-table *)
      open Mixture
  in
      if List.exists (fn (_, rhs) => not (used rhs)) uniqmrules then
	  (msgIBlock 0;
	   Location.errLocation loc;
	   errPrompt "Warning: some cases are unused in this match.";
	   msgEOL(); msgEOL();
	   msgEBlock())
      else ();
      if used failure then		               (* Inexhaustive match *)
	  tolambda decdag (failure_code ())
      else
	  tolambda decdag Lunspec
  end
