(*  back.sml : translation of lambda terms to lists of instructions. *)

(* 1996.07.27 -- e *)

open List Fnlib Mixture Const Lambda Prim Instruct;

(* "isReturn" determines if we're in tail call position. *)

fun isReturn (Kreturn _ :: _ )             = true
  | isReturn (Klabel _ :: Kreturn _ :: _ ) = true
  | isReturn _                             = false
;

(* Label generation *)

val labelCounter = ref 0;

fun resetLabel() =
  labelCounter := 0
;

fun new_label() =
  (incr labelCounter; !labelCounter)
;

(* the label ref in an Lshared node is used as follows:
   NoLabel -> (~1) initial value
   n  < 0  -> seen by the nth pass of unref
   n >= 0  -> a real label, code has been emitted
 Whenever a Lshared node is processed by a rewriter,
 its label ref is set to !labelNotCtr. The counter is 
 bumped by each rewrite in UNdeBruijn's lftexp.
*)

val labelNotCtr = ref Nolabel; (* for Lshared *)

fun resetLabelNot() =
  labelNotCtr := Nolabel - 1
;

fun newLabelNot() =
  (decr labelNotCtr; !labelNotCtr)
;

(* Add a label to a list of instructions. *)

fun labelCode C =
  case C of
    Kbranch lbl :: _ =>
      (lbl, C)
  | Klabel lbl :: _ =>
      (lbl, C)
  | _ =>
      let val lbl = new_label()
      in (lbl, Klabel lbl :: C) end
;

(* Generate a branch to the given list of instructions. *)

fun makeBranch C =
  case C of
    (return as Kreturn _) :: _ => (return, C)
  | (branch as Kbranch _) :: _ => (branch, C)
  | Kraise :: _  => (Kraise, C)
  | Klabel _ :: (return as Kreturn _) :: _ => (return, C)
  | Klabel lbl :: _ => (Kbranch lbl, C)
  | _ =>
      let val lbl = new_label()
      in (Kbranch lbl, Klabel lbl :: C) end
;

(* Discard all instructions up to the next label. *)

fun discardDeadCode C =
  case C of
    [] => []
  | Klabel _ :: _      => C
  | Krestart :: _      => C
  | Kset_global _ :: _ => C
  | _ :: rest => discardDeadCode rest
;

(* compile time model of runtime environment
     mapping Lvar id to stack or heapenv offsets
   fv is freevars, a list; position of negative id is the runtime env index
   va is vararray, an array mapping positive ids to stack index
*)

(* The nullEnv is used for compiling the initialization code of a
   unit.  A size of 4000 seems generous; the required number rarely
   exceeds 200.  The compiler reports Internal error: bindEnv if this
   table is too small. *)

val nullEnv = ([],Array.array(4000,(~1))) : int list * int Array.array;

fun makeEnv fv maxstk = (fv, Array.array(maxstk,(~1)));

fun findEnv (fv,va) sz n =
  if n < 0
  then let fun f i [] = fatalError ("findEnv fv n: " ^ (makestring n))
             | f i (x::r) = if x = n then Kenvacc i else f (i + 1) r
       in f 0 fv end
  else let val i = Array.sub (va,n)
       in if i < 0 then fatalError "findEnv va1" else Kaccess (sz - i) end
       handle Subscript => fatalError ("findEnv va2 n: " ^ (makestring n))
;
fun findStk (fv,va) sz n =
  if n < 0
  then fatalError ("findStk n: " ^ (makestring n))
  else let val i = Array.sub (va,n)
       in if i < 0 then fatalError "findStk va1" else Kassign (sz - i) end
       handle Subscript => fatalError ("findStk va2 n: " ^ (makestring n))
;

(* e -- note: this destroys the env *)

fun bindEnv (fv,va) n z =
  let val n' = n + 1
  in
    Array.update (va,n,z); (* not n' !? *)
    n'
  end
  handle Subscript => fatalError "bindEnv"
;

(* *)

fun addPop n C =
  if n = 0
  then C
  else
    case C of
      Kpop m :: C    => addPop (n + m) C
    | Kreturn m :: C => Kreturn (n + m) :: C
    | Kraise :: _    => C
    | _              => Kpop n :: C
;

(* Generate a jump through table, unless unnecessary. *)

exception JumpOut;

fun add_SwitchTable switchtable C =
  let open Array infix 9 sub in
    (for (fn i => if (switchtable sub i) <> (switchtable sub 0) then
                    raise JumpOut
                  else ())
         1 (length switchtable - 1);
     case C of
         Klabel lbl :: C1 =>
           if lbl = (switchtable sub 0) then C
           else
             Kbranch (switchtable sub 0) :: discardDeadCode C
       | _ =>
          Kbranch (switchtable sub 0) :: discardDeadCode C)
    handle JumpOut =>
      Kswitch switchtable :: C
  end;

(* Compiling N-way integer branches *)

(* Input: a list of (key, action) pairs, where keys are integers. *)
(* Output: a decision tree with the format below *)

datatype DecisionTree =
    DTfail
  | DTinterval of DecisionTree * Decision * DecisionTree

withtype Decision =
{
  low: int,
  act: Lambda Array.array,
  high: int
};

fun compileNBranch int_of_key clauses =
  let open Array infix 9 sub
      val clauses_i =
        map (fn (key, act) => (int_of_key key : int, act)) clauses
      val clauses_s =
        Sort.sort (fn (key1, act1) => fn (key2, act2) => key1 <= key2)
                  clauses_i
      val keyv = Array.fromList (map fst clauses_s)
      val actv = Array.fromList (map snd clauses_s)
      val n    = length keyv
      fun extractAct start stop =
        let val a =
              array((keyv sub stop) - (keyv sub start) + 1, Lstaticfail)
        in
          for (fn i =>
                 update(a, (keyv sub i) - (keyv sub start), actv sub i))
              start stop;
          a
        end
      (* Now we partition the set of keys keyv into maximal
         dense enough segments. A segment is dense enough
         if its span (max point - min point) is less
         than four times its size (number of points). *)
      fun partition start =
        if start >= n then [] else
        let val stop = ref (n-1) in
          while (keyv sub !stop) - (keyv sub start) >= 255 orelse
                (keyv sub !stop) - (keyv sub start) > 4 * (!stop - start)
          do decr stop;
          (* We've found a segment that is dense enough.
             In the worst case, !stop = start and the segment is
             a single point *)
          (* Now build the vector of actions *)
          { low = keyv sub start,
            act = extractAct start (!stop),
            high = keyv sub !stop } :: partition (!stop + 1)
        end
      val part = Array.fromList (partition 0)
      (* We build a balanced binary tree *)
      fun make_tree start stop =
        if start > stop then
          DTfail
        else
          let val middle = (start + stop) div 2 in
            DTinterval(make_tree start (middle-1),
                       part sub middle,
                       make_tree (middle+1) stop)
          end
  in make_tree 0 (length part - 1) end
;

(* Inversion of a boolean test ( < becomes >= and so on) *)

val invertPrimTest = fn
    PTeq => PTnoteq
  | PTnoteq => PTeq
  | PTnoteqimm x => fatalError "invertPrimTest"
  | PTlt => PTge
  | PTle => PTgt
  | PTgt => PTle
  | PTge => PTlt
;

val invertBoolTest = fn
    Peq_test => Pnoteq_test
  | Pnoteq_test => Peq_test
  | Pint_test t => Pint_test(invertPrimTest t)
  | Pfloat_test t => Pfloat_test(invertPrimTest t)
  | Pstring_test t => Pstring_test(invertPrimTest t)
  | Pword_test t => Pword_test(invertPrimTest t)
  | Pnoteqtag_test t => fatalError "invertBoolTest"
;

(* Production of an immediate test *)

val testForAtom = fn
    INTscon x => Pint_test(PTnoteqimm x)
  | WORDscon x => Pword_test(PTnoteqimm x)
  | CHARscon x => Pint_test(PTnoteqimm (Char.ord x))
  | REALscon x => Pfloat_test(PTnoteqimm x)
  | STRINGscon x => Pstring_test(PTnoteqimm x)
;

(* To keep track of function bodies that remain to be compiled. *)

(* ... a stack of (lbl, nargs, free var ids list, max id, body) *)

val stillToCompile  = (Stack.new () : (int * int * int list * int * Lambda) Stack.t);

(* translation of lambda-deBruijn to lambda-merged-stkdepth
   this requires giving all the Lvars unique ids within the enclosing function
    [well, kinda. unique within their scope, anyway]
   to do this requires maintaining an rstack depth model
     then the translation is simply: deBruijn -> depth - deBruijn - 1

   all functions are lifted to top level simultaneously
    as a result all free variables have negative ids
     a closure map is constructed for compileRest
   an Lfn is replaced with the closure constuction code
*)

fun UNdeBruijn exp =
  let
    exception Refs'R'Us
    val fv = ref []
    val md = ref 0

    fun ins x [] = [x]
      | ins x (ls as (y::r)) =
          if x > y then x :: ls
          else if x = y then ls
          else let val z = ins x r in if z = r then ls else y :: z end

    fun extClo id =
      if id >= 0 then ()
      else fv := (ins id (!fv))

    fun unref i exp = (* turn refs from heap into stack cells *)
      case exp of
        Lvar n => (* oops, maybe it's a real reference *)
          if n = i then raise Refs'R'Us else exp
      | Lassign (n,exp') => (* we'd only see n = i in an Lshared node? *)
          Lassign(n, unref i exp')
      | Lconst cst =>
          exp
      | Lapply(body, args) =>
          Lapply(unref i body, List.map (unref i) args)
      | Lfn body =>
         fatalError "UNdeBruijn/unref Lfn!?"
      | Llet(args, body) =>
          Llet(List.map (unref i) args, unref i body)
      | Lletrec(args, body) =>
          Lletrec(List.map (unref i) args, unref i body)
      | Lprim(Pfield 0, [opt as (Lvar v)]) =>
          if v = i
          then opt
          else exp
      | Lprim(Psetfield 0, [Lvar v, e]) =>
          if v = i
          then Lassign (i, unref i e)            (* new *)
          else Lprim(Psetfield 0, [Lvar v, unref i e])
      | Lprim(p, explist) =>
          Lprim(p, List.map (unref i) explist)
      | Lstatichandle(body, handler) =>
          Lstatichandle(unref i body, unref i handler)
      | Lstaticfail =>
          Lstaticfail
      | Lhandle(body, handler) =>
          Lhandle(unref i body, unref i handler)
      | Lif(cond, ifso, ifnot) =>
          Lif(unref i cond, unref i ifso, unref i ifnot)
      | Lseq(exp1, exp2) =>
          Lseq(unref i exp1, unref i exp2)
      | Lwhile(cond, body) =>
          Lwhile(unref i cond, unref i body)
      | Landalso(exp1, exp2) =>
          Landalso(unref i exp1, unref i exp2)
      | Lorelse(exp1, exp2) =>
          Lorelse(unref i exp1, unref i exp2)
      | Lcase(arg, clauses) =>
          Lcase(unref i arg,
                List.map (fn (tag,act) => (tag, unref i act)) clauses)
      | Lswitch(size, arg, clauses) =>
          Lswitch(size, unref i arg, 
                List.map (fn (tag,act) => (tag, unref i act)) clauses)
      | Lunspec =>
          Lunspec
      | Lshared(exp_ref, lbl_ref) =>
          if !lbl_ref <> !labelNotCtr
          then ( lbl_ref := !labelNotCtr; 
                 exp_ref := unref i (!exp_ref);
                 exp )
          else exp

    fun lftexp depth exp =
      (if depth > !md then md := depth else ();
      case exp of
        Lvar n =>
          let val id = (depth - n - 1)
          in extClo id; Lvar id end
      | Lassign (n,exp') =>
          let val id = (depth - n - 1)
          in extClo id; Lassign (id,lftexp depth exp') end
      | Lconst cst => exp
      | Lapply(body, args) => Lapply(lftexp depth body, List.map (lftexp depth) args)
      | Lfn body =>
         let val (qfv, exp') = UNdeBruijn exp (* recurse *)
         in if qfv > 0 then lftexp depth exp' else exp' end
      (* Optimize special case arising from #lab arg *)
      | Llet([arg], Lprim(p, [Lvar 0])) => 
         Lprim(p, [lftexp depth arg])
      | Llet(args, body) =>
          let fun opt_refs body' i [] acc =
                    Llet(acc, body')
                | opt_refs body' i (* do we care what the tag is? *)
                          ((exp as Lprim(Pmakeblock(CONtag(refTag,1)),[e]))
                           ::rest) acc =
                   (let val lnot = newLabelNot()
                        (* val () = BasicIO.print ("Optref: " ^ (makestring lnot)) *)
                        val acc' = List.map (unref i) acc
                        val body'' = unref i body'
                    in (* BasicIO.print " $\n"; *)
                       opt_refs body'' (i - 1) rest (e::acc')
                    end
                    handle Refs'R'Us =>
                      ( (* BasicIO.print ("\n"); *)
                       opt_refs body' (i - 1) rest (exp::acc) ) )
                | opt_refs body' i (exp::rest) acc =
                    opt_refs body' (i - 1) rest (exp::acc)
              fun lift_args ea [] acc =
                    opt_refs (lftexp ea body) (ea - 1) acc []
                | lift_args ea (exp::rest) acc =
                    lift_args (ea + 1) rest ((lftexp ea exp) :: acc)
          in lift_args depth args [] end
      | Lletrec(args, body) =>
          let val ea = depth + (List.length args)
          in Lletrec(List.map (lftexp ea) args, lftexp ea body) end
      | Lprim(p, explist) => Lprim(p, List.map (lftexp depth) explist)
      | Lstatichandle(body, handler) =>
          Lstatichandle(lftexp depth body, lftexp depth handler)
      | Lstaticfail => Lstaticfail
      | Lhandle(body, handler) =>
          Lhandle(lftexp depth body, lftexp (depth + 1) handler)
      | Lif(cond, ifso, ifnot) => Lif(lftexp depth cond, lftexp depth ifso, lftexp depth ifnot)
      | Lseq(exp1, exp2) => Lseq(lftexp depth exp1, lftexp depth exp2)
      | Lwhile(cond, body) => Lwhile(lftexp depth cond, lftexp depth body)
      | Landalso(exp1, exp2) => Landalso(lftexp depth exp1, lftexp depth exp2)
      | Lorelse(exp1, exp2) => Lorelse(lftexp depth exp1, lftexp depth exp2)
      | Lcase(arg, clauses) =>
          Lcase(lftexp depth arg, List.map (fn (tag,act) => (tag, lftexp depth act)) clauses)
      | Lswitch(size, arg, clauses) =>
          Lswitch(size, lftexp depth arg, List.map (fn (tag,act) => (tag, lftexp depth act)) clauses)
      | Lunspec => Lunspec
      | Lshared(exp_ref, lbl_ref) =>
          if !lbl_ref = Nolabel
          then ( lbl_ref := !labelNotCtr; 
                 exp_ref := lftexp depth (!exp_ref);
                 exp )
          else exp
      )

    fun liftbd depth exp = (* called with the body of an Lfn *)
      let val exp' = lftexp depth exp
          val lbl = new_label()
          val fre = List.map (fn id => Lvar ((~1) - id)) (!fv)
          val qfv = List.length fre
          val exp'' = Lprim(Pclosure (lbl, qfv), fre)
      in
        Stack.push (lbl, depth, !fv, !md, exp') stillToCompile;
        (* Pr_lam.printLam exp';  -- e *)
        (* Pr_lam.printLam exp''; -- e *)
        (qfv, exp'')
      end

    fun liftfn depth exp =
      case exp of
        Lfn body => liftfn (depth + 1) body
      | _        => liftbd depth exp

    fun liftit depth exp =
      case exp of
        Lfn body => liftfn (depth + 1) body
      | _        => let val exp' = lftexp depth exp in (List.length (!fv), exp') end

  in liftit 0 exp end
;

(* The translator from lambda terms to lists of instructions.

   env: the map from Lvar ids to stackptr offsets; side-effected
   staticfail : the pair (label,sz) where Lstaticfail must branch.
   sz: the current runtime stack model depth  (includes codegen temporaries)
   dp: the depth of the Front.sml stack model (excludes codegen temporaries)
   exp : the lambda term to compile.
   C : the continuation, i.e. the code that follows the code for lambda.

   The tests on the continuation detect tail-calls and avoid jumps to jumps,
   or jumps to function returns.

*)

fun compileExp env staticfail =
  let
    open Array infix 9 sub

    fun compexp sz dp exp C =
      case exp of
        Lvar n =>
          (findEnv env sz n) :: C
      | Lassign (n,exp') =>
          compexp sz dp exp' ((findStk env sz n) :: C)
      | Lconst cst =>
          (case C of
               Kquote _      :: _ => C
             | Kget_global _ :: _ => C
             | Kaccess _     :: _ => C
             | Kenvacc _     :: _ => C
             | _ => Kquote cst :: C)
      | Lapply(body, args) =>
          let val na = List.length args
          in
            if isReturn C
            then compExpList sz dp args
                   (Kpush :: compexp (sz + na) dp body
                               (Kappterm (na, sz + na) :: discardDeadCode C))
            else if na < 5
            then compExpList sz dp args
                   (Kpush :: compexp (sz + na) dp body (Kapply na :: C))
            else
		(* 3 is the number of stack positions used by Kpush_retaddr *)
              let val (lbl, C1) = labelCode C
              in Kpush_retaddr lbl ::
                   compExpList (sz + 3) dp args
                     (Kpush :: compexp (sz + 3 + na) dp body (Kapply na :: C1))
              end
          end
      | Lfn body =>
          fatalError "compileExp Lfn!?"
      (* Special case arising from val _ = arg *)
      | Llet([arg], Lunspec) => compexp sz dp arg C
      | Llet(args, body) =>
          let val na = List.length args
              fun complet sz dp [] =
                    compexp sz dp body (addPop na C)
                | complet sz dp (exp::rest) =
                    let val z = sz + 1
                    in
                      compexp sz dp exp 
		              (Kpush :: complet z (bindEnv env dp z) rest)
                    end
          in complet sz dp args end
      | Lletrec([e as Lprim(Pclosure (lbl, csz), fre)], body) =>
          let val z = sz + 1
              val d = bindEnv env dp z
              val C1 = Kpush :: compexp z d body (addPop 1 C)
          in
            case fre of
	      (Lvar n)::rest =>
                if n = dp
                then compExpList sz dp rest (Kclosurerec (lbl, csz) :: C1)
                else compExpList sz dp  fre (Kclosure (lbl, csz) :: C1)
	    | [] =>  Kclosure (lbl, 0) :: C1
	    |  _ =>  fatalError "compileExp: malformed Letrec!?"
          end
      | Lletrec(args, body) =>
          let val na = List.length args
              fun comparg sz dp i [] =
                    compexp sz dp body (addPop na C)
                | comparg sz dp i ((e as Lprim(Pclosure (lbl, csz), fre)) :: r) =
                    compexp sz dp e
                      (Kpush :: Kaccess i :: Kprim Pupdate :: comparg sz dp (i-1) r)
                | comparg _ _ _ _ =
                    fatalError "compileExp Lletrec"
              fun initarg sz dp [] =
                    comparg sz dp na args
                | initarg sz dp (Lprim(Pclosure (lbl, csz), fre) :: r) =
                    let val z = sz + 1
                    in
                      Kprim (Pdummy csz) :: Kpush :: initarg z (bindEnv env dp z) r
                    end
                | initarg _ _ (e::_) =
                    ((* Pr_lam.printLam e; *)
                     fatalError "compileExp Lletrec")
          in
            initarg sz dp args
          end
      | Lprim(Psmladdint, [exp, Lconst(ATOMsc(INTscon 1))]) =>
	  compexp sz dp exp (Kprim Psmlsuccint :: C)
      | Lprim(Psmlsubint, [exp, Lconst(ATOMsc(INTscon 1))]) =>
	  compexp sz dp exp (Kprim Psmlpredint :: C)
      | Lprim(Pget_global uid, []) =>
            Kget_global uid :: C
      | Lprim(Pset_global uid, [exp]) =>
            compexp sz dp exp (Kset_global uid :: C)
      | Lprim(Pfield i, explist) =>
            compExpListLR sz dp explist (Kgetfield i :: C)
      | Lprim(Psetfield i, explist) =>
            compExpListLR sz dp explist (Ksetfield i :: C)
      | Lprim(Pmakeblock tag, explist) =>
            compExpListLR sz dp explist 
	                        (Kmakeblock(tag, List.length explist) :: C)
      | Lprim(Pnot, [exp]) =>
          (case C of
               Kbranchif lbl :: C' =>
                 compexp sz dp exp (Kbranchifnot lbl :: C')
             | Kbranchifnot lbl :: C' =>
                 compexp sz dp exp (Kbranchif lbl :: C')
             | _ =>
                 compexp sz dp exp (Kprim Pnot :: C))
      | Lprim(p as Ptest tst, explist) =>
          (case C of
               Kbranchif lbl :: C' =>
                 compExpListLR sz dp explist (Ktest(tst,lbl) :: C')
             | Kbranchifnot lbl :: C' =>
                 compExpListLR sz dp  explist (Ktest(invertBoolTest tst,lbl) :: C')
             | _ =>
                 compExpListLR sz dp  explist (Kprim p :: C))
      | Lprim(Praise, explist) =>
            compExpListLR sz dp explist (Kraise :: discardDeadCode C)
      | Lprim(Pclosure (lbl, csz), explist) =>
            compExpList sz dp explist (Kclosure (lbl, csz) :: C)
      (* This enables merging of pop, return, etc *)
      | Lprim(Pidentity, explist) =>
            compExpListLR sz dp explist C
      | Lprim(p, explist) =>
            compExpListLR sz dp explist (Kprim p :: C)
      | Lstatichandle(body, Lstaticfail) =>
            compexp sz dp body C
      | Lstatichandle(body, handler) =>
          let val (branch1, C1) = makeBranch C
              val (handle2, C2) = labelCode (compexp sz dp handler C1)
          in
            compileExp env (handle2, sz) sz dp body 
	               (branch1 :: discardDeadCode C2)
          end
      | Lstaticfail =>
          let val (lbl,tsz) = staticfail
          in addPop (sz - tsz) (Kbranch lbl :: discardDeadCode C) end
      | Lhandle(body, handler) =>
          let val (branch1, C1) = makeBranch C
              val lbl2 = new_label()
              val z = sz + 1
          in
            Kpushtrap lbl2 ::
              compexp (sz + 4) dp body
                (Kpoptrap :: branch1
                   :: Klabel lbl2 :: Kpush 
                       :: compexp z (bindEnv env dp z) handler (addPop 1 C1))
          end
      | Lif(cond, ifso, ifnot) =>
            compTest2 sz dp cond ifso ifnot C
      | Lseq(exp1, exp2) =>
            compexp sz dp exp1 (compexp sz dp exp2 C)
      | Lwhile(cond, body) =>
          let val lbl2 = new_label() 
              val (lbl1, C1) = labelCode (compexp sz dp cond
                                            (Kbranchif lbl2 :: Kquote constUnit :: C))
          in
            Kbranch lbl1 :: Klabel lbl2 :: Kcheck_signals :: compexp sz dp body C1
          end
      | Landalso(exp1, exp2) =>
          (case C of
               Kbranch lbl :: _  =>
                 compexp sz dp exp1 (Kstrictbranchifnot lbl :: compexp sz dp exp2 C)
             | Kbranchifnot lbl :: _ =>
                 compexp sz dp exp1 (Kbranchifnot lbl :: compexp sz dp exp2 C)
             | Kbranchif lbl :: C' =>
                 let val (lbl1, C1) = labelCode C' in
                   compexp sz dp exp1 (Kbranchifnot lbl1 ::
                                 compexp sz dp exp2 (Kbranchif lbl :: C1))
                 end
             | Klabel lbl :: _ =>
                 compexp sz dp exp1 (Kstrictbranchifnot lbl :: compexp sz dp exp2 C)
             | _ =>
                 let val lbl = new_label() in
                   compexp sz dp exp1 (Kstrictbranchifnot lbl ::
                                 compexp sz dp exp2 (Klabel lbl :: C))
                 end)
      | Lorelse(exp1, exp2) =>
          (case C of
               Kbranch lbl :: _  =>
                 compexp sz dp exp1 (Kstrictbranchif lbl :: compexp sz dp exp2 C)
             | Kbranchif lbl :: _  =>
                 compexp sz dp exp1 (Kbranchif lbl :: compexp sz dp exp2 C)
             | Kbranchifnot lbl :: C' =>
                 let val (lbl1, C1) = labelCode C' in
                   compexp sz dp exp1 (Kbranchif lbl1 ::
                                 compexp sz dp exp2 (Kbranchifnot lbl :: C1))
                 end
             | Klabel lbl :: _ =>
                 compexp sz dp exp1 (Kstrictbranchif lbl :: compexp sz dp exp2 C)
             | _ =>
                 let val lbl = new_label() in
                   compexp sz dp exp1 (Kstrictbranchif lbl ::
                                 compexp sz dp exp2 (Klabel lbl :: C))
                 end)

      | Lcase(arg, clauses) =>
          let val C1 =
            if case clauses of
                   (INTscon _, _) :: _ => true
                 | (WORDscon _, _) :: _ => true
                 | (CHARscon _, _) :: _ => true
                 | _ => false
            then
              compDecision sz dp (compileNBranch intOfAtom clauses) C
            else
              compTests sz dp
                (map (fn (cst, act) => (testForAtom cst, act)) clauses) C
          in compexp sz dp arg C1 end

      | Lswitch(1, arg, [(CONtag(_,_), exp)]) =>
          compexp sz dp exp C
          (* We assume the argument to be safe (not producing side-effects
             and always terminating),
             because switches are generated only by the match compiler *)
      | Lswitch(2, arg, [(CONtag(0,_), exp0)]) =>
          compTest2 sz dp arg Lstaticfail exp0 C
      | Lswitch(2, arg, [(CONtag(1,_), exp1)]) =>
          compTest2 sz dp arg exp1 Lstaticfail C
      | Lswitch(2, arg, [(CONtag(0,_), exp0), (CONtag(1,_), exp1)]) =>
          compTest2 sz dp arg exp1 exp0 C
      | Lswitch(2, arg, [(CONtag(1,_), exp1), (CONtag(0,_), exp0)]) =>
          compTest2 sz dp arg exp1 exp0 C
      | Lswitch(size, arg, clauses) =>
          let val C1 =
	    if List.length clauses >= size - 5 then
              Kprim Ptag_of :: compDirectSwitch sz dp size clauses C
            else
              Kprim Ptag_of ::
                compDecision sz dp (compileNBranch intOfAbsoluteTag clauses) C
          in compexp sz dp arg C1 end
      | Lunspec =>
          C
      | Lshared(exp_ref, lbl_ref) =>
          if !lbl_ref < 0 then
            let val (lbl, C1) = labelCode (compexp sz dp (!exp_ref) C)
            in
              lbl_ref := lbl; C1
            end
          else
            Kbranch (!lbl_ref) :: discardDeadCode C

    (* Compile right-left evaluation of args of functions *)
    and compExpList' sz dp [] C = C
      | compExpList' sz dp [exp] C = compexp sz dp exp C
      | compExpList' sz dp (exp::rest) C =
          compExpList' (sz - 1) dp rest (Kpush :: compexp sz dp exp C)

    and compExpList sz dp ls C = 
	compExpList' (sz + List.length ls - 1) dp ls C

    (* Compile left-right evaluation of args of primitives *)
    and compExpListLR' sz dp [] C = C
      | compExpListLR' sz dp [exp] C = compexp sz dp exp C
      | compExpListLR' sz dp (exp::rest) C =
	compexp sz dp exp (Kpush :: compExpListLR' (sz + 1) dp rest C)

    and compExpListLR sz dp ls C = compExpListLR' sz dp ls C

    and compTest2 sz dp cond ifso ifnot C =
      let val (sflbl,sftsz) = staticfail
          val Cc = 
(* This optimization is rather ill-considered.  It works if the result () 
   of the switch is disregarded, but otherwise it fails.  sestoft 2000-04-26

            if ifnot = Lconst constUnit
            then let val (lbl, C1) = labelCode C
                 in Kstrictbranchifnot lbl :: compexp sz dp ifso C1 end
            else *)

            if ifso = Lstaticfail andalso sz = sftsz
            then Kbranchif sflbl :: compexp sz dp ifnot C
            else
            if ifnot = Lstaticfail andalso sz = sftsz
            then Kbranchifnot sflbl :: compexp sz dp ifso C
            else
              let val (branch1, C1) = makeBranch C
                  val (lbl2, C2) = labelCode (compexp sz dp ifnot C1)
              in
                Kbranchifnot lbl2 :: compexp sz dp ifso 
		                             (branch1 :: discardDeadCode C2)
              end
      in
        compexp sz dp cond Cc
      end

    and compTests sz dp clauses C =
      let val (branch1, C1) = makeBranch C
          val (sflbl,sftsz) = staticfail
          val () = if sz <> sftsz then fatalError "compTests sz" else () (* e -- assert ?? *)
          fun comp [] =
                fatalError "compTests"
            | comp [(test,exp)] =
                Ktest(test, sflbl) :: compexp sz dp exp C1
            | comp ((test,exp)::rest) =
                let val lbl = new_label() in
                  Ktest(test, lbl) :: 
                    compexp sz dp exp (branch1 :: Klabel lbl :: comp rest)
                end
      in comp clauses end

    and compSwitch sz dp v branch1 C =
      let val (sflbl,sftsz) = staticfail
          val switchtable = array(length v, sflbl)
          fun comp_cases n =
            if n >= length v then
              C
            else
              let val (lbl, C1) = 
		  labelCode (compexp sz dp (v sub n) 
			     (branch1 :: discardDeadCode (comp_cases (n+1))))
              in 
                update(switchtable, n, lbl); C1 
              end
      in add_SwitchTable switchtable (discardDeadCode(comp_cases 0)) end

    and compDecision sz dp tree C =
      let val (branch1, C1) = makeBranch C
          val (sflbl,sftsz) = staticfail
          val () = if sz <> sftsz then fatalError "compDecision sz" else () (* e -- assert ?? *)
          fun comp_dec DTfail C =
                Kbranch sflbl :: discardDeadCode C
            | comp_dec (DTinterval(left, dec, right)) C =
                let val (lbl_right, Cright) =
                      case right of
                          DTfail => (sflbl, C)
                        | _      => labelCode (comp_dec right C)
                    val (lbl_left, Cleft) =
                      case left of
                          DTfail => (sflbl, Cright)
                        | _ =>      labelCode (comp_dec left Cright)
                    val {low, act, high} = dec
                in
                  Kbranchinterval(low, high, lbl_left, lbl_right) ::
                  (case length act of
                       1 => compexp sz dp (act sub 0)
                                          (branch1 :: discardDeadCode Cleft)
                     | _ => compSwitch sz dp act branch1 Cleft)
                end
      in comp_dec tree C1 end

    and compDirectSwitch sz dp size clauses C =
      let val (branch1, C1) = makeBranch C
          val (sflbl,sftsz) = staticfail
          val () = if sz <> sftsz andalso size <> (List.length clauses)
                   then fatalError "compDirectSwitch sz" else () (* e -- assert ?? *)
          val switchtable = array(size, sflbl)
          fun comp_case [] =
                fatalError "compDirectSwitch"
            | comp_case [(tag, exp)] =
                let val (lbl, C2) = labelCode (compexp sz dp exp C1) in
                  update(switchtable, intOfAbsoluteTag tag, lbl);
                  C2
                end
            | comp_case ((tag, exp) :: rest) =
                let val (lbl, C2) =
                  labelCode (compexp sz dp exp 
			     (branch1 :: discardDeadCode (comp_case rest)))
                in
                  update(switchtable, intOfAbsoluteTag tag, lbl);
                  C2
                end
      in add_SwitchTable switchtable (discardDeadCode(comp_case clauses)) end

  in compexp end
;

fun compileRest C =
  let val (lbl, nargs, fv, maxstk, exp) = Stack.pop stillToCompile
      val env = makeEnv fv maxstk
      fun inienv a sz = if a < nargs
                        then inienv (bindEnv env a sz) (sz - 1)
                        else ()
      val () = inienv 0 nargs
      val C' = compileExp env (Nolabel, 0) nargs nargs exp 
      	                  (Kreturn nargs :: discardDeadCode C)
  in
    compileRest (if nargs > 1
                 then (Krestart :: Klabel lbl :: Kgrab (nargs - 1) :: C')
                 else (Klabel lbl :: C'))
  end
  handle Stack.Empty =>
    C
;

fun compileLambda is_pure exp =
  let val () = Stack.clear stillToCompile
      val () = resetLabel()
      val () = resetLabelNot()
      val (qfv, exp') = UNdeBruijn exp (* e -- could check: qfv = 0 *)
      val init_code =
            compileExp nullEnv (Nolabel, 0) 0 0 exp' []
      val function_code =
            compileRest [] 
  in
    { kph_is_pure = is_pure,
      kph_inits   = init_code,
      kph_funcs   = function_code }
  end;
