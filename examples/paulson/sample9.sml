(**** ML Programs from Chapter 9 of

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




(**** Lexical Analysis -- Scanning ****)

(*Input to LexicalFUN*)
signature KEYWORD =
  sig
  val alphas: string list
  and symbols: string list
  end;


signature LEXICAL =
  sig
  datatype token =  Key of string | Id of string 
  val scan : string -> token list
  end;


(*All characters are covered; whitespace is ignored. *)
functor Lexical (Keyword: KEYWORD) : LEXICAL =
  struct
  datatype token = Key of string  |  Id of string;

  fun member (x:string, l) = List.exists (fn y => x=y) l;

  fun alphaTok a = 
      if member(a, Keyword.alphas) then Key(a) else Id(a);

  (*scanning of a symbolic keyword*)
  fun symbolic (sy, ss) = 
      case Substring.getc ss of
	  NONE => (Key sy, ss)
	| SOME (c,ss1) => 
	      if member(sy, Keyword.symbols) 
		  orelse not (Char.isPunct c)
	      then  (Key sy, ss)
	      else  symbolic (sy ^ String.str c, ss1);

  (*Scanning a substring into a list of tokens*)
  fun scanning (toks, ss) = 
      case Substring.getc ss of
	  NONE => rev toks    (*end of substring*)
	| SOME (c,ss1) => 
	    if Char.isAlphaNum c 
		then (*identifier or keyword*)
		 let val (id, ss2) = Substring.splitl Char.isAlphaNum ss
		     val tok = alphaTok (Substring.string id)
		 in  scanning (tok::toks, ss2)
		 end
	    else if Char.isPunct c
	    then (*special symbol*)
		 let val (tok, ss2) = symbolic (String.str c, ss1)
		 in  scanning (tok::toks, ss2)
		 end
	    else (*ignore spaces, line breaks, control characters*)
		 scanning (toks, Substring.dropl (not o Char.isGraph) ss);

  fun scan a = scanning([], Substring.all a);
  end;


(**** Parsing functionals ****)

(*Infix directives: made globally*)
infix 6 $--;
infix 5 --;
infix 3 >>;
infix 0 ||;

signature PARSE =
  sig
  exception SyntaxErr of string
  type token
  val id : token list -> string * token list
  val $  : string -> token list -> string * token list
  val empty : 'a -> 'b list * 'a
  val || : ('a -> 'b) * ('a -> 'b) -> 'a -> 'b
  val !! : ('a -> 'b * 'c) -> 'a -> 'b * 'c
  val -- : ('a -> 'b * 'c) * ('c -> 'd * 'e) -> 'a -> ('b * 'd) * 'e
  val $-- : string * (token list -> 'a * 'b) -> token list -> 'a * 'b
  val >> : ('a -> 'b * 'c) * ('b -> 'd) -> 'a -> 'd * 'c
  val repeat : ('a -> 'b * 'a) -> 'a -> 'b list * 'a
  val infixes :
      (token list -> 'a * token list) * (string -> int) *
      (string -> 'a -> 'a -> 'a) -> token list -> 'a * token list
  val reader: (token list -> 'a * 'b list) -> string -> 'a
  end;

functor Parsing (Lex: LEXICAL) : PARSE =
  struct
  type token = Lex.token;

  exception SyntaxErr of string;

  (*Phrase consisting of an identifier*)
  fun id (Lex.Id a :: toks) = (a,toks)
    | id toks = raise SyntaxErr "Identifier expected";

  (*Phrase consisting of the keyword 'a' *)
  fun $a (Lex.Key b :: toks) = if a=b then (a,toks) 
			       else raise SyntaxErr a
    | $a _ = raise SyntaxErr "Symbol expected";

  (*The empty phrase!*)
  fun empty toks = ([],toks);

  (*Alternative phrases*)
  fun (ph1 || ph2) toks = ph1 toks 
			  handle SyntaxErr _ => ph2 toks;

  fun !! ph toks = ph toks
      handle SyntaxErr msg => raise Fail ("Syntax error: " ^ msg);

  (*One phrase then another*)
  fun (ph1 -- ph2) toks = 
      let val (x,toks2) = ph1 toks
	  val (y,toks3) = ph2 toks2
      in  ((x,y), toks3)  end;

  (*Application of f to the result of a phrase*)
  fun (ph>>f) toks = 
      let val (x,toks2) = ph toks
      in  (f x, toks2)  end;

  fun (a $-- ph) = ($a -- !!ph >> #2);

  (*Zero or more phrases*)
  fun repeat ph toks = (   ph -- repeat ph >> (op::)
                        || empty   ) toks;

  fun infixes (ph,prec_of,apply) = 
    let fun over k toks = next k (ph toks)
        and next k (x, Lex.Key(a)::toks) = 
              if prec_of a < k then (x, Lex.Key a :: toks)
              else next k ((over (prec_of a) >> apply a x) toks)
          | next k (x, toks) = (x, toks)
    in  over 0  end;

  (*Scan and parse, checking that no tokens remain*)
  fun reader ph a = 
	 (case ph (Lex.scan a) of 
	      (x, []) => x
	    | (_, _::_) => raise SyntaxErr "Extra characters in phrase");

  end;


(**** Example: Parsing and Displaying Types ****)


signature TYPE = 
  sig
  datatype t = Con of string * t list | Var of string
  val pr : t -> unit
  val read : string -> t
  end;

structure LamKey = 
    struct val alphas = []
           and symbols = ["(", ")", "'", "->"]
    end;
structure LamLex = Lexical (LamKey);
structure LamParsing = Parsing (LamLex);


structure Type : TYPE =
  struct
  datatype t = Con of string * t list
             | Var of string;

  local (** Parsing **)
    fun makeFun (ty1,ty2) = Con("->",[ty1,ty2]);
    open LamParsing

    fun typ toks =
     (   atom -- "->" $-- typ			>> makeFun
      || atom	
     ) toks
    and atom toks =
      (   $"'" -- id				>> (Var o op^)
       || "(" $-- typ -- $")"			>> #1
      ) toks;
  in
    val read = reader typ;
  end;

  local (** Display **)
    fun typ (Var a) = Pretty.str a
      | typ (Con("->",[ty1,ty2])) = Pretty.blo(0, [atom ty1, 
						 Pretty.str " ->", 
						 Pretty.brk 1, 
						 typ ty2])
     and atom (Var a) = Pretty.str a
       | atom ty = Pretty.blo(1, [Pretty.str"(", 
				 typ ty, 
				 Pretty.str")"]);
  in
    fun pr ty = Pretty.pr (TextIO.stdOut, typ ty, 50)
  end

end;


(**** Lambda-terms.  
      Bound variables are indicated by depth index, 
      free variables by name. ****)

signature LAMBDA =
  sig
  datatype t = Free  of string
	     | Bound of int
	     | Abs   of string * t
	     | Apply of t * t
  val abstract: int -> string -> t -> t
  val absList: string list * t -> t
  val applyList: t * t list -> t
  val subst: int -> t -> t -> t
  val inst: t StringDict.t -> t -> t
  end;
  
structure Lambda : LAMBDA =
  struct
  datatype t = Free  of string
	     | Bound of int
	     | Abs   of string*t
	     | Apply of t*t;

  (*Convert occurrences of b to bound index i in a term*)
  fun abstract i b (Free a) = if a=b then  Bound i  else  Free a
    | abstract i b (Bound j) = Bound j
    | abstract i b (Abs(a,t)) = Abs(a, abstract (i+1) b t)
    | abstract i b (Apply(t,u)) = Apply(abstract i b t, abstract i b u);

  (*Abstraction over several free variables*)
  fun absList (bs,t) = foldr (fn (b,u) => Abs(b, abstract 0 b u)) t bs;

  (*Application of t to several terms*)
  fun applyList (t0,us) = foldl (fn (u,t) => Apply(t,u)) t0 us;

  (*Shift a term's non-local indices by i; d is the depth of abstractions*)
  fun shift 0 d u = u
    | shift i d (Free a) = Free a
    | shift i d (Bound j) = if j>=d then Bound(j+i) else Bound j 
    | shift i d (Abs(a,t)) = Abs(a, shift i (d+1) t)
    | shift i d (Apply(t,u)) = Apply(shift i d t, shift i d u);

  (*Substitute u for bound variable i in a term t*)
  fun subst i u (Free a)  = Free a
    | subst i u (Bound j) =
	if j<i then  	Bound j   	(*locally bound*)
	else if j=i then shift i 0 u
	else (*j>i*)	Bound(j-1)	(*non-local to t*)
    | subst i u (Abs(a,t)) = Abs(a, subst (i+1) u t)
    | subst i u (Apply(t1,t2)) = Apply(subst i u t1, subst i u t2);

  (*Substitution for free variables*)
  fun inst env (Free a) = (inst env (StringDict.lookup(env,a)) 
		           handle StringDict.E _ => Free a)
    | inst env (Bound i) = Bound i
    | inst env (Abs(a,t)) = Abs(a, inst env t)
    | inst env (Apply(t1,t2)) = Apply(inst env t1, inst env t2);
  end;


(*** Parsing of lambda terms ***)
signature PARSE_TERM = 
  sig
  val read: string -> Lambda.t
  end;

structure ParseTerm : PARSE_TERM =
  struct

  fun makeLambda ((b,bs),t) = Lambda.absList (b::bs, t);

  open LamParsing  

  (*term/atom distinction prevents left recursion; grammar is ambiguous*)
  fun term toks =
    (   "%" $-- id -- repeat id -- "." $-- term >> makeLambda
     || atom -- repeat atom			 >> Lambda.applyList
    ) toks
  and atom toks =
    (   id					 >> Lambda.Free
     || "(" $-- term -- $")"			 >> #1
    ) toks;
  val read = reader term;

  end;


(**** Pretty Printing of lambda terms ****)

signature DISPLAY_TERM = 
  sig
  val rename: string list * string -> string
  val stripAbs: Lambda.t -> string list * Lambda.t
  val pr: Lambda.t -> unit
  end;

structure DisplayTerm : DISPLAY_TERM =
  struct

  (*Free variable in a term -- simple & slow version using append*)
  fun vars (Lambda.Free a) = [a]
    | vars (Lambda.Bound i) = []
    | vars (Lambda.Abs(a,t)) = vars t
    | vars (Lambda.Apply(t1,t2)) = vars t1 @ vars t2;

  (*Rename variable "a" to avoid clashes with the strings bs. *)
  fun rename (bs,a) =
      if  List.exists (fn x => x=a) bs  then  rename (bs, a ^ "'")  else  a;

  (*Remove leading lambdas; return bound variable names*)
  fun strip (bs, Lambda.Abs(a,t)) = 
        let val b = rename (vars t, a)
	in  strip (b::bs, Lambda.subst 0 (Lambda.Free b) t)  
        end
    | strip (bs, u) = (rev bs, u);

  fun stripAbs t = strip ([],t);

  fun spaceJoin (b,z) = " " ^ b ^ z;

  fun term (Lambda.Free a) = Pretty.str a 
    | term (Lambda.Bound i) = Pretty.str "??UNMATCHED INDEX??"
    | term (t as Lambda.Abs _) =
	  let val (b::bs,u) = stripAbs t
	      val binder = "%" ^ b ^ (foldr spaceJoin ". " bs)
	  in  Pretty.blo(0, [Pretty.str binder, term u])  
          end
    | term t = Pretty.blo(0, applic t)
  and applic (Lambda.Apply(t,u)) = applic t @ [Pretty.brk 1, atom u]
    | applic t        = [atom t]
  and atom (Lambda.Free a) = Pretty.str a 
    | atom t = Pretty.blo(1, [Pretty.str"(", 
			      term t, 
			      Pretty.str")"]);

  fun pr t = Pretty.pr (TextIO.stdOut, term t, 50);
  end;


(*** Evaluation of lambda terms ***)
signature REDUCE = 
  sig
  val eval : Lambda.t -> Lambda.t
  val byValue : Lambda.t -> Lambda.t
  val headNF : Lambda.t -> Lambda.t
  val byName : Lambda.t -> Lambda.t
  end;

structure Reduce : REDUCE =
  struct

  (*evaluation, not affecting function bodies*)
  fun eval (Lambda.Apply(t1,t2)) = 
		(case eval t1 of
		     Lambda.Abs(a,u) => eval(Lambda.subst 0 (eval t2) u)
		   | u1 => Lambda.Apply(u1, eval t2))
    | eval t = t;

  (*normalization using call-by-value*)
  fun byValue t = bodies (eval t)
  and bodies (Lambda.Abs(a,t)) = Lambda.Abs(a, byValue t)
    | bodies (Lambda.Apply(t1,t2)) = Lambda.Apply(bodies t1, bodies t2)
    | bodies t = t;

  (*head normal form*)
  fun headNF (Lambda.Abs(a,t)) = Lambda.Abs(a, headNF t)
    | headNF (Lambda.Apply(t1,t2)) = 
		(case headNF t1 of
		     Lambda.Abs(a,t) => headNF(Lambda.subst 0 t2 t)
		   | u1 => Lambda.Apply(u1, t2))
    | headNF t = t;

  (*normalization using call-by-name*)
  fun byName t = args (headNF t)
  and args (Lambda.Abs(a,t)) = Lambda.Abs(a, args t)
    | args (Lambda.Apply(t1,t2)) = Lambda.Apply(args t1, byName t2)
    | args t = t;
  end;


(*** Using the structures ***)

fun insertEnv ((a,b),env) = 
    StringDict.insert (env, a, ParseTerm.read b);

val stdEnv = foldl insertEnv StringDict.empty
[    (*booleans*)
 ("true", "%x y.x"),           ("false",  "%x y.y"), 
 ("if", "%p x y. p x y"),
     (*ordered pairs*)
 ("pair", "%x y f.f x y"),  
 ("fst", "%p.p true"),         ("snd", "%p.p false"),
     (*natural numbers*)
 ("suc", "%n f x. n f (f x)"),
 ("iszero", "%n. n (%x.false) true"),
 ("0", "%f x. x"),             ("1", "suc 0"),
 ("2", "suc 1"),               ("3", "suc 2"),
 ("4", "suc 3"),               ("5", "suc 4"),
 ("6", "suc 5"),               ("7", "suc 6"),
 ("8", "suc 7"),               ("9", "suc 8"),
 ("add",  "%m n f x. m f (n f x)"),
 ("mult", "%m n f. m (n f)"),
 ("expt", "%m n f x. n m f x"),
 ("prefn", "%f p. pair (f (fst p)) (fst p)"),
 ("pre",  "%n f x. snd (n (prefn f) (pair x x))"),
 ("sub",  "%m n. n pre m"),
 ("ack",  "%m. m (%f n. n f (f 1)) suc"),
      (*lists*)
 ("nil",  "%z.z"),
 ("cons", "%x y. pair false (pair x y)"),
 ("null", "fst"),
 ("hd", "%z. fst(snd z)"),     ("tl", "%z. snd(snd z)"),
    (*recursion for call-by-name*)
 ("Y", "%f. (%x.f(x x))(%x.f(x x))"),
 ("fact", "Y (%g n. if (iszero n) 1 (mult n (g (pre n))))"),
 ("append", "Y (%g z w. if (null z) w (cons (hd z) (g (tl z) w)))"),
 ("inflist", "Y (%z. cons MORE z)"),
     (*recursion for call-by-value*)
 ("YV", "%f. (%x.f(%y.x x y)) (%x.f(%y.x x y))"),
 ("factV", "YV (%g n. (if (iszero n) (%y.1) (%y.mult n (g (pre n))))y)") ];


(** lambda reduction examples **)

fun stdRead a = Lambda.inst stdEnv (ParseTerm.read a);
fun try evfn = DisplayTerm.pr o evfn o stdRead;


