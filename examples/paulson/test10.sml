(**** Test Data for Chapter 10 of

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


open Command;
open Tactical;

fun showResult () = 
    if Rule.final(getState()) then DisplayFol.form(Rule.main (getState()))
    else raise Fail "Unsolved subgoals exist!";

fun checkFailed() =
    if Rule.final(getState()) then raise Fail "Should have failed!"
    else "Failed, as expected...";

(*** Single step proofs and basic use of tacticals ***)

(*single steps -- essentially the proof * of section 10.2 *)
goal "P & Q  -->  Q & P";
by (Rule.impR 1);
by (Rule.conjL 1);
by (Rule.conjR 1);
by (Rule.basic 2);
by (Rule.basic 1);
showResult();

(* essentially the first quantifier proof, section 10.4 *)
goal "(ALL x.P(x)) --> (ALL x. P(x) | Q(x))";
by (Rule.impR 1);
by (Rule.allR 1);
by (Rule.disjR 1);
by (Rule.allL 1);
by (Rule.unify 1);
showResult();  


(* the second quantifier proof, section 10.4 *)
goal "EX z. P(z) --> (ALL x. P(x))";
by (Rule.exR 1);
by (Rule.impR 1);
by (Rule.allR 1);
by (Rule.unify 1);  (*FAILS!*)
by (Rule.exR 1);
by (Rule.impR 1);
by (Rule.unify 1);
showResult();  



goal "(P & Q) & R  -->  P & (Q & R)";
by (Rule.impR 1);
by (Rule.conjL 1);
by (Rule.conjL 1);
by (Rule.conjR 1);
by (Rule.conjR 2);
by (Rule.basic 1);
by (Rule.basic 1);
by (Rule.basic 1);
showResult();

(** Demonstrating tacticals **)

goal "(P & Q) & R  -->  P & (Q & R)";
by (repeat (Rule.impR 1 || Rule.conjL 1));
by (repeat (Rule.basic 1 || Rule.conjR 1));
showResult();


goal "EX z. P(z) --> (ALL x. P(x))";
by (repeat (Rule.unify 1 || Rule.impR 1 ||
            Rule.allR 1 || Rule.exR 1));
showResult();  

(*single steps*)
goal "(P & Q) | R  --> (P | R) & (Q | R)";
by (Rule.impR 1);
by (Rule.disjL 1);
by (Rule.conjL 1);
by (Rule.conjR 1);
by (Rule.disjR 1);
by (Rule.disjR 2);
by (Rule.conjR 3);
by (Rule.disjR 3);
by (Rule.disjR 4);
by (Rule.basic 1);
by (Rule.basic 1);
by (Rule.basic 1);
by (Rule.basic 1);
showResult();


(*single steps*)
goal "(ALL x. P(x) & Q(x)) <-> (ALL x. P(x))  &  (ALL x. Q(x))";
by (Rule.iffR 1);
by (Rule.conjR 1);
by (Rule.allR 1);
by (Rule.allL 1);
by (Rule.conjL 1);
by (Rule.unify 1);
by (Rule.allR 1);
by (Rule.allL 1);
by (Rule.conjL 1);
by (Rule.unify 1);
by (Rule.allR 1);
by (Rule.conjL 1);
by (Rule.allL 1);
by (Rule.allL 1);
by (Rule.conjR 1);
by (Rule.unify 1);
by (Rule.unify 1);
showResult(); 

goal "(ALL x. P(x) & Q(x)) <-> (ALL x. P(x))  &  (ALL x. Q(x))";
by (repeat (Rule.unify 1 || Rule.iffR 1 || Rule.allR 1 
            || Rule.conjR 1 || Rule.conjL 1
            || Rule.allL 1));
showResult(); 


(*single steps -- BASIC TEST OF QUANTIFIER REASONING *)
goal "(EX y. ALL x. Q(x,y)) -->  (ALL x. EX y. Q(x,y))";
by (Rule.impR 1);
by (Rule.exL 1);
by (Rule.allR 1);
by (Rule.allL 1);
by (Rule.exR 1);
by (Rule.unify 1);
showResult(); 


(*single steps -- BASIC TEST OF QUANTIFIER REASONING -- INVALID *)
goal "(ALL x. EX y. Q(x,y))  -->  (EX y. ALL x. Q(x,y))";
by (Rule.impR 1);
by (Rule.allL 1);
by (Rule.exL 1);
by (Rule.exR 1);
by (Rule.allR 1);
by (Rule.unify 1);
checkFailed();
(*NB. The failure of this proof does not demonstrate that no proof exists!*)


goal "(EX x.P(x)-->Q)  <->  (ALL x.P(x)) --> Q";
by (Rule.iffR 1);
by (Rule.impR 1);
by (Rule.exL 1);
by (Rule.allL 1);
by (Rule.impL 1);
by (Rule.unify 1);
by (Rule.unify 1);
by (Rule.impL 1);
by (Rule.exR 1);
by (Rule.impR 1);
by (Rule.unify 1);
by (Rule.allR 1);
by (Rule.exR 1);
by (Rule.impR 1);
by (Rule.unify 1);
showResult();


(*** Automatic proofs ***)

print"commutative laws of & and | \n";
goal "P & Q  -->  Q & P";
by Tac.depth;
showResult();

goal "P | Q  -->  Q | P";
by Tac.depth;
showResult();


print"associative laws of & and | \n";
goal "(P & Q) & R  -->  P & (Q & R)";
by Tac.depth;
showResult();

goal "(P | Q) | R  -->  P | (Q | R)";
by Tac.depth;
showResult();


print"distributive laws of & and | \n";
goal "(P & Q) | R  --> (P | R) & (Q | R)";
by Tac.depth;
showResult();

goal "(P | R) & (Q | R)  --> (P & Q) | R";
by Tac.depth;
showResult();

goal "(P | Q) & R  --> (P & R) | (Q & R)";
by Tac.depth;
showResult();

goal "(P & R) | (Q & R)  --> (P | Q) & R";
by Tac.depth;
showResult();


print"Laws involving implication\n";

goal "(P-->R) & (Q-->R) <-> (P|Q --> R)";
by Tac.depth;
showResult();

goal "(P & Q --> R) <-> (P--> (Q-->R))";
by Tac.depth;
showResult();

goal "((P-->R)-->R) --> ((Q-->R)-->R) --> (P&Q-->R) --> R";
by Tac.depth;
showResult();

goal "~(P-->R) --> ~(Q-->R) --> ~(P&Q-->R)";
by Tac.depth;
showResult();

goal "(P --> Q & R) <-> (P-->Q)  &  (P-->R)";
by Tac.depth;
showResult();


print"Propositions-as-types\n";

(*The combinator K*)
goal "P --> (Q --> P)";
by Tac.depth;
showResult();

(*The combinator S*)
goal "(P-->Q-->R)  --> (P-->Q) --> (P-->R)";
by Tac.depth;
showResult();

(*Converse is classical*)
goal "(P-->Q) | (P-->R)  -->  (P --> Q | R)";
by Tac.depth;
showResult();

goal "(P-->Q)  -->  (~Q --> ~P)";
by Tac.depth;
showResult();


print"Classical examples\n";

goal "(P --> Q | R) --> (P-->Q) | (P-->R)";
by Tac.depth;
showResult();

(*If and only if*)

goal "(P<->Q) <-> (Q<->P)";
by Tac.depth;
showResult();

goal "~ (P <-> ~P)";
by Tac.depth;
showResult();


print"*** Quantifier examples ***\n";

goal "(ALL x. ALL y.P(x,y))  -->  (ALL y. ALL x.P(x,y))";
by Tac.depth;
showResult();  

goal "(EX x. EX y.P(x,y)) --> (EX y. EX x.P(x,y))";
by Tac.depth;
showResult();  

(*Converse is false*)
goal "(ALL x.P(x)) | (ALL x.Q(x)) --> (ALL x. P(x) | Q(x))";
by Tac.depth;
showResult();  

goal "(ALL x. P-->Q(x))  <->  (P--> (ALL x.Q(x)))";
by Tac.depth;
showResult();  


goal "(ALL x.P(x)-->Q)  <->  ((EX x.P(x)) --> Q)";
by Tac.depth;
showResult();  


print"Some harder ones\n";

goal "(EX x. P(x) | Q(x)) <-> (EX x.P(x)) | (EX x.Q(x))";
by Tac.depth;
showResult();  

(*Converse is false*)
goal "(EX x. P(x)&Q(x)) --> (EX x.P(x))  &  (EX x.Q(x))";
by Tac.depth;
showResult();  


print"Basic test of quantifier reasoning\n";
(*TRUE*)
goal "(EX y. ALL x. Q(x,y)) -->  (ALL x. EX y. Q(x,y))";
by Tac.depth;  
showResult();  


goal "(ALL x. Q(x))  -->  (EX x. Q(x))";
by Tac.depth;  
showResult();  


print"The following should fail, as they are false!\n";

goal "(EX x. Q(x))  -->  (ALL x. Q(x))";
by Tac.depth;
checkFailed();

goal "P(?a) --> (ALL x.P(x))";
by Tac.depth;
checkFailed();

goal "(P(?a) --> (ALL x.Q(x))) --> (ALL x. P(x) --> Q(x))";
by Tac.depth;
checkFailed();

print"Back to things that are provable...\n";

goal "(ALL x.P(x)-->Q(x)) & (EX x.P(x)) --> (EX x.Q(x))";
by Tac.depth;  
showResult();  


(*An example of why exists_intr should be delayed as long as possible*)
goal "(P --> (EX x.Q(x))) & P --> (EX x.Q(x))";
by Tac.depth;  
showResult();  

(*instantiation of the variable ?a *)
goal "(ALL x. P(x)-->Q(f(x))) & (ALL x. Q(x)-->R(g(x))) & P(d) --> R(?a)";
by Tac.depth; 
showResult();  

goal "(ALL x. Q(x))  -->  (EX x. Q(x))";
by Tac.depth;
showResult();  


print"Classical Logic: examples with quantifiers.\n";

goal "(ALL x. P(x) & Q(x)) <-> (ALL x. P(x))  &  (ALL x. Q(x))";
by Tac.depth;
showResult(); 

goal "(EX x. P-->Q(x))  <->  (P --> (EX x.Q(x)))";
by Tac.depth;
showResult(); 

goal "(EX x.P(x)-->Q)  <->  (ALL x.P(x)) --> Q";
by Tac.depth;
showResult(); 

goal "(ALL x.P(x)) | Q  <->  (ALL x. P(x) | Q)";
by Tac.depth;
showResult(); 


print"Harder proofs \n";

(*This Prolog program needs multiple instantiation of ALL.  *)
goal "(ALL x. P(x)-->P(f(x)))  &  P(d)-->P(f(f(f(d))))";
by Tac.depth;
(***or
by (repeat (Tac.safeStep 1 || unify 1 || Rule.allL 1));
***)
showResult();

(*Needs double instantiation of EXISTS*)
goal "EX x. P(x) --> P(a) & P(b)";
by Tac.depth;
showResult();

goal "EX z. P(z) --> (ALL x. P(x))";
by Tac.depth;
showResult();


print"Some slow ones\n";

(*Principia Mathematica *11.53  *)
goal "(ALL x. ALL y. P(x) --> Q(y)) <-> ((EX x. P(x)) --> (ALL y. Q(y)))";
by Tac.depth;
showResult();  


(*Principia Mathematica *11.55  *)
goal "(EX x. EX y. P(x) & Q(x,y)) <-> (EX x. P(x) & (EX y. Q(x,y)))";
by Tac.depth;
showResult();  


(*Principia Mathematica *11.61  *)
goal "(EX y. ALL x. P(x) --> Q(x,y)) --> (ALL x. P(x) --> (EX y. Q(x,y)))";
by Tac.depth;
showResult();  


(*Principia Mathematica *11.71   SLOW*)
goal "(EX z. P(z)) & (EX w. Q(w)) -->   \
\  ((ALL z. P(z) --> R(z)) & (ALL w. Q(w) --> S(w))     \
\        <-> (ALL z. ALL w. P(z) & Q(w) --> R(z) & S(w)))";
by Tac.depth;
showResult();


(*from Vladimir Lifschitz, What Is the Inverse Method?, JAR 5 (1989), 1--23*)
goal "EX x. EX X. ALL y. EX z. EX Z. \
\         (~P(y,y) | P(x,x) | ~S(z,x)) & \
\         (S(x,y) | ~S(y,z) | Q(Z,Z))  & \
\         (Q(X,y) | ~Q(y,Z) | S(X,X))";


(*Sample problems from 
  F. J. Pelletier, 
  Seventy-Five Problems for Testing Automatic Theorem Provers,
  J. Automated Reasoning 2 (1986), 191-216.
  Errata, JAR 4 (1988), 236-236.
*)

print"Pelletier's examples\n";
(*1*)
goal "(P-->Q)  <->  (~Q --> ~P)";
by Tac.depth;
showResult();

(*2*)
goal "~ ~ P  <->  P";
by Tac.depth;
showResult();

(*3*)
goal "~(P-->Q) --> (Q-->P)";
by Tac.depth;
showResult();

(*4*)
goal "(~P-->Q)  <->  (~Q --> P)";
by Tac.depth;
showResult();

(*5*)
goal "((P|Q)-->(P|R)) --> (P|(Q-->R))";
by Tac.depth;
showResult();

(*6*)
goal "P | ~ P";
by Tac.depth;
showResult();

(*7*)
goal "P | ~ ~ ~ P";
by Tac.depth;
showResult();

(*8.  Peirce's law*)
goal "((P-->Q) --> P)  -->  P";
by Tac.depth;
showResult();

(*9*)
goal "((P|Q) & (~P|Q) & (P| ~Q)) --> ~ (~P | ~Q)";
by Tac.depth;
showResult();

(*10*)
goal "(Q-->R) & (R-->P&Q) & (P-->Q|R) --> (P<->Q)";
by Tac.depth;
showResult();

(*11.  Proved in each direction (incorrectly, says Pelletier!!)  *)
goal "P<->P";
by Tac.depth;
showResult();

(*12.  "Dijkstra's law"*)
goal "((P <-> Q) <-> R)  <->  (P <-> (Q <-> R))";
by Tac.depth;
showResult();

(*13.  Distributive law*)
goal "P | (Q & R)  <-> (P | Q) & (P | R)";
by Tac.depth;
showResult();

(*14*)
goal "(P <-> Q) <-> ((Q | ~P) & (~Q|P))";
by Tac.depth;
showResult();

(*15*)
goal "(P --> Q) <-> (~P | Q)";
by Tac.depth;
showResult();

(*16*)
goal "(P-->Q) | (Q-->P)";
by Tac.depth;
showResult();

(*17*)
goal "((P & (Q-->R))-->S)  <->  ((~P | Q | S) & (~P | ~R | S))";
by Tac.depth;
showResult();


print"Problem 18. \n";
goal "EX y. ALL x. P(y)-->P(x)";
by Tac.depth;
showResult(); 



print"Problem 19. \n";
goal "EX x. ALL y. ALL z. (P(y)-->Q(z)) --> (P(x)-->Q(x))";
by Tac.depth;
showResult();


print"Problem 20. \n";
goal "(ALL x. ALL y. EX z. ALL w. (P(x)&Q(y)-->R(z)&S(w)))      \
\   --> (EX x. EX y. P(x) & Q(y)) --> (EX z. R(z))";
by Tac.depth; 
showResult();



print"Problem 21.  \n";
goal "(EX x. P-->Q(x)) & (EX x. Q(x)-->P) --> (EX x. P<->Q(x))";
by Tac.depth; 
showResult();


print"Problem 22\n";
goal "(ALL x. P <-> Q(x))  -->  (P <-> (ALL x. Q(x)))";
by Tac.depth; 
showResult();


print"Problem 23\n";
goal "(ALL x. P | Q(x))  <->  (P | (ALL x. Q(x)))";
by Tac.depth; 
showResult();


print"Problem 24\n";
goal "~(EX x. S(x)&Q(x)) & (ALL x. P(x) --> Q(x)|R(x)) &        \
\    (~(EX x.P(x)) --> (EX x.Q(x))) & (ALL x. Q(x)|R(x) --> S(x)) \
\   --> (EX x. P(x)&R(x))";
by Tac.depth; 
showResult();


print"Problem 25\n";
goal "(EX x. P(x)) &                                    \
\       (ALL x. L(x) --> ~ (M(x) & R(x))) &             \
\       (ALL x. P(x) --> (M(x) & L(x))) &               \
\       ((ALL x. P(x)-->Q(x)) | (EX x. P(x)&R(x)))      \
\   --> (EX x. Q(x)&P(x))";
by Tac.depth; 
showResult();


print"Problem 26\n";
goal "((EX x. p(x)) <-> (EX x. q(x))) & \
\     (ALL x. ALL y. p(x) & q(y) --> (r(x) <-> s(y)))   \
\ --> ((ALL x. p(x)-->r(x)) <-> (ALL x. q(x)-->s(x)))";
by Tac.depth; 
showResult();


print"Problem 27\n";
goal "(EX x. P(x) & ~Q(x)) &    \
\       (ALL x. P(x) --> R(x)) &        \
\       (ALL x. M(x) & L(x) --> P(x)) & \
\       ((EX x. R(x) & ~ Q(x)) --> (ALL x. L(x) --> ~ R(x)))    \
\   --> (ALL x. M(x) --> ~L(x))";
by (Tac.depthIt 5); 
showResult();


print"Problem 28.  AMENDED\n";
goal "(ALL x. P(x) --> (ALL x. Q(x))) &                 \
\       ((ALL x. Q(x)|R(x)) --> (EX x. Q(x)&S(x))) &    \
\       ((EX x.S(x)) --> (ALL x. L(x) --> M(x)))        \
\   --> (ALL x. P(x) & L(x) --> M(x))";
by Tac.depth;  
showResult();


print"Problem 29.  Essentially the same as Principia Mathematica *11.71\n";
goal "(EX x. P(x)) & (EX y. Q(y))                               \
\   --> ((ALL x. P(x)-->R(x)) & (ALL y. Q(y)-->S(y))   <->      \
\       (ALL x. ALL y. P(x) & Q(y) --> R(x) & S(y)))";
by Tac.depth; 
showResult();


print"Problem 30\n";
goal "(ALL x. P(x) | Q(x) --> ~ R(x)) & \
\       (ALL x. (Q(x) --> ~ S(x)) --> P(x) & R(x))      \
\   --> (ALL x. S(x))";
by Tac.depth;  
showResult();


print"Problem 31.  \n";
goal "~(EX x.P(x) & (Q(x) | R(x))) &    \
\       (EX x. L(x) & P(x)) &           \
\       (ALL x. ~ R(x) --> M(x))        \
\   --> (EX x. L(x) & M(x))";
by Tac.depth;
showResult();


print"Problem 32.  \n";
goal "(ALL x. P(x) & (Q(x)|R(x))-->S(x)) &      \
\       (ALL x. S(x) & R(x) --> L(x)) &         \
\       (ALL x. M(x) --> R(x))  \
\   --> (ALL x. P(x) & M(x) --> L(x))";
by Tac.depth;
showResult();


print"Problem 33\n";
goal "(ALL x. P(a) & (P(x)-->P(b))-->P(c))  <-> \
\    (ALL x. (~P(a) | P(x) | P(c)) & (~P(a) | ~P(b) | P(c)))";
by Tac.depth;
showResult();

print"Problem 34  AMENDED (TWICE!!)\n";
(*Andrews's challenge*)
goal "((EX x. ALL y. p(x) <-> p(y))  <->                \
\      ((EX x. q(x)) <-> (ALL y. p(y))))     <->        \
\     ((EX x. ALL y. q(x) <-> q(y))  <->                \
\      ((EX x. p(x)) <-> (ALL y. q(y))))";
by Tac.depth;
showResult();


print"Problem 35.  \n";
goal "EX x. EX y. P(x,y) -->  (ALL u. ALL v. P(u,v))";
by Tac.depth;
showResult();


print"Problem 36. \n";
goal "(ALL x. EX y. J(x,y)) &                   \
\     (ALL x. EX y. G(x,y)) &                   \
\     (ALL x. ALL y. J(x,y) | G(x,y) -->        \
\     (ALL z. J(y,z) | G(y,z) --> H(x,z)))      \
\ --> (ALL x. EX y. H(x,y))";
by Tac.depth;
showResult();


print"Problem 37\n";
goal "(ALL z. EX w. ALL x. EX y.                                        \
\          (P(x,z)-->P(y,w)) & P(y,z) & (P(y,w) --> (EX u.Q(u,w)))) &   \
\       (ALL x. ALL z. ~P(x,z) --> (EX y. Q(y,z))) &                    \
\       ((EX x. EX y. Q(x,y)) --> (ALL x. R(x,x)))                      \
\   --> (ALL x. EX y. R(x,y))";
by Tac.depth;
showResult();


print"Problem 38. NOT PROVED\n";
goal "(ALL x. p(a) & (p(x) --> (EX y. p(y) & r(x,y))) -->               \
\              (EX z. EX w. p(z) & r(x,w) & r(w,z)))  <->               \
\     (ALL x. (~p(a) | p(x) | (EX z. EX w. p(z) & r(x,w) & r(w,z))) &   \
\             (~p(a) | ~(EX y. p(y) & r(x,y)) |                         \
\            (EX z. EX w. p(z) & r(x,w) & r(w,z))))";
(*NOT PROVED*)

print"Problem 39\n";
goal "~ (EX x. ALL y. J(x,y) <-> ~J(y,y))";
by Tac.depth;
showResult();


print"Problem 40. AMENDED\n";
goal "(EX y. ALL x. J(y,x) <-> ~J(x,x))  -->         \
\       ~ (ALL x. EX y. ALL z. J(z,y) <-> ~ J(z,x))";
by Tac.depth;
showResult();


print"Problem 41\n";
goal "(ALL z. (EX y. (ALL x. f(x,y) <-> f(x,z) & ~ f(x,x))))    \
\     --> ~ (EX z. ALL x. f(x,z))";
by (Tac.depthIt 1);
showResult();


print"Problem 42\n";
goal "~ (EX y. ALL x. p(x,y) <-> ~ (EX z. p(x,z) & p(z,x)))";
by (Tac.depthIt 5);
showResult();

print"Problem 43 NOT PROVED\n";
goal "(ALL x. ALL y. q(x,y) <-> (ALL z. p(z,x) <-> p(z,y)))     \
\     --> (ALL x. (ALL y. q(x,y) <-> q(y,x)))";
(*NOT PROVED*)

print"Problem 44\n";
goal "(ALL x. f(x) -->                                                  \
\             (EX y. g(y) & h(x,y) & (EX y. g(y) & ~ h(x,y))))  &       \
\     (EX x. j(x) & (ALL y. g(y) --> h(x,y)))                           \
\     --> (EX x. j(x) & ~f(x))";
by Tac.depth;
showResult();


print"Problem 45\n";
goal "(ALL x. f(x) & (ALL y. g(y) & h(x,y) --> j(x,y))          \
\                     --> (ALL y. g(y) & h(x,y) --> k(y))) &    \
\     ~ (EX y. l(y) & k(y)) &                                   \
\     (EX x. f(x) & (ALL y. h(x,y) --> l(y))                    \
\                  & (ALL y. g(y) & h(x,y) --> j(x,y)))         \
\     --> (EX x. f(x) & ~ (EX y. g(y) & h(x,y)))";
by Tac.depth;
showResult();


print"Problem 46\n";
goal "(ALL x. f(x) & (ALL y. f(y) & h(y,x) --> g(y)) --> g(x)) &        \
\     ((EX x.f(x) & ~g(x)) -->                                  \
\      (EX x. f(x) & ~g(x) & (ALL y. f(y) & ~g(y) --> j(x,y)))) &       \
\     (ALL x. ALL y. f(x) & f(y) & h(x,y) --> ~j(y,x))                  \
\      --> (ALL x. f(x) --> g(x))";
by Tac.depth;
showResult();


(* Example suggested by Johannes Schumann and credited to Pelletier *)
goal "(ALL x. ALL y. ALL z. P(x,y) --> P(y,z) --> P(x,z)) --> \
\     (ALL x. ALL y. ALL z. Q(x,y) --> Q(y,z) --> Q(x,z)) --> \
\     (ALL x. ALL y.Q(x,y) --> Q(y,x)) -->  \
\     (ALL x. ALL y. P(x,y) | Q(x,y)) --> \
\     (ALL x. ALL y.P(x,y)) | (ALL x. ALL y.Q(x,y))";
(*NOT PROVED*)

print"Problem 47  Schubert's Steamroller\n";
goal "(ALL x. P1(x) --> P0(x)) & (EX x.P1(x)) & \
\     (ALL x. P2(x) --> P0(x)) & (EX x.P2(x)) & \
\     (ALL x. P3(x) --> P0(x)) & (EX x.P3(x)) & \
\     (ALL x. P4(x) --> P0(x)) & (EX x.P4(x)) & \
\     (ALL x. P5(x) --> P0(x)) & (EX x.P5(x)) & \
\     (ALL x. Q1(x) --> Q0(x)) & (EX x.Q1(x)) & \
\     (ALL x. P0(x) --> ((ALL y.Q0(y)-->R(x,y)) |       \
\                     (ALL y.P0(y) & S(y,x) &   \
\                          (EX z.Q0(z)&R(y,z)) --> R(x,y)))) &  \
\     (ALL x. ALL y. P3(y) & (P5(x)|P4(x)) --> S(x,y)) &        \
\     (ALL x. ALL y. P3(x) & P2(y) --> S(x,y)) &        \
\     (ALL x. ALL y. P2(x) & P1(y) --> S(x,y)) &        \
\     (ALL x. ALL y. P1(x) & (P2(y)|Q1(y)) --> ~R(x,y)) &       \
\     (ALL x. ALL y. P3(x) & P4(y) --> R(x,y)) &        \
\     (ALL x. ALL y. P3(x) & P5(y) --> ~R(x,y)) &       \
\     (ALL x. (P4(x)|P5(x)) --> (EX y.Q0(y) & R(x,y)))  \
\     --> (EX x. EX y. P0(x) & P0(y) & (EX z. Q1(z) & R(y,z) & R(x,y)))";
(*NOT PROVED*)


print"Reached end of file.\n";
