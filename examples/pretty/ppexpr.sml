(* File mosml/examples/pretty/ppexpr.sml 
 * Example demonstrating installable prettyprinters.
 *
 * Try it out by running 
 *	mosml ppexpr.sml
 * from the command line.
 *)

val _ = (load "PP"; load "Int");

datatype expr = 
    Cst of int 
  | Neg of expr
  | Plus of expr * expr

fun ppexpr pps e0 = 
    let open PP
        fun ppe (Cst i)        = add_string pps (Int.toString i)
          | ppe (Neg e)        = (add_string pps "~"; ppe e)
          | ppe (Plus(e1, e2)) = (begin_block pps CONSISTENT 0;
                                  add_string pps "(";
                                  ppe e1; 
                                  add_string pps " + ";
                                  add_break pps (0, 1);
                                  ppe e2; 
                                  add_string pps ")";
                                  end_block pps)
    in
        begin_block pps INCONSISTENT 0; 
        ppe e0;
        end_block pps
    end

val e1 = Cst 1;
val e2 = Cst 2;
val e3 = Plus(e1, Neg e2);
val e4 = Plus(Neg e3, e3);
val e5 = Plus(Neg e4, e4);
val e6 = Plus(e5, e5);
val e7 = Plus(e6, e6);
val e8 = Plus(e3, Plus(e3, Plus(e3, Plus(e3, Plus(e3, Plus(e3, e7))))));

val _ = installPP ppexpr;

val _ = print 
    "\nNow a pretty-printer for arithmetic expressions has been installed.\n\
     \To see it in action, evaluate e.g.\n\
     \  e1;\n  e3;\n  Plus(e1,e3);\n  e5;\n  e6;\n  e7;\n";
    
