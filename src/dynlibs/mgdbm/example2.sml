(* Example use of Gdbm persistent hashtables, part 2.
   The data base must exist.
   File mosml/src/dynlibs/mgdbm/example2.sml. *)

load "Gdbm";

open Gdbm;

val _ = print "Dumping the data base \"monthnames\" created by example 1:\n";

val _ = withtable ("monthnames", READER)
             (app (fn (k, v) => (print k; print "/"; print v; print "\n")));

val _ = print "\nNow try e.g.   lookup \"July\";\n\n";

fun lookup mn = 
    withtable ("monthnames", READER) (fn db => peek db mn);
