(* Example use of Gdbm persistent hashtables.
   File mosml/src/dynlibs/mgdbm/example1.sml. *)

load "Gdbm";

open Gdbm;

(* Some example data: *)

val _ = print "\nCreating a new database in file \"monthnames\".\n";

local 
val monthnames = 
    [("January", "janvier"), ("february", "fevrier"), ("March", "mars"), 
     ("April", "avril"), ("May", "mai"), ("June", "juin"),
     ("July", "juillet"), ("August", "aout"), ("September", "septembre"),
     ("October", "octobre"), ("November", "novembre"), ("December", "decembre")
     ];
in 					     

    val _ = withtable ("monthnames", NEWDB) 
	              (fn db => List.app (add db) monthnames);
end

val _ = print "Dumping database:\n";

val _ = withtable ("monthnames", READER)
             (app (fn (k, v) => (print k; print "/"; print v; print "\n")));

val _ = print "\nNow try e.g.   lookup \"July\";\n\n";

fun lookup mn = 
    withtable ("monthnames", READER) (fn db => peek db mn);

