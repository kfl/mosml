(* Testing the Gdbm structure, PS 1997-11-08  *)

app load ["Int", "Gdbm", "Listsort"];
open Gdbm;

use "../../mosmllib/test/auxil.sml";

(* Operations on an empty table *)

val test1a = check'(fn _ => 
		    withtable ("empty", NEWDB) (fn db => true));

val test1b = check'(fn _ => 
    (withtable ("empty", WRITER) (fn db => find db "not there"); false)
     handle NotFound => true | _ => false);

val test1c = check'(fn _ => 
    withtable ("empty", WRITER) (fn db => peek db "not there" = NONE))

val test1d = check'(fn _ => 
    withtable ("empty", READER) (fn db => not (hasKey db "not there")))

val test1e = check'(fn _ => 
    (withtable ("empty", WRITER) (fn db => remove db "not there"); false)
    handle NotFound => true | _ => false)

val test1f = check'(fn _ => 
    withtable ("empty", READER) listKeys = [])

val test1g = check'(fn _ => 
    withtable ("empty", READER) listItems = [])

val test1h = check'(fn _ => 
    withtable ("empty", READER) numItems = 0)

val test1i = check'(fn _ => 
    let val x = ref true
    in 
	withtable ("empty", READER) (app (fn (k, v) => x := false));
	!x
    end)

val test1j = check'(fn _ => 
    withtable ("empty", READER) (map (fn (k, v) => 117)) = [])

val test1k = check'(fn _ =>
    withtable ("empty", READER) (fold (fn (k, v, res) => k :: res) 
				 ["117"]) = ["117"])


(* Operations on a non-empty table *)

val pairs = [("Hans 1", "Rischel"), ("Hans 2", "Dybkjær"), 
	     ("Anders", "Kristensen")];

val test2a = check'(fn _ => 
    withtable ("testdb", NEWDB) (fn db => (List.app (add db) pairs; true)));

val test2b1 = check'(fn _ => 
    (withtable ("testdb", WRITER) (fn db => add db (List.nth(pairs, 0))); 
     false)
    handle AlreadyThere => true | _ => false)

val test2b2 = check'(fn _ => 
    (withtable ("testdb", READER) (fn db => add db (List.nth(pairs, 0))); 
     false)
    handle NotWriter => true | _ => false)

val test2c1 = check'(fn _ => 
    (withtable ("testdb", WRITER) (fn db => insert db ("Hans 1", "Jensen")); 
     true)
    handle _ => false)

val test2c2 = check'(fn _ => 
    (withtable ("testdb", READER) (fn db => insert db (List.nth(pairs, 0))); 
     false)
    handle NotWriter => true | _ => false)
    
val test2d = check'(fn _ => 
    (withtable ("testdb", WRITER) (fn db => find db "Hans 3"); false)
    handle NotFound => true | _ => false)

val test2e = check'(fn _ => 
    withtable ("testdb", WRITER) (fn db => find db "Hans 1" = "Jensen")
    handle _ => false)

val test2f = check'(fn _ => 
    withtable ("testdb", WRITER) (fn db => peek db "Hans 3" = NONE))

val test2g = check'(fn _ => 
    withtable ("testdb", WRITER) (fn db => peek db "Hans 2" = SOME "Dybkjær"))

val test2h = check'(fn _ => 
    withtable ("testdb", READER) (fn db => not (hasKey db "Hans 3")))

val test2i = check'(fn _ =>
    withtable ("testdb", READER) (fn db => hasKey db "Hans 1"))

val test2j1 = check'(fn _ => 
    withtable ("testdb", WRITER) (fn db => (remove db "Hans 1";
					    not (hasKey db "Hans 1"))))

val test2j2 = check'(fn _ => 
    (withtable ("testdb", WRITER) (fn db => remove db "Hans 1"); false)
     handle NotFound => true | _ => false)

val test2j3 = check'(fn _ => 
    (withtable ("testdb", READER) (fn db => remove db "Hans 1"); false)
     handle NotWriter => true | _ => false)

val test2j4 =check'(fn _ =>
    (withtable ("testdb", WRITER) (fn db => add db (List.nth(pairs, 0))); 
     true)
     handle _ => false)
	 
fun sortkvs kvs = 
     Listsort.sort (fn ((k1, _), (k2, _)) => String.compare(k1, k2)) kvs

fun sortks ks = 
     Listsort.sort String.compare ks

val pairorder = withtable ("testdb", READER) listItems

val test2k1 = check'(fn _ => 
    sortks (withtable ("testdb", READER) listKeys) 
    = sortks (List.map #1 pairs))

val test2k2 = check'(fn _ => 
    withtable ("testdb", READER) listKeys = List.map #1 pairorder)

val test2l = check'(fn _ => 
    sortkvs (withtable ("testdb", READER) listItems) 
    = sortkvs pairs)

val test2m = check'(fn _ => 
    withtable ("testdb", READER) numItems = 3)

val test2n = check'(fn _ => 
    let val x = ref []
    in 
	withtable ("testdb", READER) (app (fn kv => x := kv :: !x));
	!x = pairorder
    end)

val test2o = check'(fn _ => 
    withtable ("testdb", READER) (map #2) = List.map #2 pairorder)

val test2p = check'(fn _ =>
    withtable ("testdb", READER) (fold (fn (k, v, res) => (k, v) :: res) 
				  [("117", "Ole")]) 
    = List.foldr (op ::) [("117", "Ole")] pairorder)


(* Unable to open a table which is not there *)

val test3a = check'(fn _ => 
    (withtable ("nosuchtable", READER) (fn db => false))
    handle GdbmError "File open error" => true | _ => false)

val test3b = check'(fn _ => 
    (withtable ("nosuchtable", WRITER) (fn db => false))
    handle GdbmError "File open error" => true | _ => false)

(* Unable to open a table for reading because it is being written to *)

val test3c = check'(fn _ => 
    (withtable ("empty", WRITER) 
     (fn db1 => withtable ("empty", READER) (fn db => false)))
    handle GdbmError "Can't be reader" => true | _ => false);

(* Unable to open a table for writing because it is being read from *)

val test3d = check'(fn _ => 
    (withtable ("empty", READER) 
     (fn db1 => withtable ("empty", WRITER) (fn db => false)))
    handle GdbmError "Can't be writer" => true | _ => false);

val _ = quit();
