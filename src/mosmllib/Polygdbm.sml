(* Polygdbm.sml, version 0.3, 1997-11-08 *)

type ('key, 'data) table = Gdbm.table

prim_val string_val_ : 'a -> string = 1 "string_mlval";
prim_val val_string_ : string -> 'a = 1 "mlval_string";

val fastwrite = Gdbm.fastwrite

exception NotFound = Gdbm.NotFound
exception AlreadyThere = Gdbm.AlreadyThere
exception NotWriter = Gdbm.NotWriter
exception Closed = Gdbm.Closed
exception GdbmError = Gdbm.GdbmError

fun withtable namemode transact = 
    Gdbm.withtable namemode transact

fun add db (k, v) = Gdbm.add db (string_val_ k, string_val_ v) 

fun insert db (k, v) = Gdbm.insert db (string_val_ k, string_val_ v)

fun find db k = val_string_ (Gdbm.find db (string_val_ k))

fun peek db k = 
    case Gdbm.peek db (string_val_ k) of
	NONE   => NONE
      | SOME v => SOME (val_string_ v)

fun hasKey db k = Gdbm.hasKey db (string_val_ k)

fun remove db k = Gdbm.remove db (string_val_ k)

fun listKeys db = List.map val_string_ (Gdbm.listKeys db)

fun numItems db = Gdbm.numItems db

fun listItems db = 
    Gdbm.map (fn (k, v) => (val_string_ k, val_string_ v)) db

fun app f db = 
    Gdbm.app (fn (k, v) => f (val_string_ k, val_string_ v)) db

fun map f db = 
    Gdbm.map (fn (k, v) => f (val_string_ k, val_string_ v)) db

fun fold f a db = 
    Gdbm.fold (fn (k, v, res) => f (val_string_ k, val_string_ v, res)) a db

fun reorganize db = Gdbm.reorganize db
