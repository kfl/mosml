(* Gdbm.sml, version 0.3, 1997-11-08 *)

prim_type table_

datatype openmode =
    READER | WRITER | WRCREAT | NEWDB

type table = 
    table_				(* Roughly, a GDBM_FILE             *)
    * openmode option ref		(* SOME mod if open, NONE if closed *)

type datum = string

(* Load dynamic libraries *)

open Dynlib

val dlh = dlopen {lib = "libmgdbm.so", global = false, flag = RTLD_LAZY };

val gdbm_open_ : string -> int -> int -> table_ 
    = app3 (dlsym dlh "mgdbm_open");

val gdbm_close_ : table_ -> unit 
    = app1 (dlsym dlh "mgdbm_close")

val gdbm_store_ : table_ -> string -> string -> int -> int
    = app4 (dlsym dlh "mgdbm_store")

val gdbm_fetch_ : table_ -> string -> string
    = app2 (dlsym dlh "mgdbm_fetch")

val gdbm_exists_ : table_ -> string -> bool
    = app2 (dlsym dlh "mgdbm_exists")

val gdbm_delete_ : table_ -> string -> bool
    = app2 (dlsym dlh "mgdbm_delete")

val gdbm_firstkey_ : table_ -> string
    = app1 (dlsym dlh "mgdbm_firstkey")

val gdbm_nextkey_ : table_ -> string -> string
    = app2 (dlsym dlh "mgdbm_nextkey")

val gdbm_numitems_ : table_ -> int 
    = app1 (dlsym dlh "mgdbm_numitems")

val gdbm_reorganize_ : table_ -> bool
    = app1 (dlsym dlh "mgdbm_reorganize")

val gdbm_error_ : unit -> string 
    = app1 (dlsym dlh "mgdbm_error")

val gdbm_constants_ : unit -> int * int * int * int * int * int * int 
    = app1 (dlsym dlh "mgdbm_constants")
  
val (GDBM_READER, GDBM_WRITER, GDBM_WRCREAT, GDBM_NEWDB, 
     GDBM_FAST, GDBM_INSERT, GDBM_REPLACE) = gdbm_constants_ ();

val fastwrite = ref false

exception NotFound
exception AlreadyThere
exception NotWriter
exception Closed
exception GdbmError of string

fun gdbm_error () = raise GdbmError (gdbm_error_ ()) 

local 
    fun openmode om =
	(if !fastwrite then GDBM_FAST else 0)
	+ (case om of 
	       READER  => GDBM_READER
	     | WRITER  => GDBM_WRITER
	     | WRCREAT => GDBM_WRCREAT
	     | NEWDB   => GDBM_NEWDB)
    fun opentable (name, mode) = 
	(gdbm_open_ name (openmode mode) ~1, ref (SOME mode))
	handle Fail "gdbm_open" => gdbm_error ()
    fun closedb (db, tmoptref) = (gdbm_close_ db; tmoptref := NONE)
    fun closedbs dbs = List.app closedb dbs
in
    fun withtable namemode transact = 
	let val db  = opentable namemode
	    val res = transact db handle exn => (closedb db; raise exn)
	in
	    closedb db; 
	    res
	end

    fun withtables namemodes transact =
	let fun loop [] = []
	      | loop (namemode :: rest) = 
	        let val db = opentable namemode
		in 
		    (db :: loop rest)
		    handle exn => (closedb db; raise exn)
		end
	    val dbs = loop namemodes
	    val res = transact dbs handle exn => (closedbs dbs; raise exn);
	in
	    closedbs dbs; 
	    res
	end
end

fun add (db, ref NONE) (k, v) = raise Closed
  | add (db, _) (k, v) = 
    case gdbm_store_ db k v GDBM_INSERT of
	~1 => raise NotWriter
      |  1 => raise AlreadyThere
      |  _ => ()

fun insert (db, ref NONE) (k, v) = raise Closed
  | insert (db, _) (k, v) =
    case gdbm_store_ db k v GDBM_REPLACE of
	~1 => raise NotWriter
      |  _ => ()

fun find (db, ref NONE) k = raise Closed
  | find (db, _) k = 
    gdbm_fetch_ db k
    handle Fail "gdbm_fetch" => raise NotFound

fun peek (db, ref NONE) k = raise Closed
  | peek (db, _)        k = 
    SOME(gdbm_fetch_ db k)
    handle Fail "gdbm_fetch" => NONE

fun hasKey (db, ref NONE) k = raise Closed
  | hasKey (db, _)        k = gdbm_exists_ db k 

fun remove (db, ref NONE)          k = raise Closed
  | remove (db, ref (SOME READER)) k = raise NotWriter
  | remove (db, _) k      =
    if gdbm_delete_ db k then
	()
    else
	raise NotFound

(* Internal use only: *)

fun gdbm_firstkey db = SOME(gdbm_firstkey_ db) handle Fail _ => NONE

fun gdbm_nextkey db k = SOME(gdbm_nextkey_ db k) handle Fail _ => NONE

fun listKeys (db, ref NONE) = raise Closed
  | listKeys (db, _) =
    let fun loop NONE       res = res
	  | loop (SOME key) res = 
	    let val next = gdbm_nextkey db key
	    in loop next (key :: res) end
    in loop (gdbm_firstkey db) [] end

(* This can be made more efficient in C, by immediate deallocation of keys: *)

fun numItems (db, ref NONE) = raise Closed
  | numItems (db, _) = gdbm_numitems_ db

fun listItems (db, ref NONE) = raise Closed
  | listItems (db, _) =
    let fun loop NONE       res = res
	  | loop (SOME key) res = 
	    let val next = gdbm_nextkey db key
	    in loop next ((key, gdbm_fetch_ db key) :: res) end
    in loop (gdbm_firstkey db) [] end

fun app f (db, ref NONE) = raise Closed
  | app f (db, _) =
    let fun loop NONE       = ()
	  | loop (SOME key) = 
	    (f (key, gdbm_fetch_ db key); 
	     loop (gdbm_nextkey db key))
    in loop (gdbm_firstkey db) end

fun map f (db, ref NONE) = raise Closed
  | map f (db, _) =
    let fun loop NONE       res = res
	  | loop (SOME key) res = 
	    loop (gdbm_nextkey db key) (f (key, gdbm_fetch_ db key) :: res)
    in loop (gdbm_firstkey db) [] end

fun fold f a (db, ref NONE) = raise Closed
  | fold f a (db, _) = 
    let fun loop NONE       res = res
	  | loop (SOME key) res = 
	    loop (gdbm_nextkey db key) (f (key, gdbm_fetch_ db key, res))
    in loop (gdbm_firstkey db) a end

fun reorganize (db, ref NONE) = raise Closed
  | reorganize (db, _) = 
    if gdbm_reorganize_ db then () 
    else gdbm_error ()
