(* Gdbm -- GNU gdbm persistent string hashtables -- requires Dynlib *)

type table 

datatype openmode =
    READER                              (* read-only access (nonexclusive) *)
  | WRITER                              (* read/write, table must exist    *)
  | WRCREAT                             (* read/write, create if necessary *)
  | NEWDB                               (* read/write, create empty table  *)

type datum = string

exception NotFound
exception AlreadyThere
exception NotWriter
exception Closed
exception GdbmError of string

val withtable  : string * openmode -> (table -> 'a) -> 'a
val withtables : (string * openmode) list -> (table list -> 'a) -> 'a
val add        : table -> datum * datum -> unit 
val insert     : table -> datum * datum -> unit
val find       : table -> datum -> datum
val peek       : table -> datum -> datum option
val hasKey     : table -> datum -> bool
val remove     : table -> datum -> unit
val listKeys   : table -> datum list
val numItems   : table -> int
val listItems  : table -> (datum * datum) list
val app        : (datum * datum -> unit) -> table -> unit
val map        : (datum * datum -> 'a) -> table -> 'a list
val fold       : (datum * datum * 'a -> 'a) -> 'a -> table -> 'a
val fastwrite  : bool ref    
val reorganize : table -> unit

(* 
   [table] is the type of an opened table.  A value of type table can
   be used only in the argument f to the withtable function.  This
   makes sure that the table is closed after use.

   [openmode] is the type of opening modes.  Read-only access (READER)
   is non-exclusive; read/write access (WRITER, WRCREAT, NEWDB) is
   exclusive.
    
   [withtable (nam, mod) f] first opens the table db in file nam with
   mode mod, then applies f to db, then closes db.  Makes sure to
   close db even if an exception is raised during the evaluation of
   f(db).  Raises GdbmError with an informative message in case the
   table cannot be opened.  E.g. the table cannot be opened for
   reading if already opened for writing, and cannot be opened for
   writing if already opened for reading.

   A table is only guaranteed to work properly if created by withtable
   using open modes WRCREAT or NEWDB.  If you create a table by
   creating and then opening an empty file, then numItems, listKeys,
   listItems, etc. will raise an exception.

   [withtables nammod f], where nammod = [(nam1, mod1), ..., (namn, modn)], 
   is equivalent to 
        withtable (nam1, mod1) (fn db1 => 
            withtable (nam2, mod2) (fn db2 => 
                ...
                    f [db1, db2, ...]))
   That is, first opens the databases db1, db2, ... in that order in
   files nam1, nam2, ... with modes mod1, mod2, ..., then applies f to
   [db1, db2, ...], and finally closes [db1, db2, ...].  Makes sure to
   close all databases even if an exception is raised during the
   opening of db1, db2, ... or during the evaluation of f[db1, db2, ...].
 
   [add db (k,v)] adds the pair (k, v) to db.  Raises AlreadyThere if
   there is a pair (k, _) in db already.  Raises NotWriter if db is
   not opened in write mode.

   [insert db (k, v)] adds the pair (k, v) to db, replacing any pair
   (k, _) at k if present.  Raises NotWriter if db is not opened in
   write mode.

   [find db k] returns v if the pair (k, v) is in db; otherwise
   raises NotFound.

   [peek db k] returns SOME v if the pair (k, v) is in db; otherwise
   returns NONE.

   [hasKey db k] returns true if there is a pair (k, _) in db;
   otherwise returns false.

   [remove db k] deletes the pair (k, _) from the table if present;
   otherwise raises NotFound.  Raises NotWriter if db is not opened in
   write mode.

   [listKeys db] returns a list of all keys in db in an unspecified
   order.  

   [numItems db] is the number of (key, value) pairs in db.
   Equivalent to length(listKeys db).

   [listItems db] returns a list of all (key, value) pairs in db in some 
   order.  Equivalent to 
        List.map (fn key => (key, find(db,key))) (listKeys db)

   [app f db] is equivalent to List.app f (listItems db), provided the
   function f does not change the set of keys in the table.
   Otherwise the effect is unpredictable.

   [map f db] is equivalent to List.map f (listItems db), provided the
   function f does not change the set of keys in the table.
   Otherwise the result and effect are unpredictable.
   
   [fold f a db] is equivalent to 
        List.foldr (fn ((k, v), r) => f(k, v, r)) a (listItems db)
   provided the function f does not change the set of keys in the 
   table. Otherwise the result and effect are unpredictable.

   [fastwrite] can be set to speed up writes to a table.  By default,
   !fastwrite is false and every write to a table will be followed by
   file system synchronization.  This is safe, but slow if you perform
   thousands of writes.  However, if !fastwrite is true when calling
   withtable, then writes may not be followed by synchronization,
   which may speed up writes considerably.  In any case, the file
   system is synchronized before withtable returns.

   [reorganize db] has no visible effect, but may be called after a
   lot of deletions to shrink the size of the table file.
*)
