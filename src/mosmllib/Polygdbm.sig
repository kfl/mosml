(* Polygdbm -- GNU gdbm persistent polymorphic hashtables -- requires Dynlib *)

type ('key, 'data) table 

exception NotFound
exception AlreadyThere
exception NotWriter
exception Closed
exception GdbmError of string

val withtable  : string * Gdbm.openmode -> (('key, 'data) table -> 'a) -> 'a
val add        : ('key, 'data) table -> 'key * 'data -> unit 
val insert     : ('key, 'data) table -> 'key * 'data -> unit
val find       : ('key, 'data) table -> 'key -> 'data
val peek       : ('key, 'data) table -> 'key -> 'data option
val hasKey     : ('key, 'data) table -> 'key -> bool
val remove     : ('key, 'data) table -> 'key -> unit
val listKeys   : ('key, 'data) table -> 'key list
val numItems   : ('key, 'data) table -> int
val listItems  : ('key, 'data) table -> ('key * 'data) list
val app        : ('key * 'data -> unit) -> ('key, 'data) table -> unit
val map        : ('key * 'data -> 'a) -> ('key, 'data) table -> 'a list
val fold       : ('key * 'data * 'a -> 'a) -> 'a -> ('key, 'data) table -> 'a
val fastwrite  : bool ref    
val reorganize : ('key, 'data) table -> unit

(* 
   [('key, 'data) table] is the type of an opened table with keys of
   type 'key and associated values of type 'data.  The actual values
   of type 'key and 'data cannot contain function closures or abstract
   values.  Values involving references (even circular values) can be
   stored, but the identity of references is preserved only with every
   single key or value stored, not across several different values.

   The Polygdbm table files of are not portable across platforms,
   because word size and endianness affects the lay-out of values.

   A value of type table can be used only in the argument f to the
   withtable function.  This makes sure that the table is closed after
   use.

   [withtable (nam, mod) f] first opens the table db in file nam with
   mode mod, then applies f to db, then closes db.  Makes sure to
   close db even if an exception is raised during the evaluation of
   f(db).  Raises GdbmError with an informative message in case the
   table cannot be opened.  E.g. the table cannot be opened for
   reading if already opened for writing, and cannot be opened for
   writing if already opened for reading.

   [add db (k,v)] adds the pair (k, v) to db.  Raises AlreadyThere if
   there is a pair (k, _) in db already.  Raises NotWriter if db is
   not opened in write mode.

   [insert db (k, v)] adds the pair (k, v) to db, replacing any pair
   (k, _) at k if present.  Raises NotWriter if db is not opened in
   write mode.

   [find(db, k)] returns v if the pair (k, v) is in db; otherwise
   raises NotFound.

   [peek db k] returns SOME v if the pair (k, v) is in db; otherwise
   returns NONE.

   [hasKey(db, k)] returns true if there is a pair (k, _) in db;
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
