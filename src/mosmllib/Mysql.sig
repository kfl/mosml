(* Mysql -- interface to the MySQL database server -- requires Dynlib *)

type dbconn                                   (* Connection to server *)
type dbresult                                 (* Result of a query    *)

exception Closed                              (* Connection is closed *)
exception Null                                (* Field value is NULL  *)

(* Opening, closing, and maintaining database connections *)

val openbase : { dbhost    : string option,   (* database server host *)
                 dbname    : string option,   (* database name        *)
                 dboptions : string option,   (* (not used by MySQL)  *)
                 dbport    : string option,   (* database server port *)
                 dbpwd     : string option,   (* user passwd          *)
                 dbtty     : string option,   (* (not used by MySQL)  *)
                 dbuser    : string option    (* database user        *)
               } -> dbconn

val closebase    : dbconn -> unit
val db           : dbconn -> string
val host         : dbconn -> string option
val options      : dbconn -> string
val port         : dbconn -> string
val tty          : dbconn -> string

val status       : dbconn -> bool
val reset        : dbconn -> unit
val errormessage : dbconn -> string option

(* Query execution and result set information *)

datatype dbresultstatus =
    Bad_response            (* (not used by mysql)                    *)
  | Command_ok              (* The query was a command                *)
  | Copy_in                 (* (not used by mysql)                    *)
  | Copy_out                (* (not used by mysql)                    *)
  | Empty_query
  | Fatal_error             (* (not used by mysql)                    *)
  | Nonfatal_error
  | Tuples_ok               (* The query successfully returned tuples *)

val execute      : dbconn -> string -> dbresult
val resultstatus : dbresult -> dbresultstatus
val ntuples      : dbresult -> int
val cmdtuples    : dbresult -> int
val nfields      : dbresult -> int
val fname        : dbresult -> int -> string
val fnames       : dbresult -> string vector
val fnumber      : dbresult -> string -> int option

(* Accessing the fields of a resultset *)

val getint       : dbresult -> int -> int -> int
val getreal      : dbresult -> int -> int -> real
val getstring    : dbresult -> int -> int -> string
val getdate      : dbresult -> int -> int -> int * int * int   (* Y M D *)
val gettime      : dbresult -> int -> int -> int * int * int   (* H M S *)
val getdatetime  : dbresult -> int -> int -> Date.date
val getbool      : dbresult -> int -> int -> bool
val isnull       : dbresult -> int -> int -> bool

datatype dynval =
    Int of int                          (* MySQL int4            *)
  | Real of real                        (* MySQL float8 (float4) *)
  | String of string                    (* MySQL text (varchar)  *)
  | Date of int * int * int             (* MySQL date yyyy-mm-dd *)
  | Time of int * int * int             (* MySQL time hh:mm:ss   *)
  | DateTime of Date.date               (* MySQL datetime        *)
  | NullVal                             (* MySQL NULL value      *)

val getdynfield  : dbresult -> int -> int -> dynval
val getdyntup    : dbresult -> int -> dynval vector
val getdyntups   : dbresult -> dynval vector vector 
val dynval2s     : dynval -> string

(* Bulk copying to or from a table *)

val copytableto   : dbconn * string * (string -> unit) -> unit
val copytablefrom : dbconn * string * ((string -> unit) -> unit) -> unit

(* Some standard ML and MySQL types: *)

datatype dyntype = 
    IntTy               (* ML int               MySQL int4              *)
  | RealTy              (* ML real              MySQL float8, float4    *)
  | StringTy            (* ML string            MySQL text, varchar     *) 
  | DateTy              (* ML (yyyy, mth, day)  MySQL date              *)
  | TimeTy              (* ML (hh, mm, ss)      MySQL time              *)
  | DateTimeTy          (* ML Date.date         MySQL datetime, abstime *)
  | UnknownTy

val fromtag : dyntype -> string
val ftype   : dbresult -> int -> dyntype
val ftypes  : dbresult -> dyntype Vector.vector

val applyto : 'a -> ('a -> 'b) -> 'b

(* Formatting the result of a database query as an HTML table *)

val formattable : dbresult -> Msp.wseq
val showquery   : dbconn -> string -> Msp.wseq

(*
   [dbconn] is the type of connections to a MySQL database.

   [dbresult] is the type of result sets from MySQL queries.

   [openbase { dbhost, dbport, dboptions, dbtty, dbname, dbuser, dbpwd }] 
   opens a connection to a MySQL database server on the given host
   (default the local one) on the given port (default ?), to the given
   database (defaults to the user's login name), for the given user
   name (defaults to the current user's login name), and the given
   password (default none).  The result is a connection which may be
   used in subsequent queries.  In MySQL, unlike PostgreSQL, the
   dboptions and dbtty fields are not used.

   [closebase dbconn] closes the database connection.  No further
   queries can be executed.

   [db dbconn] returns the name of the database.

   [host dbconn] returns SOME h, where h is the database server host
   name, if the connection uses the Internet; returns NONE if the
   connection is to a socket on the local server.

   [options dbconn] returns the options given when opening the database.

   [port dbconn] returns the port number of the connection.

   [tty dbconn] returns the name of the tty used for logging.

   [status dbconn] returns true if the connection is usable, false
   otherwise.

   [reset dbconn] attempts to close and then reopen the connection to
   the database server.

   [errormessage dbconn] returns NONE if no error occurred, and SOME msg
   if an error occurred, where msg describes the error.

   [execute dbconn query] sends an SQL query to the database server
   for execution, and returns a resultset dbres.
   
   [resultstatus dbres] returns the status of the result set dbres.
   After a select query that succeeded, it will be Tuples_ok.
   
   [ntuples dbres] returns the number of tuples in the result set
   after a query.

   [cmdtuples dbres] returns the number of tuples affected by an
   insert, update, or delete SQL command.

   [nfields dbres] returns the number of fields in each tuple after a
   query.

   [fname dbres fno] returns the name of field number fno (in the
   result set after a query).  The fields are numbered 0, 1,...

   [fnames dbres] returns a vector of the field names (in the result
   set after a query).

   [fnumber dbres fname] returns SOME i where i is the number (0, 1,
   ...) of the field called fname (in the result set after a query),
   if the result set contains such a field name; returns NONE otherwise.

   [ftype dbres fno] returns the dyntype of field number fno (in the
   result set after a query).

   [ftypes dbres] returns a vector of the dyntypes (in the result set
   after a query).

   [fromtag dt] returns the name of the preferred MySQL type used
   to represent values of the dyntype dt.  This may be used when
   building `create table' statements.

   [getint dbres fno tupno] returns the integer value of field number
   fno in tuple tupno of result set dbres.  Raises Null if the value
   is NULL.

   [getreal dbres fno tupno] returns the floating-point value of field
   number fno in tuple tupno of result set dbres.  Raises Null if the
   value is NULL.

   [getstring dbres fno tupno] returns the string value of field
   number fno in tuple tupno of result set dbres.  Raises Null if the
   value is NULL.

   [getdate dbres fno tupno] returns the date (yyyy, mth, day) value
   of field number fno in tuple tupno of result set dbres.  Raises
   Null if the value is NULL.  Raises Fail if the field cannot be
   scanned as a date.

   [gettime dbres fno tupno] returns the time-of-day (hh, mm, ss)
   value of field number fno in tuple tupno of result set dbres.
   Raises Null if the value is NULL.  Raises Fail if the field cannot
   be scanned as a time.

   [getdatetime dbres fno tupno] returns the Date.date value of field
   number fno in tuple tupno of result set dbres.  Raises Null if the
   value is NULL.  Raises Fail if the field cannot be scanned as a
   date.

   [getbool dbres fno tupno] returns the boolean value of field number
   fno in tuple tupno of result set dbres.  Raises Null if the value
   is NULL.

   [isnull dbres fno tupno] returns true if the value of field number
   fno in tuple tupno of result set dbres is NULL; false otherwise.

   [getdynfield dbres fno tupno] returns the value of field number fno
   in tuple tupno of result set dbres as a dynval (a wrapped value).
   A NULL value is returned as NullVal.  Note that the partial
   application  (getdynfield dbres fno)  precomputes the type of the 
   field fno.  Hence it is far more efficient to compute 
        let val getfno = getdynfield dbres fno
        in tabulate(ntuples dbres, getfno) end
   than to compute
        let fun getfno tupno = getdynfield dbres fno tupno
        in tabulate(ntuples dbres, getfno) end
   because the latter repeatedly computes the type of the field.

   [getdyntup dbres tupno] returns the fields of tuple tupno in result
   set dbres as a vector of dynvals.

   [getdyntups dbres] returns all tuples of result set dbres as a
   vector of vectors of dynvals.

   [dynval2s dv] returns a string representing the dynval dv.

   [applyto x f] computes f(x).  This is convenient for applying
   several functions (given in a list or vector) to the same value:
      map (applyto 5) (tabulate(3, getdynfield dbres))
   equals 
      [getdynfield dbres 0 5, getdynfield dbres 1 5, getdynfield dbres 2 5]

   [copytableto(dbconn, tablename, put)] simulates a PostgreSQL "COPY
   TABLE TO" statement, applies the function put to every tuple of the
   table, represented as a line of text (not terminated by newline
   \n), and cleans up at the end.  For instance, to copy the contents
   of a table t to a text stream s (one tuple on each line), define
      fun put line = 
          (TextIO.output(s, line); TextIO.output(s, "\n"))
   and execute
      copytableto(dbconn, "t", put).

   [copytablefrom(dbconn, tablename, useput)] simulates a PostgreSQL
   "COPY TABLE FROM" statement, creates a put function for copying
   lines to the table, passes the put function to useput, and cleans
   up at the end.  The put function may be called multiple times for
   each line (tuple); the end of each line is indicated by the
   newline character "\n" as usual.  For instance, to copy the
   contents of a text stream s to a table t, define
      fun useput put = 
          while not (TextIO.endOfStream s) do put(TextIO.inputLine s);
   and execute
      copytablefrom(dbconn, "t", useput).
   Note that TextIO.inputLine preserves the newline at the end of each 
   line.  

   [formattable dbresult] returns a wseq representing an HTML table.
   The HTML table has a column for every field in the dbresult.  The
   first row is a table header giving the names of the fields in the
   dbresult.  The remaining rows correspond to the tuples in the
   dbresult, in the order they are provided by the database server.
   Null fields are shown as NULL.

   [showquery dbconn query] sends the SQL query to the database
   server, then uses formattable to format the result of the query.
*)
