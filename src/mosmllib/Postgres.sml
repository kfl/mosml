(* mosml/src/dynlibs/mpq/Postgres.sml.  
   sestoft@dina.kvl.dk -- 1998 -- version 0.05 of 2000-04-28 *)

open Dynlib;

exception Closed and Null;

(* Obtain a handle pointing to the library defining the C functions: *)

val dlh = dlopen { lib = "libmpq.so",
		   flag = RTLD_LAZY, 
		   global = false }

prim_type pgconn_	(* an abstract object containing a PGconn pointer *)

type dbconn = { conn : pgconn_, closed : bool ref }

prim_type dbresult	(* a finalized object containing a PGResult pointer *)

(* The alphabetical order of constructors below must agree 
   with function pq_resultstatus in file mpq.c *)

datatype dbresultstatus =
    Bad_response            (* An unexpected response was received    *)
  | Command_ok              (* The query was a command                *)
  | Copy_in                 (* The query was "copy <table> from ..."  *)
  | Copy_out                (* The query was "copy <table> to ..."    *)
  | Empty_query
  | Fatal_error
  | Nonfatal_error
  | Tuples_ok               (* The query successfully returned tuples *)

val pq_setdb : { dbhost    : string option, (* 0  database server host *)
	         dbname    : string option, (* 1  database name        *)
	         dboptions : string option, (* 2  options              *)
	         dbport    : string option, (* 3  database server port *)
	         dbpwd     : string option, (* 4  user passwd          *)
	         dbtty     : string option, (* 5  tty for error log    *)
	         dbuser    : string option  (* 6  database user        *)
	       } -> pgconn_ = 
    app1 (dlsym dlh "pq_setdb")

val pq_db : pgconn_ -> string = 
    app1 (dlsym dlh "pq_db")

val pq_host : pgconn_ -> string option = 
    app1 (dlsym dlh "pq_host")

val pq_options : pgconn_ -> string = 
    app1 (dlsym dlh "pq_options")

val pq_port : pgconn_ -> string = 
    app1 (dlsym dlh "pq_port")

val pq_tty : pgconn_ -> string = 
    app1 (dlsym dlh "pq_tty")

val pq_status : pgconn_ -> bool = 
    app1 (dlsym dlh "pq_status")

val pq_errormessage : pgconn_ -> string option = 
    app1 (dlsym dlh "pq_errormessage")

val pq_finish : pgconn_ -> unit =
    app1 (dlsym dlh "pq_finish")

val pq_reset : pgconn_ -> unit =
    app1 (dlsym dlh "pq_reset")

val pq_exec : pgconn_ -> string -> dbresult =
    app2 (dlsym dlh "pq_exec")

val pq_resultstatus : dbresult -> dbresultstatus =
    app1 (dlsym dlh "pq_resultstatus")

val pq_ntuples : dbresult -> int =
    app1 (dlsym dlh "pq_ntuples")

val pq_cmdtuples : dbresult -> int =
    app1 (dlsym dlh "pq_cmdtuples")

val pq_nfields : dbresult -> int =
    app1 (dlsym dlh "pq_nfields")

val pq_fname : dbresult -> int -> string =
    app2 (dlsym dlh "pq_fname")

val pq_ftype : dbresult -> int -> int =
    app2 (dlsym dlh "pq_ftype")

val pq_fnumber : dbresult -> string -> int =
    app2 (dlsym dlh "pq_fnumber")

val pq_fsize : dbresult -> int -> int =
    app2 (dlsym dlh "pq_fsize")

val pq_getint : dbresult -> int -> int -> int =
    app3 (dlsym dlh "pq_getint")

val pq_getreal : dbresult -> int -> int -> real =
    app3 (dlsym dlh "pq_getreal")

val pq_getstring : dbresult -> int -> int -> string =
    app3 (dlsym dlh "pq_getstring")

val pq_getbool : dbresult -> int -> int -> bool =
    app3 (dlsym dlh "pq_getbool")

val pq_isnull : dbresult -> int -> int -> bool =
    app3 (dlsym dlh "pq_getisnull")

(* Auxiliaries *)

fun errormsg fcn conn =
    case pq_errormessage conn of
	NONE     => raise Fail ("Postgres." ^ fcn ^ ": unknown error")
      | SOME msg => raise Fail ("Postgres." ^ fcn ^ ": " ^ msg)

fun checknot (ref false) = ()
  | checknot (ref true)  = raise Closed

(* Monitoring the database connection *)

fun db {conn, closed} = 
    (checknot closed; pq_db conn)

fun host {conn, closed} = 
    (checknot closed; pq_host conn)

fun options {conn, closed} = 
    (checknot closed; pq_options conn)

fun port {conn, closed} = 
    (checknot closed; pq_port conn)

fun tty {conn, closed} = 
    (checknot closed; pq_tty conn)

fun status {conn, closed} = 
    (checknot closed; pq_status conn)

fun errormessage {conn, closed} : string option = 
    (checknot closed; pq_errormessage conn)

fun reset {conn, closed} = 
    (checknot closed; pq_reset conn)

(* Query execution and result set access *)

fun execute {conn, closed} query =
    (checknot closed; 
     (pq_exec conn query) handle Fail _ => errormsg "execute" conn)

fun resultstatus dbres =
    pq_resultstatus dbres

fun ntuples dbres =
    pq_ntuples dbres

fun cmdtuples dbres =
    pq_cmdtuples dbres

fun nfields dbres =
    pq_nfields dbres

fun fname dbres fno =
    pq_fname dbres fno

fun fnames dbres =
    Vector.tabulate(nfields dbres, pq_fname dbres)

fun fnumber dbres fname =
    let val i = pq_fnumber dbres fname 
    in if i < 0 then NONE else SOME i end

fun fsize dbres fno =
    pq_fsize dbres fno

fun getint dbres fno tupno =
    if pq_isnull dbres tupno fno then
	raise Null
    else
	pq_getint dbres tupno fno

fun getreal dbres fno tupno =
    if pq_isnull dbres tupno fno then
	raise Null
    else
	pq_getreal dbres tupno fno

fun getstring dbres fno tupno =
    if pq_isnull dbres tupno fno then
	raise Null
    else
	pq_getstring dbres tupno fno 

fun pq_getdatetime dbres fno tupno : Date.date =
    case Date.fromString (pq_getstring dbres fno tupno) of
	NONE    => raise Fail "Postgres.pq_getdatetime"
      | SOME dt => dt

fun getdatetime dbres fno tupno =
    if pq_isnull dbres tupno fno then
	raise Null
    else
	pq_getdatetime dbres tupno fno

fun pq_gettime dbres fno tupno : int * int * int =
    let val s = pq_getstring dbres fno tupno 
	open Substring (* for getc, all *)
	fun getint src = Option.valOf (Int.scan StringCvt.DEC getc src)
	fun drop p     = StringCvt.dropl p getc
	fun isSep c  = (c = #":")
	val (hour, src1) = getint (all s)
	val (min,  src2) = getint (drop isSep src1)
	val (sec,  src3) = getint (drop isSep src2)
    in 
	(hour, min, sec)
    end
    handle Option.Option => raise Fail "Postgres.pq_gettime"

fun gettime dbres fno tupno =
    if pq_isnull dbres tupno fno then
	raise Null
    else
	pq_gettime dbres tupno fno

fun pq_getdate dbres fno tupno : int * int * int =
    let val s = pq_getstring dbres fno tupno 
	open Substring (* for getc, all *)
	fun getint src = Option.valOf (Int.scan StringCvt.DEC getc src)
	fun drop p     = StringCvt.dropl p getc
	fun isSep c  = (c = #"-")
	val (month, src1) = getint (all s)
	val (day,   src2) = getint (drop isSep src1)
	val (year,  src3) = getint (drop isSep src2)
    in 
	(year, month, day)
    end
    handle Option.Option => raise Fail "Postgres.pq_getdate"

fun getdate dbres fno tupno =
    if pq_isnull dbres tupno fno then
	raise Null
    else
	pq_getdate dbres tupno fno

fun getbool dbres fno tupno =
    if pq_isnull dbres tupno fno then
	raise Null
    else
	pq_getbool dbres tupno fno 

fun isnull dbres fno tupno =
    pq_isnull dbres tupno fno

type oid = int (* In reality, a 32 bit quantity *)

datatype dynval =
    Bool of bool			(* psql bool            *)
  | Int of int				(* psql int4            *)
  | Real of real			(* psql float8, float4  *)
  | String of string			(* psql text, varchar   *)
  | Date of int * int * int		(* psql date yyyy-mm-dd *)
  | Time of int * int * int             (* psql time hh:mm:ss   *)
  | DateTime of Date.date		(* psql datetime        *)
  | Oid of oid				(* psql oid             *)
  | Bytea of Word8Array.array		(* psql bytea           *)
  | NullVal				(* psql NULL            *)

datatype dyntype = 
    BoolTy | IntTy | RealTy | StringTy | DateTy | TimeTy | DateTimeTy 
  | OidTy | ByteArrTy | UnknownTy of oid

(* A translation from Postgres types to Moscow ML types: *)

fun totag "bool"     = SOME BoolTy
  | totag "int4"     = SOME IntTy
  | totag "float8"   = SOME RealTy
  | totag "float4"   = SOME RealTy
  | totag "text"     = SOME StringTy
  | totag "varchar"  = SOME StringTy
  | totag "date"     = SOME DateTy
  | totag "datetime" = SOME DateTimeTy
  | totag "abstime"  = SOME DateTimeTy
  | totag "time"     = SOME TimeTy
  | totag "oid"      = SOME OidTy
  | totag "bytea"    = SOME ByteArrTy
  | totag _          = NONE
    
(* Translation from Moscow ML types to Postgres types: *)

fun fromtag BoolTy     = "bool"
  | fromtag IntTy      = "int4"
  | fromtag RealTy     = "float8"
  | fromtag StringTy   = "text"
  | fromtag TimeTy     = "time"
  | fromtag DateTy     = "date"
  | fromtag DateTimeTy = "datetime"
  | fromtag OidTy      = "oid"
  | fromtag ByteArrTy  = "bytea"
  | fromtag (UnknownTy _) = raise Fail "Postgres.fromtag"

local 
    val typetable = ref [] : (oid * dyntype) list ref
in
    fun settypetable pconn : unit = 
	let (* Get the oid-to-typname translation from PostgreSQL: *)
	    val res = execute pconn "select typname as type, oid from pg_type"
	    val tups = 
		List.tabulate(ntuples res, 
			      fn tupno => (pq_getstring res tupno 0, 
					   pq_getint res tupno 1))
	    fun addty ((tyname, oid), res) = 
		case totag tyname of
		    NONE     => res
		  | SOME tag => (oid, tag) :: res
	in 
	    typetable := List.foldr addty [] tups
	end
    
    fun typeof oid = 
	let fun h [] = UnknownTy oid
	      | h ((k, tyno) :: rest) = if k = oid then tyno else h rest
	in h (!typetable) end
end

fun ftype dbres fno = 
    typeof (pq_ftype dbres fno)

fun ftypes dbres =
    Vector.tabulate(nfields dbres, ftype dbres)

(* Copying to and from the database *)

val pq_endcopy : pgconn_ -> unit =
    app1 (dlsym dlh "pq_endcopy")

val pq_getline : pgconn_ -> string option =
    app1 (dlsym dlh "pq_getline")

(* Implements "copy <tablename> to stdout" : *)

fun copytableto (pconn as { conn, closed } : dbconn,
		 tablename : string,
		 put : string -> unit) : unit =
    let val res = execute pconn ("copy " ^ tablename ^ " to stdout");
	fun loop NONE     = ()
	  | loop (SOME s) = (put s; loop (pq_getline conn))
    in 
	case resultstatus res of
	    Copy_out => (loop (pq_getline conn); pq_endcopy conn)
	  | _        => raise Fail "Postgres.copytableto"
    end

val pq_putline : pgconn_ -> string -> unit =
    app2 (dlsym dlh "pq_putline")

(* Implements "copy <tablename> from stdin" : *)

fun copytablefrom (pconn as { conn, closed } : dbconn,
		tablename : string,
		useput : (string -> unit) -> unit) : unit =
    let val res = execute pconn ("copy " ^ tablename ^ " from stdin");
	val putline = pq_putline conn 
    in 
	case resultstatus res of
	    Copy_in => (useput putline;
			pq_putline conn "\\.\n"; 
			pq_endcopy conn)
	  | _       => raise Fail "Postgres.copytablefrom"
    end

fun openbase (args as
	      { dbhost    : string option, (* 0  database server host *)
	        dbname    : string option, (* 1  database name        *)
	        dboptions : string option, (* 2  options              *)
	        dbport    : string option, (* 3  database server port *)
	        dbpwd     : string option, (* 4  user passwd          *)
	        dbtty     : string option, (* 5  tty for error log    *)
	        dbuser    : string option  (* 6  database user        *)
		}) : dbconn =
    let val conn = pq_setdb args
    in 
	if pq_status conn then 
	    let val pconn = { conn = conn, closed = ref false } 
	    in (* The typetable is created anew for each database opened: *)
		settypetable pconn; 
		pconn 
	    end
	else
	    errormsg "openbase" conn
    end

fun closebase { conn : pgconn_, closed : bool ref } =
    (pq_finish conn; closed := true)

fun getdynfield dbres fno : int -> dynval =
    case ftype dbres fno of
	BoolTy     => (fn tupno => 
		       if pq_isnull dbres tupno fno then NullVal
		       else Bool (pq_getbool dbres tupno fno))
      | IntTy      => (fn tupno => 
		       if pq_isnull dbres tupno fno then NullVal
		       else Int (pq_getint dbres tupno fno))
      | RealTy     => (fn tupno => 
		       if pq_isnull dbres tupno fno then NullVal
		       else Real (pq_getreal dbres tupno fno))
      | StringTy   => (fn tupno => 
		       if pq_isnull dbres tupno fno then NullVal
		       else String (pq_getstring dbres tupno fno))
      | TimeTy     => (fn tupno => 
		       if pq_isnull dbres tupno fno then NullVal
		       else Time (pq_gettime dbres tupno fno))
      | DateTy     => (fn tupno => 
		       if pq_isnull dbres tupno fno then NullVal
		       else Date (pq_getdate dbres tupno fno))
      | DateTimeTy => (fn tupno => 
		       if pq_isnull dbres tupno fno then NullVal
		       else DateTime (pq_getdatetime dbres tupno fno))
      | OidTy     => raise Fail "Postgres.getdynfield: unimplemented: OidTy"
      | ByteArrTy => raise Fail "Postgres.getdynfield: unimplemented: ByteArrTy"
      | UnknownTy oid => 
	raise Fail ("Postgres.getdynfield: unknown type, oid = " 
		    ^ Int.toString oid)

fun applyto x f = f x

fun getdyntup dbres : int -> dynval vector = 
    let val getters = Vector.tabulate(nfields dbres, getdynfield dbres)
    in fn fieldno => Vector.map (applyto fieldno) getters end

fun getdyntups dbres : dynval vector vector =
    Vector.tabulate(ntuples dbres, getdyntup dbres)

local 
    fun i2s i = StringCvt.padLeft #"0" 2 (Int.toString i)
    fun fmttrip sep (a,b,c) = String.concat[i2s a, sep, i2s b, sep, i2s c]
in 
    fun dynval2s (Bool b)       = Bool.toString b
      | dynval2s (Int i)        = Int.toString  i
      | dynval2s (Real r)       = Real.toString r
      | dynval2s (String s)     = s
      | dynval2s (Date ymd)     = fmttrip "-" ymd
      | dynval2s (Time hms)     = fmttrip ":" hms
      | dynval2s (DateTime dt)  = Date.toString dt
      | dynval2s (Oid oid)      = "<oid>"
      | dynval2s (Bytea w8a)    = "<bytearray>"
      | dynval2s NullVal        = "NULL"
end

(* Formatting the result of database queries *)

local
    open Msp
    infix &&
in

fun formattable (dbres : dbresult) = 
    let fun (f o g) x = f (g x)
	val fieldnms = prmap (th o $) (vec2list (fnames dbres))
	fun fmtval dynval = 
	    case dynval of
		Int  _ => tda "ALIGN=RIGHT" ($ (dynval2s dynval))
	      | Real _ => tda "ALIGN=RIGHT" ($ (dynval2s dynval))
	      | _      => td ($ (dynval2s dynval))
	fun fmtrow tuple = tr(prmap fmtval (vec2list tuple))
	val tuples = vec2list (getdyntups dbres)
    in
	tablea "BORDER" (tr fieldnms && prsep Nl fmtrow tuples)
    end

fun showquery (dbconn : dbconn) (sql : string) : wseq = 
    let val dbres = execute dbconn sql
    in 
	case resultstatus dbres of
	    Tuples_ok => formattable dbres
	  | _         => $ "Error: Database query failed or was not a SELECT"
    end
    handle Fail msg => $ msg
end
