(* mosml/src/dynlibs/mmysql/Mysql.sml -- version 0.09 of 2000-04-28
   thomassi@dina.kvl.dk and sestoft@dina.kvl.dk *)

open Dynlib;

exception Closed and Null;

(* Obtain a handle pointing to the library defining the C functions: *)

val dlh = dlopen { lib = "libmmysql.so",
		   flag = RTLD_LAZY, 
		   global = false }

prim_type dbconn_	(* an abstract object containing a MYSQL pointer *)

type dbconn = { conn : dbconn_, closed : bool ref }

prim_type dbresult_	(* a finalized object containing a MYSQL_RES pointer *)

(* One mysql function requires a dbconn where Postgres requires a
   dbresult.  Hence we include the dbconn_ in the Mysql.dbresult *)

type dbresult = dbconn_ * dbresult_ 

(* The alphabetical order of constructors below must agree with 
   function db_resultstatus in file mmysql.c *)

datatype dbresultstatus =
    Bad_response        (* (not used by mysql)                    *)    
  | Command_ok          (* The query was a command                *)
  | Copy_in             (* (not used by mysql)                    *)
  | Copy_out            (* (not used by mysql)                    *)
  | Empty_query
  | Fatal_error         (* (not used by mysql)                    *)
  | Nonfatal_error
  | Tuples_ok           (* The query successfully returned tuples *)

val db_setdb : { dbhost    : string option, (* 0  database server host *)
	         dbname    : string option, (* 1  database name        *)
		 dboptions : string option, (* 2  not used             *)
		 dbport    : int,           (* 3  database server port *)
		 dbpwd     : string option, (* 4  user passwd          *)
		 dbtty     : string option, (* 5  not used             *)
		 dbuser    : string option  (* 6  database user        *)
     	       } -> dbconn_ = 
    app1 (dlsym dlh "mysql_setdb")

val db_db : dbconn_ -> string = 
    app1 (dlsym dlh "db_db")

val db_host : dbconn_ -> string option = 
    app1 (dlsym dlh "db_host")

val db_options : dbconn_ -> string = 
    app1 (dlsym dlh "db_options")

val db_port : dbconn_ -> string = 
    app1 (dlsym dlh "db_port")

val db_tty : dbconn_ -> string = 
    app1 (dlsym dlh "db_tty")

val db_status : dbconn_ -> bool =
    app1 (dlsym dlh "db_status")

val db_errormessage : dbconn_ -> string option = 
    app1 (dlsym dlh "db_errormessage")

val db_finish : dbconn_ -> unit =
    app1 (dlsym dlh "db_finish")

val db_reset : dbconn_ -> unit =
    app1 (dlsym dlh "db_reset")

val db_exec : dbconn_ -> string -> dbresult_ =
    app2 (dlsym dlh "db_exec")

val db_resultstatus : dbconn_ -> dbresultstatus =
    app1 (dlsym dlh "db_resultstatus")

val db_ntuples : dbresult_ -> int =
    app1 (dlsym dlh "db_ntuples")

val db_cmdtuples : dbconn_ -> int =
    app1 (dlsym dlh "db_cmdtuples")

val db_nfields : dbresult_ -> int =
    app1 (dlsym dlh "db_nfields")

val db_fname : dbresult_ -> int -> string =
    app2 (dlsym dlh "db_fname")

val db_ftype : dbresult_ -> int -> int =
    app2 (dlsym dlh "db_ftype")

val db_fnumber : dbresult_ -> string -> int =
    app2 (dlsym dlh "db_fnumber")

val db_ftype : dbresult_ -> int -> int =
    app2 (dlsym dlh "db_ftype")

val db_getint : dbresult_ -> int -> int -> int =
    app3 (dlsym dlh "db_getint")

val db_getreal : dbresult_ -> int -> int -> real =
    app3 (dlsym dlh "db_getreal")

val db_getstring : dbresult_ -> int -> int -> string =
    app3 (dlsym dlh "db_getstring")

val db_getbool : dbresult_ -> int -> int -> bool =
    app3 (dlsym dlh "db_getbool")

val db_isnull : dbresult_ -> int -> int -> bool =
    app3 (dlsym dlh "db_getisnull")

(* Auxiliaries *)

fun errormsg conn =
    case db_errormessage conn of
	NONE     => raise Fail ("Mysql: unknown error")
      | SOME msg => raise Fail ("Mysql: " ^ msg)


fun openbase { dbhost    : string option, (* 0  database server host *)
	       dbname    : string option, (* 1  database name        *)
	       dboptions : string option, (* 2  options              *)
	       dbport    : string option, (* 3  database server port *)
	       dbpwd     : string option, (* 4  user passwd          *)
	       dbtty     : string option, (* 5  not used             *)
	       dbuser    : string option  (* 6  database user        *)
	     } : dbconn =
    case dbname of 
	NONE   => raise Fail "Mysql.openbase: dbname required"
      | SOME _ => 
	    let val port = case Option.mapPartial Int.fromString dbport of
		                NONE => 0
			      | SOME port => port
		val conn = db_setdb { 
				      dbhost    = dbhost,
				      dbname    = dbname,
				      dboptions = dboptions,
				      dbport    = port,
				      dbpwd     = dbpwd, 
				      dbtty     = dbtty,
				      dbuser    = dbuser }
	    in 
		{ conn = conn, closed = ref false } 
	    end

fun closebase { conn : dbconn_, closed : bool ref } =
    (db_finish conn; closed := true)

fun checknot (ref false) = ()
  | checknot (ref true)  = raise Closed

(* Monitoring the database connection *)

fun db {conn, closed} = 
    (checknot closed; db_db conn)

fun host {conn, closed} = 
    (checknot closed; db_host conn)

fun options {conn, closed} = 
    (checknot closed; db_options conn)

fun port {conn, closed} = 
    (checknot closed; db_port conn)

fun tty {conn, closed} = 
    (checknot closed; db_tty conn)

fun status {conn, closed} = 
    (checknot closed; db_status conn)

fun errormessage {conn, closed} : string option = 
    (checknot closed; db_errormessage conn)

fun reset {conn, closed} = 
    (checknot closed; db_reset conn)

(* Query execution and result set access *)

fun execute {conn, closed} query : dbresult =
    (checknot closed; ((conn, db_exec conn query) 
		       handle Fail _ => errormsg conn))

fun resultstatus (dbconn, _) =
    db_resultstatus dbconn

fun ntuples (_, dbres) =
    db_ntuples dbres

fun cmdtuples (dbconn, _) = 
    db_cmdtuples dbconn

fun nfields (_, dbres) =
    db_nfields dbres

fun fname (_, dbres) fno =
    db_fname dbres fno

fun fnames (dbres as (_, dbres_)) =
    Vector.tabulate(nfields dbres, db_fname dbres_)

fun fnumber (_, dbres) fname =
    let val i = db_fnumber dbres fname 
    in if i < 0 then NONE else SOME i end

fun getint (_, dbres) fno tupno =
    if tupno < 0 andalso fno < 0 then 
	raise Fail "Mysql.getint: negative tuple or field number"
    else if db_isnull dbres tupno fno then
	raise Null
    else
	db_getint dbres tupno fno

fun getreal (_, dbres) fno tupno =
    if tupno < 0 andalso fno < 0 then 
	raise Fail "Mysql.getreal: negative tuple or field number"
    else if db_isnull dbres tupno fno then
	raise Null
    else
	db_getreal dbres tupno fno

fun getstring (_, dbres) fno tupno =
    if tupno < 0 andalso fno < 0 then 
	raise Fail "Mysql.getstring: negative tuple or field number"
    else if db_isnull dbres tupno fno then
	raise Null
    else
	db_getstring dbres tupno fno 

(* Scanning dates and times *)

local					
    open Substring (* for getc, all *)
    fun getint src = Option.valOf (Int.scan StringCvt.DEC getc src)
    fun drop p     = StringCvt.dropl p getc
in
    fun scandate (src : Substring.substring)  =
	let fun isSep c  = (c = #"-")
	    val (year,  src1) = getint src
	    val (month, src2) = getint (drop isSep src1)
	    val (day,   src3) = getint (drop isSep src2)
	in ((year, month, day), src3) end

    fun scantime (src : Substring.substring) =
	let fun isSep c  = (c = #":")
	    val (hour, src1) = getint src
	    val (min,  src2) = getint (drop isSep src1)
	    val (sec,  src3) = getint (drop isSep src2)
	in ((hour, min, sec), src3) end

    fun db_getdatetime dbres fno tupno : Date.date =
	let val src = Substring.all (db_getstring dbres fno tupno) 
	    val ((yr,mo,da), src1) = scandate src
	    val src2 = drop (fn c => c = #" ") src1
	    val ((hr,mi,se), _   ) = scantime src2
	    open Date
	    val tomonth = 
		fn 1 => Jan | 2 => Feb |  3 => Mar |  4 => Apr
		 | 5 => May | 6 => Jun |  7 => Jul |  8 => Aug
		 | 9 => Sep | 10 => Oct | 11 => Nov | 12 => Dec
		 | _ => raise Fail "Mysql.db_getdatetime 1";
	in date {year=yr, month=tomonth mo, day=da, 
		 hour=hr, minute=mi, second=se, offset=NONE} end
        handle Option.Option => raise Fail "Mysql.db_getdatetime 2"

    fun db_gettime dbres fno tupno : int * int * int =
	#1(scantime(Substring.all(db_getstring dbres fno tupno)))
	handle Option.Option => raise Fail "Mysql.db_gettime"

    fun db_getdate dbres fno tupno : int * int * int =
	#1(scandate (Substring.all (db_getstring dbres fno tupno)))
	handle Option.Option => raise Fail "Mysql.db_getdate"
end

fun getdatetime (_, dbres) fno tupno =
    if db_isnull dbres tupno fno then
	raise Null
    else
	db_getdatetime dbres tupno fno

fun gettime (_, dbres) fno tupno =
    if db_isnull dbres tupno fno then
	raise Null
    else
	db_gettime dbres tupno fno

fun getdate (_, dbres) fno tupno =
    if db_isnull dbres tupno fno then
	raise Null
    else
	db_getdate dbres tupno fno

fun getbool (_, dbres) fno tupno =
    if tupno < 0 andalso fno < 0 then 
	raise Fail "Mysql.getbool: negative tuple or field number"
    else if db_isnull dbres tupno fno then
	raise Null
    else
	db_getbool dbres tupno fno 

fun isnull (_, dbres) fno tupno =
    if tupno >= 0 andalso fno >= 0 then 
	db_isnull dbres tupno fno
    else
	raise Fail "Mysql.isnull: negative tuple or field number"

fun isnull' (_, dbres) fno tupno =	(* Assume fno >= 0 already checked *)
    if tupno >= 0 then 
	db_isnull dbres tupno fno
    else
	raise Fail "Mysql.isnull': negative tuple number"

datatype dynval =
    Int of int				(* mysql int4            *)
  | Real of real			(* mysql float8 (float4) *)
  | String of string			(* mysql text (varchar)  *)
  | Date of int * int * int             (* mysql date yyyy-mm-dd *)
  | Time of int * int * int             (* mysql time hh:mm:ss   *)
  | DateTime of Date.date               (* mysql datetime        *)
  | NullVal				(* mysql NULL            *)

datatype dyntype = 
    IntTy               (* ML int               mysql int4              *)
  | RealTy              (* ML real              mysql float8, float4    *)
  | StringTy            (* ML string            mysql text, varchar     *) 
  | DateTy              (* ML (yyyy, mth, day)  mysql date              *)
  | TimeTy              (* ML (hh, mm, ss)      mysql time              *)
  | DateTimeTy          (* ML Date.date         mysql datetime, abstime *)
  | UnknownTy

(* A translation from Mysql types to Moscow ML types.
   
   NB!: The numbers below need to correspond to the
        numbers in mmysql.c *)

fun totag 0      = SOME IntTy      (* FIELD_TYPE_DECIMAL *)
  | totag 1      = SOME IntTy      (* FIELD_TYPE_TINY *)
  | totag 2      = SOME IntTy      (* FIELD_TYPE_SHORT *)
  | totag 3      = SOME IntTy      (* FIELD_TYPE_LONG *)
  | totag 4      = SOME RealTy     (* FIELD_TYPE_FLOAT *)
  | totag 5      = SOME RealTy     (* FIELD_TYPE_DOUBLE *)
  | totag 6      = SOME UnknownTy  (* FIELD_TYPE_NULL *)
  | totag 7      = SOME DateTimeTy (* FIELD_TYPE_TIMESTAMP *)
  | totag 8      = SOME IntTy      (* FIELD_TYPE_LONGLONG *)  
  | totag 9      = SOME IntTy 	   (* FIELD_TYPE_INT24 *)     
  | totag 10     = SOME DateTy     (* FIELD_TYPE_DATE *)      
  | totag 11     = SOME TimeTy     (* FIELD_TYPE_TIME *)      
  | totag 12     = SOME DateTimeTy (* FIELD_TYPE_DATETIME *)  
  | totag 13     = SOME DateTy     (* FIELD_TYPE_YEAR *)      
  | totag 14     = SOME DateTy     (* FIELD_TYPE_NEWDATE *)   
  | totag 15     = SOME UnknownTy  (* FIELD_TYPE_ENUM *)      
  | totag 16     = SOME UnknownTy  (* FIELD_TYPE_SET *)       
  | totag 17     = SOME StringTy   (* FIELD_TYPE_TINY_BLOB *) 
  | totag 18     = SOME StringTy   (* FIELD_TYPE_MEDIUM_BLOB *)
  | totag 19     = SOME StringTy   (* FIELD_TYPE_LONG_BLOB *) 
  | totag 20     = SOME StringTy   (* FIELD_TYPE_BLOB *)      
  | totag 21     = SOME StringTy   (* FIELD_TYPE_VAR_STRING *)
  | totag 22     = SOME StringTy   (* FIELD_TYPE_STRING *)    
  | totag _      = NONE            (* NB. Unknown Type *)    

(* Translation from Moscow ML types to Mysql types: *)

fun fromtag IntTy      = "long"
  | fromtag RealTy     = "double"
  | fromtag StringTy   = "text"
  | fromtag DateTy     = "date"
  | fromtag TimeTy     = "time"
  | fromtag DateTimeTy = "datetime"
  | fromtag UnknownTy  = raise Fail "Mysql.fromtag"

fun typeof tyname = 
    case totag tyname of
	NONE     => UnknownTy
      | SOME tag => tag
		
fun ftype (_, dbres) fno = 
    if fno >= 0 then 
	typeof (db_ftype dbres fno)
    else
	raise Fail "Mysql.ftype: negative field number"

fun ftypes dbres =
    Vector.tabulate(nfields dbres, ftype dbres)

fun getdynfield (dbres as (_, dbres_)) fno : int -> dynval =
    case ftype dbres fno of
	IntTy    => (fn tupno => 
		     if isnull' dbres fno tupno then NullVal
		     else Int (db_getint dbres_ tupno fno))
      | RealTy   => (fn tupno => 
		     if isnull' dbres fno tupno then NullVal
		     else Real (db_getreal dbres_ tupno fno))
      | StringTy => (fn tupno => 
		     if isnull' dbres fno tupno then NullVal
		     else String (db_getstring dbres_ tupno fno))
      | TimeTy     => (fn tupno => 
		       if db_isnull dbres_ tupno fno then NullVal
		       else Time (db_gettime dbres_ tupno fno))
      | DateTy     => (fn tupno => 
		       if db_isnull dbres_ tupno fno then NullVal
		       else Date (db_getdate dbres_ tupno fno))
      | DateTimeTy => (fn tupno => 
		       if db_isnull dbres_ tupno fno then NullVal
		       else DateTime (db_getdatetime dbres_ tupno fno))
      | UnknownTy => 
	raise Fail ("Mysql.getdynfield: unknown type") 
(*      | _ => raise Fail "Mysql.getdynfield: unknown type" *)

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
    fun dynval2s (Int i)        = Int.toString  i
      | dynval2s (Real r)       = Real.toString r
      | dynval2s (String s)     = s
      | dynval2s (Date ymd)     = fmttrip "-" ymd
      | dynval2s (Time hms)     = fmttrip ":" hms
      | dynval2s (DateTime dt)  = Date.toString dt
      | dynval2s NullVal        = "NULL"
end


(* Implements "copy <tablename> to stdout" : *)

fun toisodate (year, month, day) = 
    String.concat
         [Int.toString year, "-",
	  StringCvt.padLeft #"0" 2 (Int.toString month), "-", 
	  StringCvt.padLeft #"0" 2 (Int.toString day)]

fun totime (hh, mm, ss) = 
    String.concat
         [StringCvt.padLeft #"0" 2 (Int.toString hh), ":", 
	  StringCvt.padLeft #"0" 2 (Int.toString mm), ":",
	  StringCvt.padLeft #"0" 2 (Int.toString ss)]
    
fun replaceminus s = 
    String.map (fn #"~" => #"-" | c => c) s

fun dynvaltostring (Int i)          = replaceminus(Int.toString i)
  | dynvaltostring (Real r)         = replaceminus(Real.toString r)
  | dynvaltostring (String str)     = str
  | dynvaltostring (Date date)      = toisodate date
  | dynvaltostring (Time time)      = totime time
  | dynvaltostring (DateTime dt)    = Date.fmt "%Y-%m-%d %H:%M:%S" dt
  | dynvaltostring (NullVal)        = "\\N"
(*  | dynvaltostring _              = raise Fail ("dynvaltostring: unknown dynval")
*)
fun copytableto (dconn as { conn, closed } : dbconn,
		 tablename : string,
		 put : string -> unit) : unit =
    let val res = execute dconn ("select * from " ^ tablename );
	val all = getdyntups res;

	fun printdyntup tuple =
	    let fun printtabvalue (fv, r) = 
		    "\t" :: dynvaltostring fv :: r
		fun mkstring [] = ""
		  | mkstring (_ :: tail) = String.concat tail
            in
		put (mkstring (Vector.foldr printtabvalue [] tuple))
            end
    in 
	Vector.app printdyntup all
    end

(* Implements "copy <tablename> from stdin" : *)

fun copytablefrom (dconn as { conn, closed } : dbconn,
		   tablename : string,
		   useput : (string -> unit) -> unit) : unit =
    let open List Substring
	fun isnewline c = (c = #"\n")
	fun istab c     = (c = #"\t")

	fun removetrailingnewlines str = 
	    dropr isnewline (all str)

        fun split orgstr = 
	    fields istab orgstr

	fun joinsus pr sep []       = []
	  | joinsus pr sep [x]      = pr x
	  | joinsus pr sep (x1::xr) =
	    let fun loop []       = []
		  | loop (y1::yr) = sep :: pr y1 @ loop yr
	    in pr x1 @ loop xr end

	val comma = all ","
	val quote = all "'"

        (* NB: Special case to handle NULL=\\N values *)
	fun enquote sus = 
	    if size sus = 2 andalso string sus = "\\N" then [sus]
	    else [quote, sus, quote]

	fun quoteandcommaseparate suss = 
	    Substring.concat (joinsus enquote comma suss)

        fun processline (suss : substring list) =
	    let (* Optimize for the frequent case where put is called
		   once for each line (including newline), or once for 
		   the body of the line and once for the newline: *)
		val linesus = 
		    case suss of
			[sus] => sus 
		      | _     => all (concat suss)
		val fields = split linesus
		val quotedfields = quoteandcommaseparate fields
		val query = 
		    ["INSERT INTO ", tablename, " VALUES (", quotedfields, ")"]
	    in 
		execute dconn (String.concat query);
		()
	    end

	val strbuf = ref [] : substring list ref
	
	fun processfrags [] = ()
	  | processfrags [more] = 
	    strbuf := more :: !strbuf
	  | processfrags (linetail :: next :: more) =
	    let val linefrags = if isEmpty linetail then !strbuf 
				else linetail :: !strbuf
	    in 
		processline (List.rev linefrags);
		strbuf := [];
		if isEmpty next then
		    processfrags more
		else
		    processfrags (next :: more)
	    end

	fun put linefrag : unit = 
	    processfrags(fields isnewline (all linefrag))
    in 
	useput put
        (* Should we do something about possible left-overs in strbuf?  
	   They must be missing their terminating newline...  *)
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
