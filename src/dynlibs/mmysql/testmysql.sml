(* Testing the Mysql interface -- 1999-08-08, 2000-05-30 *)

(* Test number 5a fails because MySQL reports the number of affected
   rows incorrectly after an SQL delete operation.  This is evident
   also when using the MySQL interactive monitor.  This test has been
   disabled below; see MYSQLCMDTUPLES *)

(* MySQL does not implement boolean fields correctly, so the bool
tests have been disabled below; see MYSQLBOOLERROR *)

app load ["Int", "Mysql", "Mosml"];

use "../../mosmllib/test/auxil.sml";

open Mysql;

val dbhost    = SOME "localhost";
val dbport    = NONE;
val dboptions = NONE;
val dbtty     = NONE;
val dbuser    = NONE;
val dbpwd     = NONE;
val dbname    = SOME "test";

val pc = openbase { dbhost = dbhost, dbport = dbport, 
		    dboptions = dboptions, dbtty = dbtty, 
		    dbuser = dbuser, dbpwd = dbpwd,
		    dbname = dbname };

val info = (db pc, host pc, options pc, port pc, tty pc)

val _ = (execute pc "drop table t"; ()) handle Fail _ => ();

val _ = execute pc "create table t (fb bool, fi int4, ff8 float8,\
 \ ff4 float4, ftx text, fv varchar(80), fd date, ftm time, fdt datetime)";

fun inst tup = execute pc ("insert into t values " ^ tup)

val res1 = inst "('false', 1234, 1234.1, 1234.2, 'Abc dEf', 'Abc DEF',\
                 \ '1998-12-24', '23:59:42', '1975-06-25 13:45:56')"

val test1a = check' 
    (fn _ => resultstatus res1 = Command_ok 
             andalso cmdtuples res1 = 1)

val test1b = check'
    (fn _ => ntuples res1 = 0 andalso nfields res1 = 0)

val test1c = 
    (fname res1 0; "WRONG")
    handle Fail _ => "OK" | _ => "WRONG";

val test1d = check'
    (fn _ => fnames res1 = #[])

val test1e = check'
    (fn _ => fnumber res1 "" = NONE)

val test1f = (getstring res1 0 0; "WRONG")
             handle Fail _ => "OK" | _ => "WRONG";

val test1g = (isnull res1 0 0; "WRONG")
             handle Fail _ => "OK" | _ => "WRONG";

val test1h = (ftype res1 0; "WRONG")
             handle Fail _ => "OK" | _ => "WRONG";

val test1i = check'
    (fn _ => case ftypes res1 of #[] => true | _ => false)

val test1j = check'
    (fn _ => case getdyntups res1 of #[] => true | _ => false)

val test1k = check'
    (fn _ => case getdyntup res1 0 of #[] => true | _ => false)

val test1l = (getdynfield res1 0 0; "WRONG")
             handle Fail _ => "OK" | _ => "WRONG";

val res2 = inst "('true', -1234, -1234.1, -1234.2, '', '',\
                 \ '1752-03-01', '04:59:42', NULL)"

val res3 = execute pc "select * from t order by fi";

val test3a = check'
    (fn _ => resultstatus res3 = Tuples_ok 
     andalso errormessage pc = NONE
     andalso ntuples res3 = 2 
     andalso nfields res3 = 9)

local 
    val fieldnames = ["fb", "fi", "ff8", "ff4", "ftx", "fv", 
		      "fd", "ftm", "fdt"];
in
val test3b = check'
    (fn _ => fieldnames = List.tabulate(nfields res3, fname res3)
             andalso Vector.fromList fieldnames = fnames res3);

val test3c = check'
    (fn _ => List.map (fnumber res3) fieldnames 
             = List.tabulate(nfields res3, SOME));
end;

local

val tups3 = getdyntups res3

(* MYSQLBOOLERROR: the vector pattern should have Bool(vb) in the
   first position, and the tuple should have vb as the first component: *)

fun vcheck #[_ (* Bool vb *), Int vi , Real vf8, Real vf4, String vtx, 
	     String vv, Date vd, Time vt, v8] val0_7 val8 =
    ((* vb, *) vi, vf8, vf4, vtx, vv, vd, vt) = val0_7
    andalso (case (v8, val8) of 
		 (NullVal, NullVal) => true
	       | (DateTime d1, DateTime d2) => Date.compare(d1, d2) = EQUAL
	       | (_, _) => false)
  | vcheck _ _ _ = false;

val date = Date.date { year = 1975, month = Date.Jun, day = 25, 
		       hour = 13, minute = 45, second = 56, offset = NONE};

fun checkbounds2 ok fcn fno tupno = 
    check'(fn _ => (fcn res3 fno tupno; ok) 
	           handle Fail _ => not ok)

fun checkallbounds2 ok fno tupno =
    (checkbounds2 ok getbool fno tupno,
     checkbounds2 ok getint fno tupno,
     checkbounds2 ok getreal fno tupno,
     checkbounds2 ok getstring fno tupno,
     checkbounds2 ok getdate fno tupno,
     checkbounds2 ok gettime fno tupno,
     checkbounds2 ok getdatetime fno tupno,
     checkbounds2 ok isnull fno tupno)

fun checkbounds1 ok fcn fno = 
    check'(fn _ => (fcn res3 fno; ok) 
	           handle Fail _ => not ok)

fun checkallbounds1 ok fno =
    (checkbounds1 ok ftype fno,
     checkbounds1 ok fname fno)
in

(* MYSQLBOOLERROR: the result tuples below should have false or true
   in the first position *)

val test3d = check'
    (fn _ => Vector.length tups3 = 2
     andalso Vector.length (Vector.sub(tups3, 0)) = 9
     andalso vcheck 
             (Vector.sub(tups3, 0)) 
	     (~1234, ~1234.1, ~1234.2, "", "", (1752, 3, 1), (4, 59, 42))
	     NullVal
     andalso vcheck 
             (Vector.sub(tups3, 1)) 
	     (1234, 1234.1, 1234.2, "Abc dEf", "Abc DEF", 
	      (1998, 12, 24), (23, 59, 42))
	     (DateTime date)
     andalso vcheck 
             (getdyntup res3 0)
	     (~1234, ~1234.1, ~1234.2, "", "", (1752, 3, 1), (4, 59, 42))
	     NullVal
     andalso vcheck 
             (getdyntup res3 1)
	     (1234, 1234.1, 1234.2, "Abc dEf", "Abc DEF", 
	      (1998, 12, 24), (23, 59, 42))
	     (DateTime date)
     andalso vcheck 
             (Vector.map (applyto 0) (Vector.tabulate(nfields res3, getdynfield res3)))
	     (~1234, ~1234.1, ~1234.2, "", "", (1752, 3, 1), (4, 59, 42))
	     NullVal
     andalso vcheck 
             (Vector.map (applyto 1) (Vector.tabulate(nfields res3, getdynfield res3)))
	     (1234, 1234.1, 1234.2, "Abc dEf", "Abc DEF", 
	      (1998, 12, 24), (23, 59, 42))
	     (DateTime date))

val test3e = check'
    (fn _ => (* getbool res3 0 0 = true
     andalso *) getint res3 1 0 = ~1234     
     andalso getreal res3 2 0 = ~1234.1
     andalso getreal res3 3 0 = ~1234.2
     andalso getstring res3 4 0 = ""
     andalso getstring res3 5 0 = ""
     andalso getdate res3 6 0 = (1752, 3, 1)
     andalso gettime res3 7 0 = (4, 59, 42)
     andalso (getdatetime res3 8 0; false)
	      handle Null => true | _ => false
     andalso isnull res3 8 0);

val test3f = check'
    (fn _ => getbool res3 0 1 = false
     andalso getint res3 1 1 = 1234     
     andalso getreal res3 2 1 = 1234.1
     andalso getreal res3 3 1 = 1234.2
     andalso getstring res3 4 1 = "Abc dEf"
     andalso getstring res3 5 1 = "Abc DEF"
     andalso getdate res3 6 1 = (1998, 12, 24)
     andalso gettime res3 7 1 = (23, 59, 42)
     andalso Date.compare(getdatetime res3 8 1, date) = EQUAL
     andalso not (isnull res3 8 1));

val test3ga = checkallbounds2 false 0 2
val test3gb = checkallbounds2 false 0 ~1
val test3gc = checkallbounds2 false 9 0
val test3gd = checkallbounds2 false ~1 0
val test3ha = checkallbounds1 false ~1
val test3hb = checkallbounds1 false 9
val test3hc = checkallbounds1 true 8

end;

local 
    (* A collector (app, ret) provides a function app for collecting data
       in a list, and a function ret to return the data: *)
    fun collector () : (string -> unit) * (unit -> string list) =
	let val buf = ref []
	    fun append s  = buf := s :: !buf
	    fun return () = rev (!buf)
	in (append, return) end

    (* MYSQLBOOLERROR: really the two leading zeroes below should 
       be f and t, respectively *)
    val expected =   
	["0\t1234\t1234.1\t1234.2\tAbc dEf\tAbc DEF\t1998-12-24\t23:59:42\
	 \\t1975-06-25 13:45:56",
	 "0\t-1234\t-1234.1\t-1234.2\t\t\t1752-03-01\t04:59:42\t\\N"]
    val (append1, return1) = collector ()
    val (append2, return2) = collector ()
    val (append3, return3) = collector ()
    val (append4, return4) = collector ()
    val (append5, return5) = collector ()
    val (append6, return6) = collector ()
in

val test4 = check'(fn _ => (copytableto (pc, "t", append1); 
			    expected = return1 ()))

val res5 = execute pc "delete from t";
 
val test5a = check' 
    (fn _ => errormessage pc = NONE
     (* MYSQLCMDTUPLES: andalso cmdtuples res5 = 2 *))

val test6 = check'(fn _ => (copytableto (pc, "t", append2); 
			    [] = return2 ()))

val _ = copytablefrom (pc, "t", 
		       fn put => app (fn s => (put s; put "\n"))  expected)

val test7 = check'(fn _ => (copytableto (pc, "t", append3); 
			    expected = return3 ()));

val _ = execute pc "delete from t";

val _ = copytablefrom (pc, "t", 
		       fn put => app (fn s => (app (put o str) (explode s); 
					       put "\n")) expected)

val test8 = check'(fn _ => (copytableto (pc, "t", append4); 
			    expected = return4 ()));

val _ = execute pc "delete from t";

val _ = copytablefrom (pc, "t", 
		       fn put => app (fn s => put (s ^ "\n")) expected)

val test9 = check'(fn _ => (copytableto (pc, "t", append5); 
			    expected = return5 ()));

val _ = execute pc "delete from t";

val _ = copytablefrom (pc, "t", 
		       fn put => 
		       put (foldr (fn (line, res) => line ^ "\n" ^ res) 
			          "" 
				  expected))

val test10 = check'(fn _ => (copytableto (pc, "t", append6); 
			    expected = return6 ()));

val _ = (execute pc "DROP TABLE large"; ()) handle Fail _ => ();

val test11a = execute pc "CREATE TABLE large (nr INT)";

val test11b = 
    let fun f 0 = ()
	  | f n = (execute pc ("INSERT INTO large VALUES (" ^ 
			       Int.toString n ^ ")"); f (n-1))
    in f 50000 end;

(* Function getall used to take 189 sec, now takes 0.3 sec, on 
   AMD K7 600 MHz (2000-05-30): 
*)

fun getall _ = let val dbres = execute pc "SELECT * FROM large"
	       in Mosml.time getdyntups dbres end;

val test11c = List.app (ignore o getall) (List.tabulate(15, fn i => i))

end;

(*
fun gettuplist dbres (getfields : int -> 'a) : 'a list =
    List.tabulate(ntuples dbres, getfields)

fun gettupvec dbres (getfields : int -> 'a) : 'a Vector.vector =
    Vector.tabulate(ntuples dbres, getfields)
*)

val _ = quit();
