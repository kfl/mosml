(* File mosml/examples/cgi/cgiex2.sml 

   Example CGI program, which may be invoked by the webserver to
   handle requests transmitted from the example HTML FORM in file
   upload.html.  See file README2 for instructions.

 *)

open Mosmlcgi;

fun prtint i = print (Int.toString i)
fun prtline sus = (print (Substring.string sus); print "\n")
fun lines s = Substring.fields (fn c => c = #"\n") (Substring.all s)

fun processfile filedata =
    (case part_data (valOf (cgi_part "action")) of
	 "count characters" => 
	     (print "File contains "; 
	      prtint (size filedata); 
	      print " characters\n")
       | "count lines" => 
	     (print "File contains "; 
	      prtint (length (lines filedata)); 
	      print " lines\n")
       | "sort" => 
	     (print "Sorted file:<P>\n<PRE>";
	      app prtline (Listsort.sort Substring.compare (lines filedata));
	      print "</PRE>\n")
       | _ => 
	     print "Unknown action\n")
    handle Option => print "No action\n"

val _ = 
    (print "Content-type: text/html\n\n";
     print "<HTML><BODY>";
     (case cgi_part "filecontents" of
	  SOME part => processfile (part_data part)
	| NONE      => print "No file\n");
     print "</BODY></HTML>")
