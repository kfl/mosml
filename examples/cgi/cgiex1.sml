(* File mosml/examples/cgi/cgiex1.sml 

   Example CGI program, which may be invoked by the webserver to
   handle requests transmitted from the example HTML FORM in file
   htmlform.html.  See file README1 for instructions.

 *)

open Mosmlcgi;

fun prtint i = print (Int.toString i)
fun prtline sus = (print (Substring.string sus); print "\n")
fun lines s = Substring.fields (fn c => c = #"\n") (Substring.all s)

fun processfile filedata =
    (case valOf (cgi_field_string "action") of
	 "count characters" => 
	     (print "Text contains "; 
	      prtint (size filedata); 
	      print " characters\n")
       | "count lines" => 
	     (print "Text contains "; 
	      prtint (length (lines filedata)); 
	      print " lines\n")
       | "sort" => 
	     (print "Sorted text:<P>\n<PRE>";
	      app prtline (Listsort.sort Substring.compare (lines filedata));
	      print "</PRE>\n")
       | _ => 
	     print "Unknown action\n")
    handle Option => print "No action\n"

val _ = 
    (print "Content-type: text/html\n\n";
     print "<HTML><BODY>";
     (case cgi_field_string "texttoprocess" of
	  SOME data => processfile data
	| NONE      => print "No text to process\n");
     print "</BODY></HTML>")
