(* test/string.sml 
   PS 1994-12-10, 1995-06-14, 1996-05-16 *)

use "auxil.sml";

local 
    open Char String

    val s1 = ""				(* size s1 =  0 *)
    and s2 = "ABCDE\tFGHI";		(* size s2 = 10 *)
    val ABCDE = List.map chr [65,66,67,68,69];  
in

val test1 = check'(fn _ => (size s1 = 0 andalso size s2 = 10));
val test2 = check'(fn _ => (sub(s2,6) = chr 70 andalso sub(s2,9) = chr 73));
val test3 = (sub(s1, 0)  seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test4 = (sub(s2, ~1) seq "WRONG") handle Subscript => "OK" | _ => "WRONG";
val test5 = (sub(s2, 10) seq "WRONG") handle Subscript => "OK" | _ => "WRONG";

val test6a = 
    check'(fn _ => "abcdef" = "abc" ^ "def" 
	   andalso "abc" = "" ^ "abc"
	   andalso "abc" = "abc" ^ ""
	   andalso "abc" = "" ^ "abc" ^ ""
	   andalso "" = "" ^ "")
val test6b = 
    check'(fn _ => 
	   "" = concat [] andalso "" = concat [s1] 
	   andalso "" = concat [""] andalso "" = concat ["", ""] 
	   andalso s2 = concat [s2] andalso s2^s2 = concat [s2,s2]
	   andalso s2 = concat ["", s2] andalso s2 = concat [s2,""]
	   andalso "ABCD" = concat ["A","B","C","D"]
	   andalso "ABCD" = concat ["A","B","", "C","D"]);
    
val test7 = check'(fn _ => "A" = str(chr 65));

val test8 = 
    check'(fn _ => 
	   "" = implode [] andalso "ABCDE" = implode ABCDE); 

val test9 = 
    check'(fn _ => 
	   [] = explode "" andalso ABCDE = explode "ABCDE");

val test10 = 
    check'(fn _ => 
	   s1 < s2 andalso s1 <= s1 
	   andalso s2 > s1 andalso s2 >=s2);

val test11a = 
    check'(fn _ => 
	   s2 = extract(s2, 0, SOME (size s2))
	   andalso s2 = extract(s2, 0, NONE)
	   andalso "" = extract(s2, size s2, SOME 0)
	   andalso "" = extract(s2, size s2, NONE)
	   andalso "" = extract(s1, 0, SOME 0)
	   andalso "" = extract(s1, 0, NONE));

val test11b = (extract(s2, ~1, SOME 0) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test11c = (extract(s2, 11, SOME 0) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test11d = (extract(s2, 0, SOME 11) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test11e = (extract(s2, 10, SOME 1) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test11f = (extract(s2, ~1, NONE) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test11g = (extract(s2, 11, NONE) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";

val test11h = 
    check'(fn _ => 
	   "ABCDE" = extract(s2, 0, SOME 5)
	   andalso "FGHI" = extract(s2, 6, SOME 4)
	   andalso "FGHI" = extract(s2, 6, NONE));

val test12a = 
    check'(fn _ => 
	   s2 = substring(s2, 0, size s2)
	   andalso "" = substring(s2, size s2, 0)
	   andalso "" = substring(s1, 0, 0));

val test12b = (substring(s2, ~1, 0) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test12c = (substring(s2, 11, 0) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test12d = (substring(s2, 0, 11) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test12e = (substring(s2, 10, 1) seq "WRONG") 
              handle Subscript => "OK" | _ => "WRONG";
val test12f = 
    check'(fn _ => 
	   "ABCDE" = substring(s2, 0, 5)
	   andalso "FGHI" = substring(s2, 6, 4));
val test12g = 
    case (Int.minInt, Int.maxInt) of
	(SOME min, SOME max) => 
	    ((substring("", max, max); substring("", min, min); "WRONG")
	     handle Subscript => "OK" | _ => "WRONG")
       | _ => "OK";

val test12'a = 
    check'(fn _ =>
	   map Char.toLower "AbHYUpqQ" = "abhyupqq"
	   andalso map Char.toLower "" = "")
val test12'b = 
    check'(fn _ => let val s = "7yuewjHHGDywj;p"
		       val v = ref []
		       fun prep c = (v := c :: !v; #"X")
		   in 
		       map prep s = "XXXXXXXXXXXXXXX"
		       andalso !v = rev(explode s)
		   end)

val test13a = 
    check'(fn _ => 
	   (translate (fn _ => "") s2 = ""
	    andalso translate (fn x => str x) "" = ""
	    andalso translate (fn x => str x) s2 = s2));

val test13b = 
    check'(fn _ => 
	   (translate (fn c => if c = #"\t" then "XYZ " else str c) s2 
	              = "ABCDEXYZ FGHI"));

val test14 = 
    check'(fn _ => 
	   (tokens isSpace "" = []
	    andalso tokens isSpace "   \t \n" = []
	    andalso tokens (fn c => c = #",") ",asd,,def,fgh" 
	            = ["asd","def","fgh"]));

val test15 = 
    check'(fn _ => 
	   (fields isSpace "" = [""]
	    andalso fields isSpace "   \t \n" = ["","","","","","",""]
	    andalso fields (fn c => c = #",") ",asd,,def,fgh" 
	            = ["","asd","","def","fgh"]));

val test16a = 
    check'(fn _ => 
	   EQUAL = compare(s1,s1) andalso EQUAL = compare(s2,s2)
	   andalso LESS = compare("A", "B")
	   andalso GREATER = compare("B", "A")
	   andalso LESS = compare("ABCD", "ABCDE")
	   andalso GREATER = compare("ABCDE", "ABCD"));

val test16b = 
    check'(fn _ => 
	   EQUAL = compare(s1,s1) andalso EQUAL = compare(s2,s2)
	   andalso LESS = compare("A", "a")
	   andalso GREATER = compare("b", "B")
	   andalso LESS = compare("abcd", "abcde")
	   andalso GREATER = compare("abcde", "abcd"));


(* Test cases for SML string escape functions. *)

val test17 = 
    let fun chk (arg, res) = toString (str arg) = res
	val argResList =
[(#"\000", "\\^@"),
	       (#"\001", "\\^A"),
	       (#"\006", "\\^F"),
	       (#"\007", "\\a"),
	       (#"\008", "\\b"),
	       (#"\009", "\\t"),
	       (#"\010", "\\n"),
	       (#"\011", "\\v"),
	       (#"\012", "\\f"),
	       (#"\013", "\\r"),
	       (#"\014", "\\^N"),
	       (#"\031", "\\^_"),
	       (#"\032", " "),
	       (#"\126", "~"),
	       (#"\\", "\\\\"),
	       (#"\"", "\\\""),
	       (#"A", "A"),
	       (#"\127", "\\127"),
	       (#"\128", "\\128"),
	       (#"\255", "\\255")]
	val (arg, res) = (implode (List.map #1 argResList), 
			  concat (List.map #2 argResList))
    in check'(fn _ => List.all chk argResList
	              andalso toString arg = res)
    end;

val test18 = 
    let val chars = CharVector.tabulate(256, chr)
    in check'(fn _ => fromString(toString chars) = SOME chars) end

val test19 =   		     
    let fun chkFromString (arg, res) = fromString arg = SOME (str res)
	val argResList = 
	    [("A", #"A"),
	     ("z", #"z"),
	     ("@", #"@"),
	     ("~", #"~"),
	     ("\\a", #"\007"),
	     ("\\b", #"\008"),
	     ("\\t", #"\009"),
	     ("\\n", #"\010"),
	     ("\\v", #"\011"),
	     ("\\f", #"\012"),
	     ("\\r", #"\013"),
	     ("\\\\", #"\\"),
	     ("\\\"", #"\""),
	     ("\\^@", #"\000"),
	     ("\\^A", #"\001"),
	     ("\\^Z", #"\026"),
	     ("\\^_", #"\031"), 
	     ("\\000", #"\000"),
	     ("\\097", #"a"),
	     ("\\255", #"\255"),
	     ("\\   \t\n\n \\A", #"A"),
	     ("\\   \t\n\n \\z", #"z"),
	     ("\\   \t\n\n \\@", #"@"),
	     ("\\   \t\n\n \\~", #"~"),
	     ("\\   \t\n\n \\\\n", #"\n"),
	     ("\\   \t\n\n \\\\t", #"\t"),
	     ("\\   \t\n\n \\\\\\", #"\\"),
	     ("\\   \t\n\n \\\\\"", #"\""),
	     ("\\   \t\n\n \\\\^@", #"\000"),
	     ("\\   \t\n\n \\\\^A", #"\001"),
	     ("\\   \t\n\n \\\\^Z", #"\026"),
	     ("\\   \t\n\n \\\\^_", #"\031"), 
	     ("\\   \t\n\n \\\\000", #"\000"),
	     ("\\   \t\n\n \\\\097", #"a"),
	     ("\\   \t\n\n \\\\255", #"\255")]
	val (arg, res) = (concat (List.map #1 argResList), 
			  implode (List.map #2 argResList))
    in 
	check'(fn _ => List.all chkFromString argResList 
	               andalso fromString arg = SOME res)
    end;

val test20 = 
    check'(fn _ => List.all (fn arg => fromString arg = NONE)
	   ["\\",
	    "\\c",
	    "\\F",
	    "\\e",
	    "\\g",
	    "\\N",
	    "\\T",
	    "\\1",
	    "\\11",
	    "\\256",
	    "\\-65",
	    "\\~65",
	    "\\?",
	    "\\^`",
	    "\\^a",
	    "\\^z",
	    "\\   a",
	    "\\   a\\B",
	    "\\   \\"]);


(* Test cases for C string escape functions *)

val test21 = 
    let val chars = CharVector.tabulate(256, chr)
    in check'(fn _ => fromCString(toCString chars) = SOME chars) end;

val test22 = 
    let val argResList = 
	    [("\010", "\\n"),
	     ("\009", "\\t"),
	     ("\011", "\\v"),
	     ("\008", "\\b"),
	     ("\013", "\\r"),
	     ("\012", "\\f"),
	     ("\007", "\\a"),
	     ("\\", "\\\\"),
	     ("?", "\\?"),
	     ("'", "\\'"),
	     ("\"", "\\\"")]
	val (arg, res) = (concat (List.map #1 argResList), 
			  concat (List.map #2 argResList))
    in
	check'(fn _ => 
	       List.all (fn (arg, res) => toCString arg = res) argResList
	       andalso toCString arg = res)
    end;

val test23 = 
    let fun checkFromCStringSucc (arg, res) = fromCString arg = SOME res
	val argResList = 
	    [("\\n", "\010"),
	     ("\\t", "\009"),
	     ("\\v", "\011"),
	     ("\\b", "\008"),
	     ("\\r", "\013"),
	     ("\\f", "\012"),
	     ("\\a", "\007"),
	     ("\\\\",  "\\"),
	     ("\\?", "?"),
	     ("\\'", "'"),
	     ("\\\"", "\""),
	     ("\\1", "\001"),
	     ("\\11", "\009"),
	     ("\\111", "\073"),
	     ("\\1007", "\0647"),
	     ("\\100A", "\064A"),
	     ("\\0",   "\000"),
	     ("\\377", "\255"),
	     ("\\18", "\0018"),
	     ("\\178", "\0158"),
	     ("\\1C", "\001C"),
	     ("\\17C", "\015C"),
	     ("\\x0", "\000"),
	     ("\\xff", "\255"),
	     ("\\xFF", "\255"),
	     ("\\x1", "\001"),
	     ("\\x11", "\017"),
	     ("\\xag", "\010g"),
	     ("\\xAAg", "\170g"),
	     ("\\x0000000a", "\010"),
	     ("\\x0000000a2", "\162"),
	     ("\\x0000000ag", "\010g"),
	     ("\\x0000000A", "\010"),
	     ("\\x0000000A2", "\162"),
	     ("\\x0000000Ag", "\010g"),
	     ("\\x00000000000000000000000000000000000000000000000000000000000000011+",
	      "\017+")]
	val (arg, res) = (concat (List.map #1 argResList), 
			  concat (List.map #2 argResList))
    in 
	check'(fn _ => List.all checkFromCStringSucc argResList
	               andalso fromCString arg = SOME res)
    end;

val test24 = 
    let fun checkFromCStringFail arg = fromCString arg = NONE
    in
	check'(fn _ => List.all checkFromCStringFail 
	       ["\\",
		"\\X",
		"\\=",
		"\\400",
		"\\777",
		"\\8",
		"\\9",
		"\\c",
		"\\d",
		"\\x",
		"\\x100",
		"\\xG"])
    end;

val test25 = 
    check'(fn _ => 
	   isPrefix "" ""
	   andalso isPrefix ""  "abcde"
	   andalso isPrefix "a"  "abcde"
	   andalso isPrefix "abcd"  "abcde"
	   andalso isPrefix "abcde"  "abcde"
	   andalso not (isPrefix "abcde"  "")
	   andalso not (isPrefix "abcdef"  "abcde")
	   andalso not (isPrefix "Abcde"  "abcde")
	   andalso not (isPrefix "abcdE"  "abcde"))

end
