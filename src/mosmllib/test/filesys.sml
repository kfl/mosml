(* test/filesys.sml
   PS 1995-03-23, 1996-05-01, 1998-04-06, 1999-03-02
*)

(* DOS: Plain WRONG: test6a, test9a (and test9b);
        Excusable:   test8b, test11b, test12a, test13a, test13b, test13c
 *)

(* This requires two links to be present in the current directory:
	testlink -> README
	testcycl -> testcycl 
	testbadl -> exists.not
   Moreover, the file README must exist and the file exists.not not.
*)


use "auxil.sml";

local
    open FileSys
    (* Clean up: *)
    val _ = (rmDir "testdir") handle OS.SysErr _ => (); 
    val _ = (rmDir "testdir2") handle OS.SysErr _ => (); 
in

val test1a = (mkDir "testdir" seq "OK") handle _ => "WRONG";
val test1b = (mkDir "testdir" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";

val test2 = check'(fn _ => isDir "testdir");
    
val test3a = check'(fn _ => access("testdir", [A_READ, A_EXEC, A_WRITE]));

local 
    val cdir = getDir();
in
    val test4a = (chDir cdir seq "OK") handle _ => "WRONG";
    val test4b = check'(fn _ => cdir = getDir());
    val _ = chDir "testdir";
    val test4c = check'(fn _ => cdir <> getDir());
    val _ = chDir "..";
    val test4d = check'(fn _ => cdir = getDir());
end;

val _ = rename{old = "testdir", new = "exists.not"};

val test5 = (rmDir "exists.not" seq "OK") handle _ => "WRONG";

val test6a = (openDir "exists.not" seq "WRONG") 
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6b = (isDir "exists.not" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6c = (rmDir "exists.not" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6d = (chDir "exists.not" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6e = (fullPath "exists.not" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6f = (realPath "exists.not" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6g = (modTime "exists.not" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6h = (setTime("exists.not", NONE) seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6i = (remove "exists.not" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6j = (rename{old="exists.not", new="testdir2"} seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6k = (fileSize "exists.not" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test6l = check'(fn _ => not (access("exists.not", [])));

val _ = mkDir "testdir";

local 
    val dstr = openDir "testdir";
in
    val test7a = 
	check'(fn _ => NONE = readDir dstr);
    val _ = rewindDir dstr;
    val test7b = 
	check'(fn _ => NONE = readDir dstr);
    val _ = closeDir dstr;
    val test7c = (readDir dstr seq "WRONG")
 	         handle OS.SysErr _ => "OK" | _ => "WRONG";
    val test7d = (rewindDir dstr seq "WRONG")
 	         handle OS.SysErr _ => "OK" | _ => "WRONG";
    val test7e = (closeDir dstr seq "OK")
 	         handle _ => "WRONG"
end

val test8a = 
    check'(fn _ => fullPath "." = getDir ());
val test8b = 
    check'(fn _ => fullPath "testlink" = getDir() ^ "/README");
val test8c = (fullPath "testcycl" seq "WRONG")
 	     handle OS.SysErr _ => "OK" | _ => "WRONG";
val test8d = (fullPath "testbadl" seq "WRONG")
 	     handle OS.SysErr _ => "OK" | _ => "WRONG";
val test8e = 
    check'(fn _ => realPath "." = ".");
val test8f = 
    check'(fn _ => realPath "testlink" = "README");
val test8g = (realPath "testcycl" seq "WRONG")
 	     handle OS.SysErr _ => "OK" | _ => "WRONG";
val test8h = (realPath "testbadl" seq "WRONG")
 	     handle OS.SysErr _ => "OK" | _ => "WRONG";

val test9a = 
    check'(fn _ => 
	   setTime ("README", SOME (Time.fromReal 1E6)) = ());
val test9b = 
    check'(fn _ => modTime "README" = Time.fromReal 1E6);
    
val test10a = (remove "testdir" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";
val test10b = 
    check'(fn _ => 
	   rename{old = "testdir", new = "testdir2"} = ());
val test10c = 
    check'(fn _ => isDir "testdir2");

val test11a = 
    check'(fn _ => not (access ("testdir", [])));
val test11b = 
    check'(fn _ => access("testlink", []));
val test11c = 
    check'(fn _ => not (access("testbadl", [])));

val test12a = 
    check'(fn _ => isLink "testcycl" 
	   andalso isLink "testlink"
	   andalso isLink "testbadl");
val test12b = 
    check'(fn _ => not (isLink "testdir2"
			orelse isLink "README"));
val test12c = (isLink "exists.not" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";

val test13a = 
    check'(fn _ => readLink "testcycl" = "testcycl");
val test13b = 
    check'(fn _ => readLink "testlink" = "README");
val test13c = 
    check'(fn _ => readLink "testbadl" = "exists.not");
val test13d = (readLink "testdir2" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";
val test13e = (readLink "exists.not" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";

val test14 = (tmpName () seq "OK");

val test15a = 
    check'(fn _ => 
	   fileId "." = fileId "."
	   andalso fileId "testlink" = fileId "README"
 	   andalso fileId "." <> fileId "README");
val test15b = 
    check'(fn _ => compare(fileId ".", fileId ".") = EQUAL
	   andalso compare(fileId ".", fileId "README") <> EQUAL
	   andalso compare(fileId "testlink", fileId "README") = EQUAL
	   andalso (compare(fileId ".", fileId "README") = LESS 
		    andalso compare(fileId "README", fileId ".") = GREATER
		    orelse 
		    compare(fileId ".", fileId "README") = GREATER 
		    andalso compare(fileId "README", fileId ".") = LESS));
val test15c = (fileId "exists.not" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";
val test15d = (fileId "testbadl" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";
val test15e = (fileId "testcycl" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";
(* Unix only: *)
val test15f = 
       check'(fn _ => 
	   fileId "hardlinkA" = fileId "hardlinkB"
	   andalso compare(fileId "hardlinkA", fileId "hardlinkB") = EQUAL);

val _ = rmDir "testdir2";
end
