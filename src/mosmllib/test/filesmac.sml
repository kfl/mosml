(* test/filesys.sml
   PS 1995-03-23, 1996-05-01
*)

(* Mac: changed "/" to ":", "." to ":", ".." to "::"
        10a is testing that remove doesn't work on directories
            -- it does on the Mac; this is probably OK
        testcycl -> testcycl  can this be done on the Mac?
        readDir doen't make "." and ".." -- commented out
                so rewindDir test is less worthwhile (but it does work!)
        test3b has been replaced, see the note there
        test13a & 13c fail because readLink fails for non-links
        test13b fails because readLink makes full pathname

        test5 is hacked extensively... the MacOS has a bug where the OS
              holds a working directory link to renamed directory for
              an indeterminant time... so if test5 fails twice, as it will
              (since you can't delete it until MacOS gives up its WD link)
              succeeding tests will fail as well
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
    val _ = (rmDir "exists.not") handle OS.SysErr _ => (); 
in

val test1a = (mkDir "testdir" seq "OK") handle _ => "WRONG";
val test1b = (mkDir "testdir" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";

val test2 = check'(fn _ => isDir "testdir");
    
val test3a = check'(fn _ => access("testdir", [A_READ, A_EXEC, A_WRITE]));

(* on Mac, empty directories have zero size...
val test3b = check'(fn _ => fileSize "testdir" > 0);
*)
val test3b = check'(fn _ => fileSize "testdir" = 0);
val test3c = check'(fn _ => fileSize ":" > 0);

local 
    val cdir = getDir();
in
    val test4a = (chDir cdir seq "OK") handle _ => "WRONG";
    val test4b = check'(fn _ => cdir = getDir());
    val _ = chDir "testdir";
    val test4c = check'(fn _ => cdir <> getDir());
    val _ = chDir "::";
    val test4d = check'(fn _ => cdir = getDir());
end;

val _ = rename{old = "testdir", new = "exists.not"};

fun suspend n =
  let open Timer
      open Time
      val sta = startRealTimer ()
      val dur = fromMilliseconds n
  in 
      while ((checkRealTimer sta) < dur) do ()
  end;

val test5 = (rmDir "exists.not" seq "OK") (* 960619 e -- why? *)
            handle _ => "WRONG,"
                        ^ (if access("exists.not", [A_READ, A_EXEC, A_WRITE])
                           then let val _ = suspend 3000 (* 2 to 3 sec. *)
                                in "Found," end
                           else "Missing,")
                        ^ ((rmDir "exists.not" seq "OK")
                            handle _ => "WRONG,"
                        ^ (if access("exists.not", [A_READ, A_EXEC, A_WRITE])
                           then let val _ = suspend 3000 (* 2 to 3 sec. *)
                                in "Found," end
                           else "Missing,")
                        ^ ((rmDir "exists.not" seq "OK")
                            handle _ => "WRONG"));

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
        check'(fn _ => (* ":" = readDir dstr
               andalso "::" = readDir dstr
               andalso *) "" = readDir dstr);
    val _ = rewindDir dstr;
    val test7b = 
        check'(fn _ => (* ":" = readDir dstr
               andalso "::" = readDir dstr
               andalso *) "" = readDir dstr);
    val _ = closeDir dstr;
    val test7c = (readDir dstr seq "WRONG")
                 handle OS.SysErr _ => "OK" | _ => "WRONG";
    val test7d = (rewindDir dstr seq "WRONG")
                 handle OS.SysErr _ => "OK" | _ => "WRONG";
    val test7e = (closeDir dstr seq "OK")
                 handle _ => "WRONG"
end

val test8a = 
    check'(fn _ => fullPath ":" = getDir ());
val test8b = 
    check'(fn _ => fullPath "testlink" = getDir() ^ ":README");
val test8c = (fullPath "testcycl" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test8d = (fullPath "testbadl" seq "WRONG")
             handle OS.SysErr _ => "OK" | _ => "WRONG";
val test8e = 
    check'(fn _ => realPath ":" = ":");
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
    
(* -- e
val test10a = (remove "testdir" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";
*)
val test10a = "WRONG, because remove removes directories.";
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
    check'(fn _ => (* isLink "testcycl" 
           andalso *)  isLink "testlink"
           andalso isLink "testbadl");
val test12b = 
    check'(fn _ => not (isLink "testdir2"
                        orelse isLink "README"));
val test12c = (isLink "exists.not" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";

val test13a = 
    check'(fn _ => readLink "testcycl" = "testcycl");
val test13b = 
    check'(fn _ => readLink "testlink" = (FileSys.getDir() ^ ":README"));
val test13c = 
    check'(fn _ => readLink "testbadl" = "exists.not");
val test13d = (readLink "testdir2" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";
val test13e = (readLink "exists.not" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";

val test14 = (tmpName () seq "OK");

val test15a = 
    check'(fn _ => 
           fileId ":" = fileId ":"
           andalso fileId "testlink" = fileId "README"
           andalso fileId ":" <> fileId "README");
val test15b = 
    check'(fn _ => compare(fileId ":", fileId ":") = EQUAL
           andalso compare(fileId ":", fileId "README") <> EQUAL
           andalso compare(fileId "testlink", fileId "README") = EQUAL
           andalso (compare(fileId ":", fileId "README") = LESS 
                    andalso compare(fileId "README", fileId ":") = GREATER
                    orelse 
                    compare(fileId ":", fileId "README") = GREATER 
                    andalso compare(fileId "README", fileId ":") = LESS));
val test15c = (fileId "exists.not" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";
val test15d = (fileId "testbadl" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";
val test15e = (fileId "testcycl" seq "WRONG")
              handle OS.SysErr _ => "OK" | _ => "WRONG";
(* Unix only: *)
(*
val test15f = 
       check'(fn _ => 
           fileId "hardlinkA" = fileId "hardlinkB"
           andalso compare(fileId "hardlinkA", fileId "hardlinkB") = EQUAL);
*)
val _ = rmDir "testdir2";
end
