(* test/dospath.sml 6 -- for DOS -- incomplete, 1995-06-16 *)

use "auxil.sml";

local 
    open Path
in
    
val test1a = 
    check'(fn _ => fromString "" = {isAbs=false, vol = "", arcs = []});
val test1b = 
    check'(fn _ => fromString "/" = {isAbs=true, vol="", arcs=[""]});
val test1c = 
    check'(fn _ => fromString "//" = {isAbs=true, vol="", arcs=["", ""]});
val test1d = 
    check'(fn _ => fromString "a" = {isAbs=false, vol = "", arcs = ["a"]});
val test1e = 
    check'(fn _ => fromString "/a" = {isAbs=true, vol="", arcs=["a"]});
val test1f = 
    check'(fn _ => fromString "//a" = {isAbs=true, vol="", arcs=["","a"]});
val test1g = 
    check'(fn _ => fromString "a/" = {isAbs=false, vol = "", arcs = ["a", ""]});
val test1h = 
    check'(fn _ => fromString "a//" = {isAbs=false, vol = "", arcs = ["a", "", ""]});
val test1i = 
    check'(fn _ => fromString "a/b" = {isAbs=false, vol = "", arcs = ["a", "b"]});
val test1j = 
    check'(fn _ => fromString "a.b/c" = {isAbs=false, vol = "", arcs = ["a.b", "c"]});
val test1k = 
    check'(fn _ => fromString "a.b/c/" = {isAbs=false, vol = "", arcs = ["a.b", "c", ""]});
val test1l = 
    check'(fn _ => fromString "a/./c" = {isAbs=false, vol = "", arcs = ["a", ".", "c"]});
val test1m = 
    check'(fn _ => fromString "a/../c" = {isAbs=false, vol = "", arcs = ["a", "..", "c"]});
val test1n = 
    check'(fn _ => fromString "." = {isAbs=false, vol = "", arcs = ["."]});

val test2a =
    check'(fn _ => toString {isAbs=false, vol = "", arcs = []} = "");
val test2b = 
    check'(fn _ => toString {isAbs=true, vol="", arcs=[]} = "/");
val test2c = 
    check'(fn _ => toString {isAbs=true, vol="", arcs=["", ""]} = "//");
val test2d = 
    check'(fn _ => toString {isAbs=false, vol = "", arcs = ["a"]} = "a");
val test2e = 
    check'(fn _ => toString {isAbs=true, vol="", arcs=["a"]} = "/a");
val test2f = 
    check'(fn _ => toString {isAbs=true, vol="", arcs=["","a"]} = "//a");
val test2g = 
    check'(fn _ => toString {isAbs=false, vol = "", arcs = ["a", ""]} = "a/");
val test2h = 
    check'(fn _ => toString {isAbs=false, vol = "", arcs = ["a", "", ""]} = "a//");
val test2i = 
    check'(fn _ => toString {isAbs=false, vol = "", arcs = ["a", "b"]} = "a/b");
val test2j = 
    check'(fn _ => toString {isAbs=false, vol = "", arcs = ["a.b", "c"]} = "a.b/c");
val test2k = 
    check'(fn _ => toString {isAbs=false, vol = "", arcs = ["a.b", "c", ""]} = "a.b/c/");
val test2l = 
    check'(fn _ => toString {isAbs=false, vol = "", arcs = ["a", ".", "c"]} = "a/./c");
val test2m = 
    check'(fn _ => toString {isAbs=false, vol = "", arcs = ["a", "..", "c"]} = "a/../c");
val test2n = 
    check'(fn _ => toString {isAbs=true, vol="", arcs=["a", "..", "c"]} = "/a/../c");
val test2o = (toString {isAbs=false, vol = "", arcs =  ["", "a"]} seq "WRONG")
             handle Path => "OK" | _ => "WRONG";
val test2p = 
    check'(fn _ => 
   (toString {isAbs=true, vol = "C:", arcs =  ["windows"]} = "C:/windows"));

val test3b = 
    check'(fn _ => getVolume "/" = "");
val test3c = 
    check'(fn _ => getVolume "//" = "");
val test3d = 
    check'(fn _ => getVolume "a//b/c/" = "");
val test3e = 
    check'(fn _ => getVolume "./" = "");
val test3f = 
    check'(fn _ => getVolume "../" = "");
val test3g = 
    check'(fn _ => getVolume "" = "");
val test3h = 
    check'(fn _ => getVolume "C:" = "C:");

val test4a = 
    check'(fn _ => 
	   List.all isRelative ["", ".", "..", "a//"]
	   andalso not (List.exists isRelative ["/", "/a", "//"]));
val test4b = 
    check'(fn _ => 
	   List.all isAbsolute ["/", "/a", "//", "/.", "/.."]
	   andalso not (List.exists isAbsolute ["", ".", "..", "a//"]));

val test5a = 
    check'(fn _ => 
	   getParent "/" = "/"
	   andalso getParent "a" = "."
	   andalso getParent "a/" = "a/.."
	   andalso getParent "a///" = "a///.."
	   andalso getParent "a/b" = "a"
	   andalso getParent "a/b/" = "a/b/.."
	   andalso getParent "/a/b" = "/a"
	   andalso getParent "/a/b/" = "/a/b/.."
	   andalso getParent ".." = "../.."
	   andalso getParent "." = ".."
	   andalso getParent "../" = "../.."
	   andalso getParent "./" = "./.."
	   andalso getParent "" = "..");

val test6a = 
    check'(fn _ => 
	   concat("a", "b") = "a/b"
	   andalso concat("a", "b/c") = "a/b/c"
	   andalso concat("/", "b/c") = "/b/c"
	   andalso concat("", "b/c") = "b/c"
	   andalso concat("/a", "b/c") = "/a/b/c"
	   andalso concat("a/", "b/c") = "a/b/c"
	   andalso concat("a//", "b/c") = "a//b/c"
	   andalso concat(".", "b/c") = "./b/c"
	   andalso concat("a/b", "..") = "a/b/.."
	   andalso concat("a/b", "../c") = "a/b/../c");
val test6b = (concat ("a", "/b") seq "WRONG")
             handle Path => "OK" | _ => "WRONG";

val test7a = 
    check'(fn _ => 
	   mkAbsolute("/a/b", "/c/d") = "/a/b"
	   andalso mkAbsolute("/", "/c/d") = "/"
	   andalso mkAbsolute("a/b", "/c/d") = "/c/d/a/b");
val test7b = (mkAbsolute("a", "c/d") seq "WRONG")
             handle Path => "OK" | _ => "WRONG";
val test7c = (mkAbsolute("/a", "c/d") seq "WRONG")
              handle Path => "OK" | _ => "WRONG";

val test8a = 
    check'(fn _ => 
	   mkRelative("a/b", "/c/d") = "a/b"
	   andalso mkRelative("/", "/a/b/c")	   = "../../.." 
	   andalso mkRelative("/a/", "/a/b/c")	   = "../../" 
	   andalso mkRelative("/a/b/", "/a/c")	   = "../b/"     
	   andalso mkRelative("/a/b", "/a/c/")	   = "../b"      
	   andalso mkRelative("/a/b/", "/a/c/")	   = "../b/"     
	   andalso mkRelative("/", "/")		   = "."	      
	   andalso mkRelative("/", "/.")	   = "."	      
	   andalso mkRelative("/", "/..")	   = "."	      
	   andalso mkRelative("/", "/a")	   = ".."	      
	   andalso mkRelative("/a/b/../c", "/a/d") = "../b/../c" 
	   andalso mkRelative("/a/b", "/c/d")      = "../../a/b"
	   andalso mkRelative("/c/a/b", "/c/d")    = "../a/b"
	   andalso mkRelative("/c/d/a/b", "/c/d")  = "a/b");
val test8b = (mkRelative("/a", "c/d") seq "WRONG")
              handle Path => "OK" | _ => "WRONG";
val test8c = (mkRelative("a", "c/d") seq "WRONG")
              handle Path => "OK" | _ => "WRONG";

val test9a = let
    fun chkCanon (a, b) =
	  (mkCanonical a = b) 
	  andalso (mkCanonical b = b)
	  andalso (isCanonical b)
    in
      check'(fn _ => 
	   chkCanon("", ".")
	   andalso chkCanon(".", ".")
	   andalso chkCanon("./.", ".")
	   andalso chkCanon("/.", "/")
	   andalso chkCanon("..", "..")
	   andalso chkCanon("../..", "../..")
	   andalso chkCanon("b", "b")
	   andalso chkCanon("a/b", "a/b")
	   andalso chkCanon("/a/b", "/a/b")
	   andalso chkCanon("a/b/", "a/b")
	   andalso chkCanon("a/b//", "a/b")
	   andalso chkCanon("a/../b", "b")
	   andalso chkCanon("a/..", ".")
	   andalso chkCanon("a/.", "a")
	   andalso chkCanon("a/", "a")
	   andalso chkCanon("/a/../b/", "/b")
	   andalso chkCanon("/..", "/")
	   andalso chkCanon("/../../a/b", "/a/b")
	   andalso chkCanon("/./../../a/b", "/a/b")
	   andalso chkCanon("/./../..", "/")
	   andalso chkCanon("a/../b", "b")
	   andalso chkCanon("a/./b", "a/b")
	   andalso chkCanon("a////b", "a/b")
           andalso chkCanon("a////b", "a/b"))
    end

val test10a = 
    check'(fn _ => 
	   not (isCanonical "./."
		orelse isCanonical "/.."
		orelse isCanonical "/."
		orelse isCanonical "//"
		orelse isCanonical "a/.."
		orelse isCanonical "a//b"
		orelse isCanonical "a/."
	        orelse isCanonical "a/b/"
		orelse isCanonical "a/.."))
		
val test11a = 
    check'(fn _ => 
	   splitDirFile "" = {dir = "", file = ""}
	   andalso splitDirFile "." = {dir = "", file = "."}
	   andalso splitDirFile ".." = {dir = "", file = ".."}
	   andalso splitDirFile "b" = {dir = "", file = "b"}
	   andalso splitDirFile "b/" = {dir = "b", file = ""}
	   andalso splitDirFile "a/b" = {dir = "a", file = "b"}
	   andalso splitDirFile "/a" = {dir = "/", file = "a"}
	   andalso splitDirFile "/a/b" = {dir = "/a", file = "b"}
	   andalso splitDirFile "/c/a/b" = {dir = "/c/a", file = "b"}
	   andalso splitDirFile "/c/a/b/" = {dir = "/c/a/b", file = ""}
	   andalso splitDirFile "/c/a/b.foo.bar" = {dir = "/c/a", file="b.foo.bar"}
	   andalso splitDirFile "/c/a/b.foo" = {dir = "/c/a", file = "b.foo"});

(*    
val test11b = (splitDirFile "" seq "WRONG") 
              handle Path => "OK" | _ => "WRONG";
*)

val test12 = 
    check'(fn _ => 
	   "" = joinDirFile {dir = "", file = ""}
	   andalso "b" = joinDirFile {dir = "", file = "b"}
	   andalso "/" = joinDirFile {dir = "/", file = ""}
	   andalso "/b" = joinDirFile {dir = "/", file = "b"}
	   andalso "a/b" = joinDirFile {dir = "a", file = "b"}
	   andalso "/a/b" = joinDirFile {dir = "/a", file = "b"}
	   andalso "/c/a/b" = joinDirFile {dir = "/c/a", file = "b"}
	   andalso "/c/a/b/" = joinDirFile {dir = "/c/a/b", file = ""}
	   andalso "/c/a/b.foo.bar" = joinDirFile {dir = "/c/a", file="b.foo.bar"}
	   andalso "/c/a/b.foo" = joinDirFile {dir = "/c/a", file = "b.foo"});

val test13 = 
    check'(fn _ => 
	   dir "b" = ""
	   andalso dir "a/b" = "a"
	   andalso dir "/" = "/"
	   andalso dir "/b" = "/"
	   andalso dir "/a/b" = "/a"
	   andalso dir "/c/a/b" = "/c/a"
	   andalso dir "/c/a/b/" = "/c/a/b"
	   andalso dir "/c/a/b.foo.bar" = "/c/a"
	   andalso dir "/c/a/b.foo" = "/c/a");

val test14 = 
    check'(fn _ => 
	   file "b" = "b"
	   andalso file "a/b" = "b"
	   andalso file "/" = ""
	   andalso file "/b" = "b"
	   andalso file "/a/b" = "b"
	   andalso file "/c/a/b" = "b"
	   andalso file "/c/a/b/" = ""
	   andalso file "/c/a/b.foo.bar" = "b.foo.bar"
	   andalso file "/c/a/b.foo" = "b.foo");

val test15 = 
    check'(fn _ => 
	   splitBaseExt "" = {base = "", ext = NONE}
	   andalso splitBaseExt ".login" = {base = ".login", ext = NONE}
	   andalso splitBaseExt "/.login" = {base = "/.login", ext = NONE}
	   andalso splitBaseExt "a" = {base = "a", ext = NONE}
	   andalso splitBaseExt "a." = {base = "a.", ext = NONE}
	   andalso splitBaseExt "a.b" = {base = "a", ext = SOME "b"}
	   andalso splitBaseExt "a.b.c" = {base = "a.b", ext = SOME "c"}
	   andalso splitBaseExt "/a.b" = {base = "/a", ext = SOME "b"}
	   andalso splitBaseExt "/c/a.b" = {base = "/c/a", ext = SOME "b"}
	   andalso splitBaseExt "/c/a/b/.d" = {base = "/c/a/b/.d", ext = NONE}
	   andalso splitBaseExt "/c.a/b.d" = {base = "/c.a/b", ext = SOME "d"}
	   andalso splitBaseExt "/c.a/bd" = {base = "/c.a/bd", ext = NONE}
	   andalso splitBaseExt "/c/a/b.foo.bar" = {base="/c/a/b.foo",ext=SOME "bar"}
	   andalso splitBaseExt "/c/a/b.foo" = {base = "/c/a/b", ext = SOME "foo"});

val test16 = 
    check'(fn _ => 
	   "" = joinBaseExt {base = "", ext = NONE}
	   andalso ".login" = joinBaseExt {base = ".login", ext = NONE}
	   andalso "a" = joinBaseExt {base = "a", ext = NONE}
	   andalso "a." = joinBaseExt {base = "a", ext = SOME ""}
	   andalso "a.b" = joinBaseExt {base = "a", ext = SOME "b"}
	   andalso "a.b.c" = joinBaseExt {base = "a.b", ext = SOME "c"}
	   andalso "a.b.c.d" = joinBaseExt {base = "a.b", ext = SOME "c.d"}
	   andalso "/a.b" = joinBaseExt {base = "/a", ext = SOME "b"}
	   andalso "/c/a.b" = joinBaseExt {base = "/c/a", ext = SOME "b"}
	   andalso "/c/a/b/.d" = joinBaseExt {base = "/c/a/b/", ext = SOME "d"}
	   andalso "/c/a/b.foo.bar" = joinBaseExt {base="/c/a/b",ext=SOME "foo.bar"}
	   andalso "/c/a/b.foo" = joinBaseExt {base = "/c/a/b", ext = SOME "foo"});

val test17 = 
    check'(fn _ => 
	   ext "" = NONE
	   andalso ext ".login" = NONE
	   andalso ext "/.login" = NONE
	   andalso ext "a" = NONE
	   andalso ext "a." = NONE
	   andalso ext "a.b" = SOME "b"
	   andalso ext "a.b.c" = SOME "c"
	   andalso ext "a.b.c.d" = SOME "d"
	   andalso ext "/a.b" = SOME "b"
	   andalso ext "/c/a.b" = SOME "b"
	   andalso ext "/c/a/b/.d" = NONE
	   andalso ext "/c.a/b.d" = SOME "d"
	   andalso ext "/c.a/bd" = NONE
	   andalso ext "/c/a/b.foo.bar" = SOME "bar"
	   andalso ext "/c/a/b.foo" = SOME "foo");

val test18 = 
    check'(fn _ => 
	   base "" = ""
	   andalso base ".d" = ".d"
	   andalso base ".login" = ".login"
	   andalso base "/.login" = "/.login"
	   andalso base "a" = "a"
	   andalso base "a." = "a."
	   andalso base "a.b" = "a"
	   andalso base "a.b.c" = "a.b" 
	   andalso base "a.b.c.d" = "a.b.c"
	   andalso base "/a.b" = "/a"
	   andalso base "/c/a.b" = "/c/a"
	   andalso base "/c/a/b/.d" = "/c/a/b/.d"
	   andalso base "/c.a/b.d" = "/c.a/b"
	   andalso base "/c.a/bd" = "/c.a/bd"
	   andalso base "/c/a/b.foo.bar" = "/c/a/b.foo"
	   andalso base "/c/a/b.foo" = "/c/a/b");

val test19 = 
    check'(fn () => validVolume{isAbs=false, vol=""}
	   andalso validVolume{isAbs=true, vol=""}
	   andalso validVolume{isAbs=true, vol="C:"}
	   andalso validVolume{isAbs=false, vol="C:"}
	   andalso not (validVolume{isAbs=true, vol="/"}
			orelse validVolume{isAbs=false, vol="/"} 
			orelse validVolume{isAbs=true, vol=" "}
			orelse validVolume{isAbs=false, vol=" "})); 
end
