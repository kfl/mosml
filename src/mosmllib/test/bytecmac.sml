(* test/bytechar.sml -- test cases for Byte and Char, suitable for ASCII
   PS 1994-12-10, 1995-05-11, 1995-11-10 *)

(* Mac: changed \^M to \^J in three places *)

use "auxil.sml";

local 

in 
val test1 = checkrange (0,255) (fn i => 
    (Word8.toInt o Byte.charToByte o Byte.byteToChar o Word8.fromInt) i = i);

val test2 = checkrange (0,Char.maxOrd) (fn i => 
    (Word8.toInt o Byte.charToByte o Char.chr) i = i);

val test3 = checkrange (0,255) 
    (fn i => (Char.ord o Byte.byteToChar o Word8.fromInt) i = i);

val test4 = checkrange (0, Char.maxOrd) 
    (fn i => (Char.ord o Char.chr) i = i);

val test5 = (Char.chr ~1 seq "WRONG") handle Chr => "OK" | _ => "WRONG";

val test6 = (Char.chr (Char.maxOrd+1) seq "WRONG") 
            handle Chr => "OK" | _ => "WRONG";
        
val test7 = check("" = Byte.bytesToString (Word8Vector.fromList []));

val test8 = 
    check("ABDC" = 
          (Byte.bytesToString o Word8Vector.fromList o map Word8.fromInt)
           [65, 66, 68, 67]);

val test9 = check("" = Byte.unpackString(Word8Array.fromList [], 0, SOME 0));

local 
    val arr = Word8Array.tabulate(10, fn i => Word8.fromInt(i+65))
in
val test10a = check("" = Byte.unpackString(arr, 0, SOME 0));
val test10b = check("" = Byte.unpackString(arr, 10, SOME 0) 
                   andalso "" = Byte.unpackString(arr, 10, NONE));
val test10c = check("BCDE" = Byte.unpackString(arr, 1, SOME 4));
val test10d = (Byte.unpackString(arr, ~1, SOME 0) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test10e = (Byte.unpackString(arr, 11, SOME 0) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test10f = (Byte.unpackString(arr, 0, SOME ~1) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test10g = (Byte.unpackString(arr, 0, SOME 11) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test10h = (Byte.unpackString(arr, 10, SOME 1) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test10i = (Byte.unpackString(arr, ~1, NONE) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test10j = (Byte.unpackString(arr, 11, NONE) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
end

local 
    val vec = Word8Vector.tabulate(10, fn i => Word8.fromInt(i+65))
in
val test11a = check("" = Byte.unpackStringVec(vec, 0, SOME 0));
val test11b = check("" = Byte.unpackStringVec(vec, 10, SOME 0) 
                   andalso "" = Byte.unpackStringVec(vec, 10, NONE));
val test11c = check("BCDE" = Byte.unpackStringVec(vec, 1, SOME 4));
val test11d = (Byte.unpackStringVec(vec, ~1, SOME 0) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test11e = (Byte.unpackStringVec(vec, 11, SOME 0) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test11f = (Byte.unpackStringVec(vec, 0, SOME ~1) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test11g = (Byte.unpackStringVec(vec, 0, SOME 11) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test11h = (Byte.unpackStringVec(vec, 10, SOME 1) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test11i = (Byte.unpackStringVec(vec, ~1, NONE) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
val test11j = (Byte.unpackStringVec(vec, 11, NONE) seq "WRONG")
              handle Subscript => "OK" | _ => "WRONG";
end

val test18 = check(not (Char.contains "" (Char.chr 65))
                   andalso not (Char.contains "aBCDE" (Char.chr 65))
                   andalso (Char.contains "ABCD" (Char.chr 67))
                   andalso not (Char.contains "" #"\000")
                   andalso not (Char.contains "" #"\255")
                   andalso not (Char.contains "azAZ09" #"\000")
                   andalso not (Char.contains "azAZ09" #"\255"));

val test19 = check(Char.notContains "" (Char.chr 65)
                   andalso Char.notContains "aBCDE" (Char.chr 65)
                   andalso not (Char.notContains "ABCD" (Char.chr 67))
                   andalso Char.notContains "" #"\000"
                   andalso Char.notContains "" #"\255"
                   andalso Char.notContains "azAZ09" #"\000"
                   andalso Char.notContains "azAZ09" #"\255");

val test20 = check(Char.ord Char.maxChar = Char.maxOrd);

local 
fun mycontains s c = 
    let val stop = String.size s
        fun h i = i < stop andalso (c = String.sub(s, i) orelse h(i+1))
    in h 0 end;

(* Check that p(c) = (mycontains s c) for all characters: *)
fun equivalent p s = 
    let fun h n =
        n > 255 orelse 
        (p (chr n) = mycontains s (chr n)) andalso h(n+1)
    in h 0 end

fun checkset p s = check'(fn _ => equivalent p s);

val graphchars = "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ\
 \[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~";

val ascii = "\^@\^A\^B\^C\^D\^E\^F\^G\^H\t\^J\^K\^L\n\^N\^O\^P\
 \\^Q\^R\^S\^T\^U\^V\^W\^X\^Y\^Z\^[\^\\^]\^^\^_\
 \ !\"#$%&'()*+,-./0123456789:;<=>?@\
 \ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127" 

val lowerascii = "\^@\^A\^B\^C\^D\^E\^F\^G\^H\t\^J\^K\^L\n\^N\^O\^P\
 \\^Q\^R\^S\^T\^U\^V\^W\^X\^Y\^Z\^[\^\\^]\^^\^_\
 \ !\"#$%&'()*+,-./0123456789:;<=>?@\
 \abcdefghijklmnopqrstuvwxyz[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127" 

val upperascii = "\^@\^A\^B\^C\^D\^E\^F\^G\^H\t\^J\^K\^L\n\^N\^O\^P\
 \\^Q\^R\^S\^T\^U\^V\^W\^X\^Y\^Z\^[\^\\^]\^^\^_\
 \ !\"#$%&'()*+,-./0123456789:;<=>?@\
 \ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`ABCDEFGHIJKLMNOPQRSTUVWXYZ{|}~\127" 

val allchars = 
    let fun h 0 res = chr 0 :: res
          | h n res = h (n-1) (chr n :: res)
    in h 255 [] end

open Char
in

val test21 = 
    checkset isLower "abcdefghijklmnopqrstuvwxyz";
val test22 = 
    checkset isUpper "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
val test23 = 
    checkset isDigit "0123456789";
val test24 = 
    checkset isAlpha "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
val test25 = 
    checkset isHexDigit "0123456789abcdefABCDEF";
val test26 = 
    checkset isAlphaNum 
       "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
val test27 = 
    checkset isPrint (" " ^ graphchars)
val test28 = 
    checkset isSpace " \009\010\011\012\013";
val test29 = 
    checkset isGraph graphchars
val test30 = 
    checkset isAscii ascii

val test31 = 
    check'(fn _ => map toLower (explode ascii) = explode lowerascii)
val test32 = 
    check'(fn _ => map toUpper (explode ascii) = explode upperascii)
val test33 = 
    check'(fn _ => 
           map toUpper (explode graphchars)
           seq map toLower (explode graphchars)
           seq true)

val test34a =
    check'(fn _ => 
           map pred (List.drop(allchars, 1)) = List.take(allchars, 255));
val test34b = (pred minChar seq "WRONG")
              handle Chr => "OK" | _ => "WRONG";
val test35a =
    check'(fn _ => 
           map succ (List.take(allchars, 255)) = List.drop(allchars, 1));
val test35b = (succ maxChar seq "WRONG")
              handle Chr => "OK" | _ => "WRONG";
end


(* Test cases for SML character escape functions. *)

val test36 = 
    let fun chk (arg, res) = Char.toString arg = res
    in check'(fn _ => List.all chk 
              [(#"\000", "\\^@"),
               (#"\001", "\\^A"),
               (#"\006", "\\^F"),
               (#"\007", "\\a"),
               (#"\008", "\\b"),
               (#"\009", "\\t"),
               (#"\010", "\\r"),
               (#"\011", "\\v"),
               (#"\012", "\\f"),
               (#"\013", "\\n"),
               (#"\014", "\\^N"),
               (#"\031", "\\^_"),
               (#"\032", " "),
               (#"\126", "~"),
               (#"\\", "\\\\"),
               (#"\"", "\\\""),
               (#"A", "A"),
               (#"\127", "\\127"),
               (#"\128", "\\128"),
               (#"\255", "\\255")])
    end;

val test37 = 
    let val chars = List.tabulate(256, chr)
        fun chk c = Char.fromString(Char.toString c) = SOME c
    in check'(fn _ => List.all chk chars) end

val test38 =                 
    let fun chkFromString (arg, res) = Char.fromString arg = SOME res
        val argResList = 
            [("A", #"A"),
             ("z", #"z"),
             ("@", #"@"),
             ("~", #"~"),
             ("\\a", #"\007"),
             ("\\b", #"\008"),
             ("\\t", #"\009"),
             ("\\r", #"\010"),
             ("\\v", #"\011"),
             ("\\f", #"\012"),
             ("\\n", #"\013"),
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
    in 
        check'(fn _ => List.all chkFromString argResList)
    end;

val test39 = 
    check'(fn _ => List.all (fn arg => Char.fromString arg = NONE)
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

val test40 = 
    let val chars = List.tabulate(256, chr)
    in check'(fn _ => 
              List.map SOME chars 
              = List.map Char.fromCString (List.map Char.toCString chars))
    end;

val test41 = 
    let val argResList = 
            [(#"\010", "\\r"),
             (#"\009", "\\t"),
             (#"\011", "\\v"),
             (#"\008", "\\b"),
             (#"\013", "\\n"),
             (#"\012", "\\f"),
             (#"\007", "\\a"),
             (#"\\", "\\\\"),
             (#"?", "\\?"),
             (#"'", "\\'"),
             (#"\"", "\\\"")]
    in
        check'(fn _ => 
               List.all (fn (arg, res) => Char.toCString arg = res) argResList)
    end;

val test42 = 
    let fun checkFromCStringSucc (arg, res) = 
            str (valOf (Char.fromCString arg)) = res
        val argResList = 
            [("\\r", "\010"),
             ("\\t", "\009"),
             ("\\v", "\011"),
             ("\\b", "\008"),
             ("\\n", "\013"),
             ("\\f", "\012"),
             ("\\a", "\007"),
             ("\\\\",  "\\"),
             ("\\?", "?"),
             ("\\'", "'"),
             ("\\\"", "\""),
             ("\\1", "\001"),
             ("\\11", "\009"),
             ("\\111", "\073"),
             ("\\1007", "\064"),
             ("\\100A", "\064"),
             ("\\0",   "\000"),
             ("\\377", "\255"),
             ("\\18", "\001"),
             ("\\178", "\015"),
             ("\\1C", "\001"),
             ("\\17C", "\015"),
             ("\\x0", "\000"),
             ("\\xff", "\255"),
             ("\\xFF", "\255"),
             ("\\x1", "\001"),
             ("\\x11", "\017"),
             ("\\xag", "\010"),
             ("\\xAAg", "\170"),
             ("\\x0000000a", "\010"),
             ("\\x0000000a2", "\162"),
             ("\\x0000000ag", "\010"),
             ("\\x0000000A", "\010"),
             ("\\x0000000A2", "\162"),
             ("\\x0000000Ag", "\010"),
             ("\\x00000000000000000000000000000000000000000000000000000000000000011+",
              "\017")]
    in 
        check'(fn _ => List.all checkFromCStringSucc argResList)
    end;

val test43 = 
    let fun checkFromCStringFail arg = Char.fromCString arg = NONE
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
end
