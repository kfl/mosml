(* General -- incomplete 1996-04-19, 1996-09-30, 1997-03-12 *)

use "auxil.sml";

exception NoExceptionRaised

fun getExn (f : unit -> 'a) = 
    (f (); NoExceptionRaised) handle e => e

fun prExn(exnStr, exn) =
    (print "\nShould be `"; print exnStr; print "':\n  ";
     print (exnName exn); print "\n  ";
     print (exnMessage exn));

exception E1;
exception E2 = E1;

val _ = List.map prExn
    [("E1 or E2",  E2),
     ("Bind",      getExn(fn _ => let val true = false in () end)),
     ("Match",     getExn(fn _ => (fn true => ()) false)),
     ("Subscript", getExn(fn _ => Vector.sub(vector [], ~1))),
     ("Size",      getExn(fn _ => Array.array(Array.maxLen+1, ()))),
(*   ("Overflow",  getExn(fn _ => Math.exp 1E99)),
     ("Domain",    getExn(fn _ => Math.ln ~1.0)),
*)   ("Div",       getExn(fn _ => 1 div 0)),
     ("Chr",       getExn(fn _ => Char.chr 9999999)),
     ("Fail",      Fail "demo"),
     ("Option",    getExn(fn _ => valOf NONE)),
     ("Empty",     getExn(fn _ => List.hd [])),
     ("SysErr",    getExn (fn _ => FileSys.modTime "exists.not")),
     ("Io",        getExn (fn _ => TextIO.openOut ":"))];
