(* test local opening of top-level structures *)
structure A = 
    struct  
	val a = "a";
	val b = "b";
	val c = "c";
	val d = "d";

	val e = "e"
	and f = "f"
	and g = "g"
	and h = "h";

	val i = "_i"
	and j = "_j"
	and k = "_k"
	and l = "_l";
	    
	val i = "i"
	and j = "j"
	and k = "k" 
	and l = "l";

	(* structures *)

	structure a = struct val v = "a" end;
	structure b = struct val v = "b" end;
	structure c = struct val v = "c" end;
	structure d = struct val v = "d" end;
	    
	structure e = struct val v = "e" end
	and f = struct val v = "f" end
	and g = struct val v = "g" end
	and h = struct val v = "h" end;
	    
	structure i = struct val v = "_i" end
	and j = struct val v = "_j" end
	and k = struct val v = "_k" end
	and l = struct val v = "_l" end;
	    
	    
	structure i = struct val v = "i" end
	and j = struct val v = "j" end
	and k = struct val v = "k" end 
	and l = struct val v = "l" end;
	    
	(* functors *)

	functor a () = struct val v = "a" end;
	functor b () = struct val v = "b" end;
	functor c () = struct val v = "c" end;
	functor d () = struct val v = "d" end;
	    
	functor e () = struct val v = "e" end
	and f () = struct val v = "f" end
	and g () = struct val v = "g" end
	and h () = struct val v = "h" end;
	    
	functor i () = struct val v = "_i" end
	and j () = struct val v = "_j" end
	and k () = struct val v = "_k" end
	and l () = struct val v = "_l" end;
	    
	functor i () = struct val v = "i" end
	and j () = struct val v = "j" end
	and k () = struct val v = "k" end 
	and l () = struct val v = "l" end;
    end;

structure Y =
struct
    val a = "a";
    structure A1 = A
    structure B = 
	struct
	    val a = "B.a"
	    val b = "B.b"
	val y = "B.y"
	
	structure a = struct val v = "B.a" end
	structure b = struct val v = "B.b" end
	structure y = struct val v = "B.y" end
    
	functor a ()= struct val v = "B.a" end
	functor b ()= struct val v = "B.b" end
        functor y ()= struct val v = "B.y" end
	end
    structure C1 = A
    structure D1 = A
    functor A1 () = struct end
end;

val _ = TextIO.print "linking values";

fun check id s = TextIO.print (if id = s then "\nOK: "^s else "\nFAIL expecting value A."^id^" found "^s);

val _ = check A.a "a";
val _ = check A.b "b";
val _ = check A.c "c";
val _ = check A.d "d";

val _ = check A.e "e";
val _ = check A.f "f";
val _ = check A.g "g";
val _ = check A.h "h";

val _ = check A.i "i";
val _ = check A.j "j";
val _ = check A.k "k";
val _ = check A.l "l";

(* unqualified *)

val () =  let

val _ = TextIO.print "\nopen A";

open A;

fun check id s = TextIO.print (if id = s then "\nOK: "^s else "\nFAIL expecting value "^id^" found "^s);

val _ = check a "a";
val _ = check b "b";
val _ = check c "c";
val _ = check d "d";

val _ = check e "e";
val _ = check f "f";
val _ = check g "g";
val _ = check h "h";

val _ = check i "i";
val _ = check j "j";
val _ = check k "k";
val _ = check l "l";
in ()
end;
(* local unqualified *)

val () = let
val _ = TextIO.print "\nlocal open A";
local open A in
fun check id s = TextIO.print (if id = s then "\nOK: "^s else "\nFAIL expecting value "^id^" found "^s);

val _ = check a "a";
val _ = check b "b";
val _ = check c "c";
val _ = check d "d";

val _ = check e "e";
val _ = check f "f";
val _ = check g "g";
val _ = check h "h";

val _ = check i "i";
val _ = check j "j";
val _ = check k "k";
val _ = check l "l";
end 
in 
()
end;

(* structures *)
val () = let 
val _ = TextIO.print "linking structures";

fun check id s = TextIO.print (if id = s then "\nOK: "^s else "\nFAIL expecting structure A."^id^" found "^s);

val _ = check A.a.v "a";
val _ = check A.b.v "b";
val _ = check A.c.v "c";
val _ = check A.d.v "d";

val _ = check A.e.v "e";
val _ = check A.f.v "f";
val _ = check A.g.v "g";
val _ = check A.h.v "h";

val _ = check A.i.v "i";
val _ = check A.j.v "j";
val _ = check A.k.v "k";
val _ = check A.l.v "l";
in
    ()
end;

(* unqualified *)
val () = let
val _ = TextIO.print "\nopen A";

fun check id s = TextIO.print (if id = s then "\nOK: "^s else "\nFAIL expecting structure "^id^" found "^s);

open A;

val _ = check a.v "a";
val _ = check b.v "b";
val _ = check c.v "c";
val _ = check d.v "d";

val _ = check e.v "e";
val _ = check f.v "f";
val _ = check g.v "g";
val _ = check h.v "h";

val _ = check i.v "i";
val _ = check j.v "j";
val _ = check k.v "k";
val _ = check l.v "l";
in ()
end;

val () = let
(* local unqualified *)
val _ = TextIO.print "\nlocal open A";

fun check id s = TextIO.print (if id = s then "\nOK: "^s else "\nFAIL expecting structure "^id^" found "^s);

local open A in
val _ = check a.v "a";
val _ = check b.v "b";
val _ = check c.v "c";
val _ = check d.v "d";

val _ = check e.v "e";
val _ = check f.v "f";
val _ = check g.v "g";
val _ = check h.v "h";

val _ = check i.v "i";
val _ = check j.v "j";
val _ = check k.v "k";
val _ = check l.v "l";
end
in 
()
end;

(* functors *)
val () = let
val _ = TextIO.print "linking functors";

fun check id s = TextIO.print (if id = s then "\nOK: "^s else "\nFAIL expecting sfunctor A."^id^" found "^s);

val _ = check let structure V = A.a() in V.v end "a";
val _ = check let structure V = A.b() in V.v end "b";
val _ = check let structure V = A.c() in V.v end "c";
val _ = check let structure V = A.d() in V.v end "d";

val _ = check let structure V = A.e() in V.v end "e";
val _ = check let structure V = A.f() in V.v end "f";
val _ = check let structure V = A.g() in V.v end "g";
val _ = check let structure V = A.h() in V.v end "h";

val _ = check let structure V = A.i() in V.v end "i";
val _ = check let structure V = A.j() in V.v end "j";
val _ = check let structure V = A.k() in V.v end "k";
val _ = check let structure V = A.l() in V.v end "l";
in
()
end;

(* unqualified *)
val () =  let
val _ = TextIO.print "\nopen A";

fun check id s = TextIO.print (if id = s then "\nOK: "^s else "\nFAIL expecting structure "^id^" found "^s);

open A;

val _ = check let structure V = a() in V.v end "a";
val _ = check let structure V = b() in V.v end "b";
val _ = check let structure V = c() in V.v end "c";
val _ = check let structure V = d() in V.v end "d";

val _ = check let structure V = e() in V.v end "e";
val _ = check let structure V = f() in V.v end "f";
val _ = check let structure V = g() in V.v end "g";
val _ = check let structure V = h() in V.v end "h";

val _ = check let structure V = i() in V.v end "i";
val _ = check let structure V = j() in V.v end "j";
val _ = check let structure V = k() in V.v end "k";
val _ = check let structure V = l() in V.v end "l";
in ()
end;

(* local unqualified *)
val () = let
val _ = TextIO.print "\nlocal open A in";

fun check id s = TextIO.print (if id = s then "\nOK: "^s else "\nFAIL expecting structure "^id^" found "^s);

local open A in

val _ = check let structure V = a() in V.v end "a";
val _ = check let structure V = b() in V.v end "b";
val _ = check let structure V = c() in V.v end "c";
val _ = check let structure V = d() in V.v end "d";

val _ = check let structure V = e() in V.v end "e";
val _ = check let structure V = f() in V.v end "f";
val _ = check let structure V = g() in V.v end "g";
val _ = check let structure V = h() in V.v end "h";

val _ = check let structure V = i() in V.v end "i";
val _ = check let structure V = j() in V.v end "j";
val _ = check let structure V = k() in V.v end "k";
val _ = check let structure V = l() in V.v end "l";
end
in
()
end;

(* shadowing of units by structures *)

structure X = 
    struct 
	val a = "X.a"
	structure a = struct val v = "X.a" end
	functor a ()= struct val v = "X.a" end
	val x = "X.x"
	structure x = struct val v = "X.x" end
	functor x ()= struct val v = "X.x" end
    end

fun check d id s = 
    TextIO.print ("\n"^d^(if id = s then " OK: "^s else "FAIL expecting "^id^" found "^s));

val () = let
(* open A B X *)
val _ = TextIO.print "\nopen A B X";
open A Y.B X
val _ = check "value" a "X.a";
val _ = check "value" b "B.b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "X.a";
val _ = check "structure" b.v "B.b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "X.a";
val _ = check "functor" (let structure V = b() in V.v end) "B.b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
in
()
end;

val () = let
val _ = TextIO.print "\nlocal open A Y.B X";
local open A Y.B X in
val _ = check "value" a "X.a";
val _ = check "value" b "B.b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "X.a";
val _ = check "structure" b.v "B.b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "X.a";
val _ = check "functor" (let structure V = b() in V.v end) "B.b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
end
in
 ()
end;
(* open Y.B A X *)

val () = let
val _ = TextIO.print "\nopen B A X";
open Y.B A X
val _ = check "value" a "X.a";
val _ = check "value" b "b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "X.a";
val _ = check "structure" b.v "b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "X.a";
val _ = check "functor" (let structure V = b() in V.v end) "b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
in ()
end;

val () = let
val _ = TextIO.print "\nlocal open B A X";
local open Y.B A X in
val _ = check "value" a "X.a";
val _ = check "value" b "b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "X.a";
val _ = check "structure" b.v "b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "X.a";
val _ = check "functor" (let structure V = b() in V.v end) "b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
end
in ()
end;

(* open B X A *)
val () = let
val _ = TextIO.print "\nopen B X A" ;
open Y.B X A
val _ = check "value" a "a";
val _ = check "value" b "b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "a";
val _ = check "structure" b.v "b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "a";
val _ = check "functor" (let structure V = b() in V.v end) "b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
in () 
end

val () = let
val _ = TextIO.print "\nlocal open B X A";
local open Y.B X A  in
val _ = check "value" a "a";
val _ = check "value" b "b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "a";
val _ = check "structure" b.v "b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "a";
val _ = check "functor" (let structure V = b() in V.v end) "b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
end
in
()
end;

(* open A X B *)
val () = let
val _ = TextIO.print "\nopen A X B" ;
open A X Y.B
val _ = check "value" a "B.a";
val _ = check "value" b "B.b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "B.a";
val _ = check "structure" b.v "B.b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "B.a";
val _ = check "functor" (let structure V = b() in V.v end) "B.b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
in
     ()
end;

val () = let
val _ = TextIO.print "\nlocal open A X B";
local open A X Y.B  in
val _ = check "value" a "B.a";
val _ = check "value" b "B.b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "B.a";
val _ = check "structure" b.v "B.b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "B.a";
val _ = check "functor" (let structure V = b() in V.v end) "B.b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
end
in 
   ()
end;

val () = let
(* open X A B  *)
val _ = TextIO.print "\nopen X A B" ;
open X A Y.B
val _ = check "value" a "B.a";
val _ = check "value" b "B.b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "B.a";
val _ = check "structure" b.v "B.b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "B.a";
val _ = check "functor" (let structure V = b() in V.v end) "B.b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
in
   () 
end;


val () = let 
val _ = TextIO.print "\nlocal open X A B";
local open X A Y.B  in
val _ = check "value" a "B.a";
val _ = check "value" b "B.b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "B.a";
val _ = check "structure" b.v "B.b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "B.a";
val _ = check "functor" (let structure V = b() in V.v end) "B.b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
end
in 
    ()
end;

val () = let
(* open X B A *)
val _ = TextIO.print "\nopen X B A" ;
open X Y.B A
val _ = check "value" a "a";
val _ = check "value" b "b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "a";
val _ = check "structure" b.v "b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "a";
val _ = check "functor" (let structure V = b() in V.v end) "b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
in 
()
end;

val () = let
val _ = TextIO.print "\nlocal open X B A";
local open X Y.B A  in
val _ = check "value" a "a";
val _ = check "value" b "b";
val _ = check "value" x "X.x";
val _ = check "value" y "B.y";
val _ = check "structure" a.v "a";
val _ = check "structure" b.v "b";
val _ = check "structure" x.v "X.x";
val _ = check "structure" y.v "B.y";
val _ = check "functor" (let structure V = a() in V.v end) "a";
val _ = check "functor" (let structure V = b() in V.v end) "b";
val _ = check "functor" (let structure V = x() in V.v end) "X.x";
val _ = check "functor" (let structure V = y() in V.v end) "B.y";
end
in
() 
end;

val _ = TextIO.print "\n";