structure C = struct 
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

(* local unqualified *)

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

(* structures *)

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

(* unqualified *)
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

(* functors *)

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

(* unqualified *)
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

(* local unqualified *)
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

(* open A B X *)
val _ = TextIO.print "\n open A B X";
open A B X
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

val _ = TextIO.print "\nlocal open A B X";
local open A B X in
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

(* open B A X *)

val _ = TextIO.print "\nopen B A X";
open B A X
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

val _ = TextIO.print "\nlocal open B A X";
local open B A X in
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

(* open B X A *)

val _ = TextIO.print "\nopen B X A" ;
open B X A
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

val _ = TextIO.print "\nlocal open B X A";
local open B X A  in
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

(* open A X B *)

val _ = TextIO.print "\nopen A X B" ;
open A X B
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

val _ = TextIO.print "\nlocal open A X B";
local open A X B  in
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

(* open X A B  *)
val _ = TextIO.print "\nopen X A B" ;
open X A B
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

val _ = TextIO.print "\nlocal open X A B";
local open X A B  in
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

(* open X B A *)
val _ = TextIO.print "\nopen X B A" ;
open X B A
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

val _ = TextIO.print "\nlocal open X B A";
local open X B A  in
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

val _ = TextIO.print "\n";

end;