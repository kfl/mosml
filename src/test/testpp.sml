(* Check that installed failure of an installed prettyprinters is handled 
   correctly.  1999-10-26 
 *)

app load ["PP"];

exception PP1 and PP2 of int

fun intfail0 _ (i : int) = raise Fail "ak"
val _ = installPP intfail0;
val a = 0;

fun intfail1 _ (i : int) = raise PP1
val _ = installPP intfail1;
val b = 1;

fun intfail2 _ (i : int) = raise PP2 (i+1)
val _ = installPP intfail2;
val c = 2;

val cl = [5,6,7];

fun boolfail1 _ (b : bool) = let exception PP3 in raise PP3 end
val _ = installPP boolfail1;
val d = false;

fun boolfail2 _ (b : bool) = let exception PP4 of bool in raise PP4 false end
val _ = installPP boolfail2;
val e = true;

(* Check no infinite loop when printing the raised exception *)

exception PP5
fun exnfail1 _ (e : exn) = raise PP5
val _ = installPP exnfail1;
val f = Fail "wrong1";

fun exnfail2 _ (e : exn) = raise Io { cause = PP5, function = "nofun",
				      name = "noname" }
val _ = installPP exnfail2;
val g = Fail "wrong2";

exception PP7
fun intfail3 pps (i : int) = (PP.add_string pps "so far so good"; raise PP7)
val _ = installPP intfail3;
val h = 117;

