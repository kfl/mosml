(* Evaluation order *)

(let val nil = [1,2] and _ =
     raise Io{function = "WRONG",  name = "WRONG", cause = Fail "WRONG"}
 in "DEAD" end)
handle Bind => "OK" | _ => "WRONG";
((fn 0 => "WRONG") 1)
handle Match => "OK" | _ => "WRONG";

(map (raise Io{function = "OK", name = "OK", cause = Fail "OK"})
 (raise Io{function = "WRONG", name = "WRONG", cause = Fail "WRONG"}))
handle Io x => [x];

(raise Io{function = "OK", name = "OK", cause = Fail "OK"},
 raise Io{function = "WRONG", name = "WRONG", cause = Fail "WRONG"})
handle Io x => (x, x);

{bbb = raise Io{function = "OK", name = "OK", cause = Fail "OK"},
 aaa = raise Io{function = "WRONG", name = "WRONG", cause = Fail "WRONG"}}
handle Io x => {aaa=x, bbb=x};

fun pr s = (TextIO.output (TextIO.stdOut, s); s);
fun x7 a b c d e f g = (a, b, c, d, e, f, g);
x7 "1" (pr "2") "3" (pr "4") "5" (pr "6") "7";

datatype ('a, 'b) AB = NILab | CONSab of {a: 'a, b: 'b};
CONSab {a=pr "a", b=pr "b"};
CONSab {b=pr "b", a=pr "a"};

(* Top-level bindings *)

val f = fn x => x;
val f = fn 0 => 1 | x => x * f(x-1);
if f 4 = 12 then "OK" else "WRONG";
