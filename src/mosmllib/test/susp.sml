(* test/susp.sml -- 1995-05-21 *)

use "auxil.sml";
val _ = load "Susp";

local 
    open Susp
    val v = ref 0
    fun inc () = v := !v + 1;
in
val x = 177;
val a1 = delay(fn () => (inc (); 1 + x));
val test1 = check'(fn () => !v = 0);
val x = 200;
val test2 = check'(fn () => 178 = force a1);
val test3 = check'(fn () => !v = 1);
val test4 = check'(fn () => 178 = force a1);
val test5 = check'(fn () => !v = 1);
val test6 = check'(fn () => 178 = force a1);
val test7 = check'(fn () => !v = 1);
end
