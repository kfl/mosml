structure A = struct
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