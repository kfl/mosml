structure B = struct
val a = "B.a"
val b = "B.b"
val y = "B.y"
structure a = struct val v = "B.a" end
structure b = struct val v = "B.b" end
structure y = struct val v = "B.y" end
functor a ()= struct val v = "B.a" end
functor b ()= struct val v = "B.b" end
functor y ()= struct val v = "B.y" end
end;

