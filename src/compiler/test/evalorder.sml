val (v1,v2,v3,v4,v5) = (1,2,3,4,5)
signature S = sig end;
functor F(X:S)(Y:S)(Z:S)=struct val _ = printVal v5 end;
structure R = (let val _ = printVal v4 in F end)(val x = printVal v3)(val x = printVal v2)(val x = printVal v1);


functor F(X:sig val x:int end)(Y: sig val y:int end)(Z: sig val z:int end)=struct val _ = printVal v5 end;
structure R =  (let val _ = printVal v4 in F end)(val x = printVal v3)(val y = printVal v2)(val z = printVal v1);

signature S = sig val x: int end;
functor F(X:S)(Y:S)(Z:S)=struct val _ = printVal v5 end;
structure R = (let val _ = printVal v4 in F end) (val x = printVal v3 val y = ())(val x = printVal v2 val y = ())(val x = printVal v1 val y = ());

signature S = sig val x: int end;
functor F(X:S)(Y:S)(Z:S)=struct val _ = printVal v4 end;
functor R = (let val _ = printVal v4 in F end) (val x = printVal v1 val y = ());

fun f x y z = printVal v4;
f (printVal v1) (printVal v2) (printVal v3);

val g = fn x => (fn y => (fn z => printVal v4));
g (printVal v1) (printVal v2) (printVal v3);

val h = fn x => let in fn y => let in fn z => printVal v4 end end;
h (printVal v1) (printVal v2) (printVal v3);

val h = fn x => let val x = 1 in fn y => let val x = 2 in fn z => printVal v4 end end;
h (printVal v1) (printVal v2) (printVal v3);
