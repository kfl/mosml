signature S = sig end;
functor F(X:S)(Y:S)(Z:S)=struct val _ = printVal 5 end;
structure R = (let val _ = printVal 4 in F end)(val x = printVal 3)(val x = printVal 2)(val x = printVal 1);


functor F(X:sig val x:int end)(Y: sig val y:int end)(Z: sig val z:int end)=struct val _ = printVal 5 end;
structure R =  (let val _ = printVal 4 in F end)(val x = printVal 3)(val y = printVal 2)(val z = printVal 1);

signature S = sig val x: int end;
functor F(X:S)(Y:S)(Z:S)=struct val _ = printVal 5 end;
structure R = (let val _ = printVal 4 in F end) (val x = printVal 3 val y = ())(val x = printVal 2 val y = ())(val x = printVal 1 val y = ());

signature S = sig val x: int end;
functor F(X:S)(Y:S)(Z:S)=struct val _ = printVal 4 end;
functor R = (let val _ = printVal 4 in F end) (val x = printVal 1 val y = ());

fun f x y z = printVal 4;
f (printVal 1) (printVal 2) (printVal 3);

val g = fn x => (fn y => (fn z => printVal 4));
g (printVal 1) (printVal 2) (printVal 3);

val h = fn x => let in fn y => let in fn z => printVal 4 end end;
h (printVal 1) (printVal 2) (printVal 3);

val h = fn x => let val x = 1 in fn y => let val x = 2 in fn z => printVal 4 end end;
h (printVal 1) (printVal 2) (printVal 3);
