fun f1 x = (x=x, x 1);

fun f2 x = (x 1, x=x);

fun f3a x = f3a [x];

fun f3b (x : 'a) = f3b [x];

fun f3c x = (x=x, f3c [x]);

fun f4 [x] = f4 x;

fun f5 (SOME x) = f5 x;

fun f6 (x, y, z) = f6([x], x, x);

fun f7 [x : bool] = f7 x ;

fun f8a (x : 'a) = x=x;

fun f8b (x : '_a) = x=x;

fun f9 {x, ...} = f9 x;

fun f10 (r as {a, b, c}) = r = {d=2};

fun f11 x = #11 = #11;

fun f12a (x as {u, v}) = #w x;

fun f12b (x as {u, v}) = #3 x;

fun f13 (x as {u, v}) = x = {z = 12};

fun f14 (x as (u, v)) = x = ([1],2,3);

fun f14 (x as (u, v)) = #3 x;

val f15 = (let val Id : 'a -> 'a = fn z => z in Id Id end,
	   fn z => z : 'a);

