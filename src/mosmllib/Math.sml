(* Math.sml, 1995-02-21, 1995-11-06 *)

type real = real

val pi = 3.14159265358979323846;
val e  = 2.7182818284590452354;

prim_val sqrt : real -> real = 1 "sml_sqrt";
prim_val sin  : real -> real = 1 "sml_sin";
prim_val cos  : real -> real = 1 "sml_cos";
prim_val atan : real -> real = 1 "atan_float";
prim_val asin : real -> real = 1 "sml_asin";
prim_val acos : real -> real = 1 "sml_acos";

fun tan r = (sin r / cos r) handle Div => raise Domain;

local 
    prim_val atan2_ : real -> real -> real = 2 "sml_atan2";
    prim_val pow_   : real -> real -> real = 2 "sml_pow";
in
    fun atan2(x, y) = atan2_ x y;
    fun pow (x, y) = pow_ x y;
end

prim_val exp : real -> real = 1 "sml_exp";
prim_val ln  : real -> real = 1 "sml_ln";

local 
    val ln10 = ln 10.0 
in 
    fun log10 r = ln r / ln10
end;

prim_val sinh : real -> real = 1 "sml_sinh";
prim_val cosh : real -> real = 1 "sml_cosh";
prim_val tanh : real -> real = 1 "sml_tanh";
