(* recsucc.sml 
   Test of legal  recursive valbinds as per SML Definition 1997.
   sestoft@dina.kvl.dk 1999-01-06
 *)

(* Legal rebindings *)

datatype t = f | g of int
exception F and G of int

fun f x = x;
fun g x = x;
fun F x = x;
fun G x = x;

datatype t = f | g of int
exception F and G of int

val rec f = fn x => x;
val rec g = fn x => x;
val rec F = fn x => x;
val rec G = fn x => x;

datatype t = ff | gg of int
exception FF and GG of int
infix ff gg FF GG

fun x ff y = x;
fun x gg y = x;
fun x FF y = x;
fun x GG y = x;

fun it x = x;
val rec it = fn x => x;

datatype t = f | g of int
exception F and G of int
local 
val rec f = fn x => x;
val rec g = fn x => x;
val rec F = fn x => x;
val rec G = fn x => x;
in end

(* This will fail if f or g have been deprived of constructor status: *)

val test1 = (let val f = g 8 in false end handle Bind => true | _ => false);

(* Strange but legal lhs patterns *)

val rec f as g                      = fn x => x;
val rec f as (g)                    = fn x => x;
val rec f as g as h                 = fn x => g x;
val rec (f)                         = fn x => x;
val rec ((f))                       = fn x => x;
val rec f : int -> int              = fn x => x;
val rec f : int -> int : int -> int = fn x => x;
val rec _                           = fn x => x;
