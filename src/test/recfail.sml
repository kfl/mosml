(* recfail.sml 
   Test of illegal recursive valbinds as per SML Definition 1997.
   sestoft@dina.kvl.dk 1999-01-06
 *)

(* Illegal rebindings --- must fail *)

fun true x  = x;
fun false x = x;
fun nil x   = x;
fun [] x    = x;
fun ref x   = x;

val rec true    = fn x => x;
val rec false   = fn x => x;
val rec nil     = fn x => x;
val rec []      = fn x => x;
val rec (op ::) = fn x => x;
val rec ref     = fn x => x;

(* Illegally qualified variables *)

val rec General.x              = fn x => x;
fun General.f                  = fn x => x;     
val rec General.f as g         = fn x => x;
val rec f as General.g         = fn x => x;
val rec General.f as (g)       = fn x => x;
val rec f as (General.g)       = fn x => x;
val rec f as g as General.h    = fn x => g x;
val rec (General.f)            = fn x => x;
val rec ((General.f))          = fn x => x;
val rec General.f : int -> int = fn x => x;

(* Ill-formed patterns --- must fail *)

val rec #[]     = fn x => x;
val rec ()      = fn x => x;
val rec (a, b)  = fn x => x;
val rec {a, b}  = fn x => x;
val rec { 1=a } = fn x => x;
val rec [_]     = fn x => x;

(* Ill-formed funvalbind clauses --- must fail *)

fun _       x = x;
fun #[]     x = x;
fun ()      x = x;
fun (a, b)  x = x;
fun {a, b}  x = x;
fun { 1=a } x = x;
fun [_]     x = x;
