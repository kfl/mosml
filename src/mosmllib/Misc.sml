(* Misc.sml *)

type 'a array  = 'a Array.array

fun (g o f) x = g (f x);
fun a before b = a;

fun getOpt (SOME v, _) = v
  | getOpt (NONE,   a) = a;

fun isSome (SOME _) = true 
  | isSome NONE     = false;

fun valOf (SOME v) = v
  | valOf NONE     = raise Option.Option;

(* The definitions below implement the requirement that units
   Char, String and List are partially opened in the initial environment.
 *)

val chr = Char.chr;
val ord = Char.ord;

val explode = String.explode;
val implode = String.implode;
val concat = String.concat;
val str = String.str;
val substring = String.substring;

exception Empty = List.Empty;
val op @ = List.@;
val app = List.app;
val foldl = List.foldl;
val foldr = List.foldr;
val hd = List.hd;
val length = List.length;
val map = List.map;
val null = List.null;
val rev = List.rev;
val tl = List.tl;

val vector = Vector.fromList;

fun print s = TextIO.print s;
