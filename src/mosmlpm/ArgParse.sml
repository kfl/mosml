(* COMMAND-LINE PARSING
 *
 * From compiler/Arg.sml
 *
 * Author: Peter Sestoft.
 *
 * Changed to rely on the Standard Basis instead of 
 * compiler internals. (Henning Niss)
 *)

structure ArgParse :> ArgParse =
struct

open BasicIO

exception Bad of string

datatype spec =
    String  of (string -> unit)
  | Int     of (int -> unit)
  | Unit    of (unit -> unit)
  | Real    of (real -> unit)

datatype error =
    Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string

fun stop error =
  let val progname = CommandLine.name()
      val message =
        case error of
            Unknown s =>
              progname ^ ": unknown option: \"" ^ s ^ "\"."
          | Missing s
              => progname ^ ": option \"" ^ s ^ "\" needs an argument."
          | Wrong (opt, arg, expected)
              => progname ^ ": wrong argument \"" ^ arg ^ "\"; option \""
                   ^ opt ^ "\" expects " ^ expected ^ "."
          | Message s
              => progname ^ ": " ^ s
  in
     raise Bad (message)
  end;

fun lookup k [] = raise Subscript
  | lookup k ((a, v) :: xs) =
    if k = a then v else lookup k xs

fun parse speclist anonfun =
  let fun p [] = ()
        | p (s::t) =
            if size s >= 1 andalso CharVector.sub(s, 0) = #"-"
            then do_key s t
            else ((anonfun s; p t)
                   handle Bad m => stop (Message m))
      and do_key s l =
        let val action =
              lookup s speclist
                handle Subscript => stop (Unknown s)
        in
          (case (action, l) of
               (Unit f, l) => (f (); p l)
             | (String f, arg::t) => (f arg; p t)
             | (Int f, arg::t) =>
                 let val arg_i =
                       case Int.fromString arg of
			   SOME i => i
                       |   NONE =>
                             stop (Wrong (s, arg, "an integer"))
                 in f arg_i; p t end
             | (Real f, arg::t) =>
                 let val arg_r =
                       case Real.fromString arg of
			   SOME r => r
                       |   NONE =>
                             stop (Wrong (s, arg, "a real"))
                 in f arg_r; p t end
             | (_, []) => stop (Missing s)
          ) handle Bad m => stop (Message m)
        end
  in
      p (CommandLine.arguments())
  end;

end (* structure ArgParse *)
