(* arg.sml *)

open BasicIO Fnlib;

exception Bad of string

datatype spec =
    String  of (string -> unit)
  | Int     of (int -> unit)
  | Unit    of (unit -> unit)
  | Real    of (real -> unit)
;

datatype error =
    Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string
  | Message of string
;

fun stop error =
  let val progname = if Vector.length Miscsys.command_line > 0
                     then Vector.sub(Miscsys.command_line, 0)
                     else "(?)"
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
     output(std_err, message); output(std_err, "\n"); flush_out std_err;
     exit 2
  end;

prim_val sml_int_of_string : string -> int = 1 "sml_int_of_string";
prim_val sml_float_of_string : string -> real = 1 "sml_float_of_string";

fun listOfVector v =
  List.tabulate(Vector.length v, fn i => Vector.sub(v, i))
;

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
                       sml_int_of_string arg
                       handle Fail _ =>
                         stop (Wrong (s, arg, "an integer"))
                 in f arg_i; p t end
             | (Real f, arg::t) =>
                 let val arg_r =
                       sml_float_of_string arg
                       handle Fail _ =>
                         stop (Wrong (s, arg, "a real"))
                 in f arg_r; p t end
             | (_, []) => stop (Missing s)
          ) handle Bad m => stop (Message m)
        end
  in
    case listOfVector Miscsys.command_line of
        [] => ()
      | a::l => p l
  end;

