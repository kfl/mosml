(* Parsing of command line arguments. *)

(* This module provides a general mechanism for extracting options and
   arguments from the command line to the program. *)
(* Syntax of command lines.
    A keyword is a character string starting with a [-].
    An option is a keyword alone or followed by an argument.
    There are 4 types of keywords: Unit, String, Int, and Float.
    Unit keywords do not take an argument.
    String, Int, and Float keywords take the following word on the command line
    as an argument.
    Arguments not preceded by a keyword are called anonymous arguments.

    Examples ([foo] is assumed to be the command name):

-   [foo -flag           ](a unit option)
-   [foo -int 1          ](an int option with argument [1])
-   [foo -string foobar  ](a string option with argument ["foobar"])
-   [foo -real 12.34    ](a real option with argument [12.34])
-   [foo 1 2 3           ](three anonymous arguments: ["1"], ["2"], and ["3"])
-   [foo 1 2 -flag 3 -string bar 4]
-   [                    ](four anonymous arguments, a unit option, and
-   [                    ] a string option with argument ["bar"])
*)

datatype spec =
    String  of (string -> unit)
  | Int     of (int -> unit)
  | Unit    of (unit -> unit)
  | Real    of (real -> unit)
;

(*
    The concrete type describing the behavior associated with a keyword.
*)

val parse : (string * spec) list -> (string -> unit) -> unit;
(*
    [parse speclist anonfun]
    parses the command line, calling the functions in [speclist]
    whenever appropriate, and [anonfun] on anonymous arguments.
    The functions are called in the same order as they appear on the command
    line.
    The strings in the [(string * spec) list] are keywords and must
    start with a [-], else they are ignored.
    For the user to be able to specify anonymous arguments starting with a
    [-], include for example [("--", String anonfun)] in [speclist].
*)

exception Bad of string

(*
    Functions in [speclist] or [anonfun] can raise [Bad message]
    to reject invalid arguments.
*)
