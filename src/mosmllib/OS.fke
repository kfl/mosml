(* OS -- SML Basis Library *)

type syserror

exception SysErr of string * syserror option

val errorMsg : syserror -> string

(*  Various functions for interacting with the operating system.

   [errorMsg err] returns a string explaining the error message system
   error code err, as found in a SysErr exception.  The precise form
   of the strings are operating system dependent.  
*)
