(* OS -- SML Basis Library *)

signature OS = sig

type syserror = syserror

exception SysErr of string * syserror option

val errorMsg      : syserror -> string

structure FileSys : FileSys
structure Path    : Path
structure Process : Process

end

(*  Various functions for interacting with the operating system.

   [errorMsg err] returns a string explaining the error message system
   error code err, as found in a SysErr exception.  The precise form
   of the string depends on the operating system.  
*)
