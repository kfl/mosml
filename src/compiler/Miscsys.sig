(* Miscsys.sig -- not part of the new unified basis, 12-27-94 *)

(* This module provides a simple interface to the operating system. *)

(* exception SysErr of string * syserror option; *)
        (* This exception is identical to sys__Sys_error in the runtime.
           Raised by some functions, when the underlying system calls
           fail. The argument to SysErr describes the error.
           The texts of the error messages are implementation-dependent,
           and should not be relied upon to catch specific system errors. *)

prim_val command_line : string Vector.vector = 0 "command_line";
        (* The command line arguments given to the process.
           The first element is the command name used to invoke the
           program. *)

prim_val interactive: bool = 0 "interactive";
        (* True if we're running under the toplevel system. False if
           we're running as a standalone program. *)

prim_val getenv : string -> string = 1 "sys_getenv";
        (* Return the value associated to a variable in the process
           environment. Raise [Not_found] if the variable is unbound. *)

prim_val catch_interrupt : bool -> unit = 1 "sys_catch_break"
  (* Currently, this doesn't work properly in the top-level system,
     because it calls this primitive itself to prevent the system
     from being interrupted while in critical intervals. *)
        (* [catch_interrupt] governs whether user interrupt terminates
           the program or raises the [Interrupt] exception. Call
           [catch_interrupt true] to enable raising [Interrupt],
           and [catch_interrupt false] to let the system terminate
           the program on user interrupt. *)

val remove : string -> unit

val rename : {old: string, new: string} -> unit

val chdir  : string -> unit


(* [remove f] deletes the file [f] from the operating system.

   [rename{new, old}] renames file [old] to [new].

   [chdir dir] changes the current working directory of the process.
   Note that there is no easy way of getting the current working
   directory from the operating system.
*)

