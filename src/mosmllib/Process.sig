(* OS.Process -- SML Basis Library *)

eqtype status

val success   : status
val failure   : status

val system    : string -> status

val atExit    : (unit -> unit) -> unit
val exit      : status -> 'a
val terminate : status -> 'a

val getEnv    : string -> string option

(* 
   Portable functions for manipulating processes.

   [success] is the unique status value that signifies successful
   termination of a process.  Note: MS DOS (sometimes) believes that
   all processes are successful.

   [failure] is a status value that signifies an error during
   execution of a process.  Note that in contrast to the success
   value, there may be several distinct failure values.

   [system cmd] asks the operating system to execute command cmd, and
   returns a status value.

   [atExit act] registers the action act to be executed when the
   current SML program calls Process.exit.  Actions will be executed
   in reverse order of registration.

   [exit i] executes all registered actions, then terminates the SML
   process with completion code i.

   [terminate i] terminates the SML process with completion code i 
   (but without executing the registered actions).

   [getEnv evar] returns SOME s if the environment variable evar is
   defined and is associated with the string s; otherwise NONE.
*)
