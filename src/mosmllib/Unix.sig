(* Unix -- SML Basis Library *)

type proc
type signal = Signal.signal

val executeInEnv : string * string list * string list -> proc 
val execute      : string * string list -> proc 
val streamsOf    : proc -> TextIO.instream * TextIO.outstream
val kill         : proc * signal -> unit
val reap         : proc -> Process.status 

(* 
   This structure allows Moscow ML programs to start other processes
   and to communicate with them.  

   Child processes are not automatically terminated when the parent
   (ML) process terminates.  To forcibly terminate a child process pr,
   use Unix.kill(pr, Signal.term).  Then, to remove the terminated
   process from the operating system tables, call Unix.reap(pr).

   The protocol for communication between the ML program and its child
   process must be designed with some care, typically using
   non-blocking input for reading from the child process.

   [proc] is the type of processes started by the ML program.

   [signal] is the type of Unix-style signals, which can be sent to
   another process.  Signal values must be obtained from the Signal
   structure.

   [execute (cmd, args)] asks the operating system to execute the
   command cmd with the argument list args, as a separate process.
   Two pipes connected to the standard input and standard output of
   the new process are created; these may be obtained using streamsOf.
   A proc value representing the new process is returned.  The new
   process executes using the same environment as the calling process.
   Raises Fail in case of failure, e.g. if the process or the pipes
   cannot be created.

   Typically, the cmd argument will be the full pathname of an
   executable.  On Unix systems, simple command searching as done by
   the shell, allowing cmd to be a relative pathname, can be achieved
   by using
        execute("/bin/sh", "-c" :: concat (cmd :: " " :: args))

   [executeInEnv (cmd, args, env)] asks the operating system to
   execute the command cmd with the argument list args in the
   environment env, as a separate process.  Returns a proc value
   representing the new process.  Typically, a string in the env list
   has the form "NAME=VALUE".  See also Process.getEnv.  

   [streamsOf pr] returns a pair (ins, outs) of input and output
   streams associated with process pr.  The standard output of pr is
   the source for the input stream ins, and the standard input of pr
   is the sink for the output stream outs.

   [reap pr] closes the input and output streams associated with pr,
   and then suspends the current (ML) process until the process
   corresponding to pr terminates.  Returns the exit status given by
   pr when it terminated.  Raises Fail in case of failure, e.g. if pr
   has already been reaped.

   Under Unix, information about a terminated process remains in the
   system tables until the process is reaped.  Thus, an ML program
   using execute or executeInEnv must make sure to reap any process it
   has created, or else the system tables will fill up.

   [kill (pr, s)] sends the signal s to the process pr.  Raises Fail
   in case of failure, e.g. if pr has already been killed.
*)
