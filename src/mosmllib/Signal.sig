(* Signal -- SML Basis Library *)

eqtype signal

val abrt : signal 
val alrm : signal 
val bus  : signal 
val fpe  : signal 
val hup  : signal 
val ill  : signal 
val int  : signal 
val kill : signal 
val pipe : signal 
val quit : signal 
val segv : signal 
val term : signal 
val usr1 : signal 
val usr2 : signal 
val chld : signal 
val cont : signal 
val stop : signal 
val tstp : signal 
val ttin : signal 
val ttou : signal 

val toWord   : signal -> Word.word 
val fromWord : Word.word -> signal 

(* 
   [signal] is the type of Unix/Posix-style signals, which can be sent
   to another process.  

   [toWord sig] returns the signal number as an unsigned word.

   [fromWord w] returns the signal whose number is w.

   [abrt] is SIGABRT, the abort signal from abort(3).

   [alrm] is SIGALRM, a timer signal from alarm(1).

   [bus]  is SIGBUS, a bus error.

   [fpe]  is SIGFPE, a floating point exception.

   [hup]  is SIGHUP, a hangup.

   [ill]  is SIGILL, an illegal instruction.

   [int]  is SIGINT, an interrupt.

   [kill] is SIGKILL, the kill signal.

   [pipe] is SIGPIPE, a broken pipe.

   [quit] is SIGQUIT, a quit from keyboard.

   [segv] is SIGSEGV, a segmentation violation.

   [term] is SIGTERM, the termination signal.

   [usr1] is SIGUSR1, the first user signal.

   [usr2] is SIGUSR2, the second user signal.
   
   [chld] is SIGCHLD, child process stopped or terminated.

   [cont] is SIGCONT, continue if stopped.

   [stop] is SIGSTOP, signal to stop process.

   [tstp] is SIGTSTP, a stop signal typed at the tty.

   [ttin] is SIGTTIN, tty input for background process.

   [ttou] is SIGTTOU, tty output for background process.
 *) 
