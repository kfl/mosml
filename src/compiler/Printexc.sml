(* A catch-all exception handler *)

open Obj BasicIO Nonstdio;

fun errString s = output(std_err, s);

fun f fct arg =
(
  (fct arg)
  handle x =>
    (
    flush_out std_out;
    (case x of
         Out_of_memory =>
           errString "Out of memory"
       | Fail s =>
           (errString "Evaluation failed: "; errString s)
       | Invalid_argument s =>
           (errString "Invalid argument: "; errString s)
       | SysErr(msg, _) =>
           (errString "I/O failure: "; errString msg)
       | x =>
	   (errString "Uncaught exception: ";
	    errString (exnMessage x); 
	    flush_out std_err)
    );
    errString "\n"; flush_out std_err;
    BasicIO.exit 2
    )
);
















