(* 2000-01-18 *)

val _ = TextIO.print "Hello\n"; 

(* 
 Symptom: PUSH_GETGLOBAL_APPTERMi gave a segmentation violation in the 
          threaded runtime.
 Cause: wrong address translation while threading
 Fix:   runtime/expand.c function buildrealmap
*)

fun f1 a = (TextIO.print "Hello\n"; 255)

fun loop x f1 y = 1 + f1 ();

val _ = loop () f1 () : int;


(* 
 Symptom: CLOSURE and CLOSREC gave a segmentation violation in the 
          unthreaded runtime
 Cause: interpreted signed 30-bit ints as signed 31-bit ints
 Fix:  compiler/Buffcode.sml out_long rightshift signed
*)

fun h y = let fun f x = x in 1 + f y end

val _ = h 10;



(* 
 Symptom: SWITCH gave a segmentation violation
 Cause: wrong JUMPSWITCHTABLE: multiplied by 2, not 4
 Fix:  runtime/interp.c
*)

datatype t = A | B | C | D | E

fun f A = 1 
  | f B = 2
  | f C = 3
  | f D = 4
  | f E = 5;

val _ = f D; 
