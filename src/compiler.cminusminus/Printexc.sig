(* A catch-all exception handler *)

val f: ('a -> 'b) -> 'a -> 'b;
        (* [f fn x] applies [fn] to [x] and returns the result.
           If the evaluation of [fn x] raises any exception, the
           name of the exception is printed on standard error output,
           and the programs aborts with exit code 2.
           Typical use is [f main ()], where [main], with type
           [unit->unit], is the entry point of a standalone program, to catch
           and print stray exceptions.
           For [f] to work properly, the program must be linked
           with the [-g] option. *)
