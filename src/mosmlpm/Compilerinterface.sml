signature COMPILER_INTERFACE =
sig
    type debug_func = string list -> unit

    val compile : debug_func -> string list -> string list -> string -> bool
    val link    : debug_func -> string list -> string list -> string -> bool
end

(* 
   [debug_func] is a function for emitting debug information.

   [compile debug options context file] compiles `file` in the context
   described by `context` with the options `options`.  The return
   value describe if things went good (true) or bad (false).

   [link debug options context file] works like compile.

*)
