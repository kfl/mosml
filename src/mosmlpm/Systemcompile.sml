(* This is an interface to mosml's compiler and linker based on
   OS.Process.system.
*)

structure Compiler :> COMPILER_INTERFACE =
struct
    
    type debug_func = string list -> unit

    fun insertSep sep []      = []
      | insertSep sep (x::xs) = x :: sep :: insertSep sep xs     

    structure Proc = OS.Process
    
    fun compile debug options files file =
	let val cont  = options @ (files @ [file])
	    val args  = insertSep " " cont
	    val sargs = String.concat ("mosmlc -c " ::args)
	in  
	    debug [sargs] 
	  ; Proc.system sargs = Proc.success
	end

    fun link debug options files file =
	let val cont  = options @ files
	    val args  = insertSep " " cont
	    val sargs = String.concat ("mosmlc -o " :: file :: " " :: args)
	in  
	    debug [sargs] 
	  ; Proc.system sargs = Proc.success
	end
    
end
