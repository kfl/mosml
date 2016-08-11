(** 
 * ML Basis System frontend for MosML compiler.
 *)

fun parseAndPrint filename =
    let 
        val mlbAST = Mlb_functions.openParseSingleFile filename
    in
        Mlb_functions.printAST print mlbAST
    end

fun main () =
    let 
        val args = CommandLine.arguments () 
    in
        case args of
          [] => print "Error: no .mlb file specified.\n"
        | file::_ => parseAndPrint file
    end

val _ = (main() before OS.Process.exit OS.Process.success)
        handle e => (print ("Error occurred:\n"^exnMessage e^"\n");
		     OS.Process.exit OS.Process.failure)

