(** 
 * ML Basis System frontend for MosML compiler.
 *)

fun parseAndPrint filename =
    let 
        val mlbAST = Mlb_functions.openParseSingleFile filename
    in
        Mlb_functions.printAST print mlbAST;
        Mlb_functions.extractPaths mlbAST
    end

(**
 * Parse .mlb file and automatically load other mlb files.
 * @param filename - name of the first file to load.
 *
 * @return list of tuples (path_to_mlb_file, ast) *)
fun loadMLBWithDeps filename =
    let
        fun iter astList (filename::tail) =
        let
            val ast = Mlb_functions.openParseSingleFile filename
            val includedFiles = Mlb_functions.extractPaths ast
            val includedMLB = 
                List.filter (fn (Mlb.MLBFile,_) => true | _ => false) includedFiles
            val fileNames = map (fn (_, file) => file) includedMLB
        in
            iter ((filename, ast)::astList) (fileNames@tail)
        end 
          | iter astList [] = astList
    in
        iter [] [filename]
    end

fun main () =
    let 
        val args = CommandLine.arguments () 
    in
        case args of
          [] => print "Error: no .mlb file specified.\n"
        | file::_ => 
            let 
                val _ = loadMLBWithDeps file
            in
                ()
            end
(*            app (fn (_, s) => print (s ^ "\n")) (parseAndPrint file) *)
    end

val _ = (main() before OS.Process.exit OS.Process.success)
        handle e => (print ("Error occurred:\n"^exnMessage e^"\n");
		     OS.Process.exit OS.Process.failure)

