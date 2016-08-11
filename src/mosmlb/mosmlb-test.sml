(*
 * This is a simple program used for testing of .mlb parser/lexer.
 *
 * It reads single .mlb file into a Parse tree, prints this
 * parse tree into intermediate file. After that it reads
 * intermediate file into second Parse tree. Finally it compares
 * both trees.
 * 
 * It accepts two filenames - one is initial file to read,
 * second - the name of intermediate .mlb file.
 *)
fun parseAndPrint filename =
    let 
        val mlbAST = Mlb_functions.openParseSingleFile filename
    in
        Mlb_functions.printAST mlbAST
    end

fun main () =
    let 
        val args = CommandLine.arguments () 
    in
        case args of
          [] => raise Fail "Error: no .mlb file specified.\n"
        | _::[] => raise Fail "Error: no .mlb file specified.\n"
        | file::intermediateFile::_ => parseAndPrint file
    end

fun print_usage () = 
    print ("Usage: " ^ (CommandLine.name ()) ^ " file.mlb intermediate.mlb\n")

val _ = (main() before OS.Process.exit OS.Process.success)
        handle e => (print ("Error occurred:\n"^exnMessage e^"\n");
                     print_usage (); OS.Process.exit OS.Process.failure)

