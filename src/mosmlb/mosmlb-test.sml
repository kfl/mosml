(**
 * This is a simple program used for testing of .mlb parser/lexer.
 *
 * It reads single .mlb file into a Parse tree, prints this
 * parse tree into intermediate file. After that it reads
 * intermediate file into second Parse tree. Finally it compares
 * both trees.
 * 
 * It accepts two filenames - one is initial file to read,
 * second - the name of intermediate .mlb file. If both
 * trees are equal, it prints nothing, otherwise it notifies
 * about difference.
 *
 * NOTE - intermediate file is overwritten!
 *)

fun readAndWriteIntermediate filename intermediateFile =
    let 
        val initialAST = Mlb_functions.openParseSingleFile filename
        val outStream = BasicIO.open_out intermediateFile
    in
        Mlb_functions.printAST 
            (fn str => BasicIO.output (outStream, str)) initialAST;
        BasicIO.close_out outStream;
        initialAST
    end

fun main () =
    let 
        val args = CommandLine.arguments () 
    in
        case args of
          [] => raise Fail "Error: no .mlb file specified.\n"
        | _::[] => raise Fail "Error: no .mlb file specified.\n"
        | file::intermediateFile::_ => 
            let
                val initialAST = readAndWriteIntermediate file intermediateFile
                val secondAST = Mlb_functions.openParseSingleFile intermediateFile
            in
                if initialAST <> secondAST then
                    print "Parse trees are different.\n"
            end
    end

fun print_usage () = 
    print ("Usage: " ^ (CommandLine.name ()) ^ " file.mlb intermediate.mlb\n")

val _ = (main() before OS.Process.exit OS.Process.success)
        handle e => (print ("Error occurred:\n"^exnMessage e^"\n");
                     print_usage (); OS.Process.exit OS.Process.failure)

