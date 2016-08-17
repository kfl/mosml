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

(* Unit test for function Scanner.findAndSplit. *)
fun testSplit () =
    let
        val strTestList = ["hilo","tau","alpha","cau_"]
    in
        app (fn a =>
            app (fn b =>
                let
                    val (a1, b1) = Scanner.findAndSplit #"$" (a ^ "$" ^ b)
                in
                    case (String.compare (a, a1), String.compare (b, b1)) of
                      (EQUAL, EQUAL) => ()
                    | _ => print "Split test failed.\n"
                end
                ) strTestList) strTestList
    end

(* Unit test for Scanner.searchForPathVar *)
fun testPathVarSearch () =
    let
        val strTestList = ["hilo","tau","alpha","cau_"]
    in
        app (fn a =>
            app (fn b =>
                app (fn c =>
                    case Scanner.searchForPathVar (a ^ "$(" ^ b ^ ")" ^ c) of
                      NONE => print "Internal failure in path var search test.\n"
                    | SOME (b1, a1, c1) =>
                        case (String.compare (a, a1), String.compare (b, b1),
                                String.compare (c, c1)) of
                          (EQUAL, EQUAL, EQUAL) => ()
                        | _ => print "Path var search test failed.\n"
                ) strTestList) strTestList) strTestList
    end

(* Unit test for path variable extension *)
fun testSubstitutePathVars () =
    let
        val path = "$(SML_LIB)/happy.sml"
        val substituted = Scanner.substitutePathVars path
    in
        case String.compare (substituted, "/sml-lib-location/happy.sml") of
          EQUAL => ()
        | _ => print "Substitute path variables test failed.\n"
    end

fun main () =
    let 
        val args = CommandLine.arguments () 
    in
        testSplit ();
        testPathVarSearch ();
        testSubstitutePathVars ();
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
                else
                    ()
            end
    end

fun print_usage () = 
    print ("Usage: " ^ (CommandLine.name ()) ^ " file.mlb intermediate.mlb\n")

val _ = (main() before OS.Process.exit OS.Process.success)
        handle e => (print ("Error occurred:\n"^exnMessage e^"\n");
                     print_usage (); OS.Process.exit OS.Process.failure)

