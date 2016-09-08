(** 
 * ML Basis System frontend for MosML compiler.
 *)

fun execCompiler toplevel (_, file) = 
    let
        val system = "-stdlib ../mosmllib -P none -P full "
        val args = if toplevel then 
            [system, "-toplevel ", file]
        else
            [system, file]
    in
        if OS.Process.isSuccess (
            OS.Process.system (String.concat ("../camlrunm ../mosmlcmp "::args))) then
            print ("Compiled " ^ file ^ ".\n")
        else
            print ("Compilation of " ^ file ^ " failed.\n")
    end

fun execLinker sources mlbFile =
    let
        val output = "-o " ^ (Path.base mlbFile)
        val system = "-stdlib ../mosmllib -P none -P full -noheader "
        val objects = 
            map (fn (_, s) => Path.joinBaseExt {base = Path.base s, ext = SOME "uo"}) sources
        val uniqObjects = Mlb_functions.listUnique String.compare objects
        val uniqObjects = map (fn s => " " ^ s ^ " ") uniqObjects
        val args = [system] @ (rev uniqObjects) @ [output]
    in
        print ("OUTPUT = '" ^ (String.concat ("../camlrunm ../mosmllnk "::args)) ^ "'\n");
        if OS.Process.isSuccess (
            OS.Process.system (String.concat ("../camlrunm ../mosmllnk "::args))) then
            print ("Linked " ^ output ^ ".\n")
        else
            print ("Linking of " ^ output ^ " failed.\n")
    end

fun buildProject file parseTree =
    let 
        val fileList = Mlb_functions.extractPaths parseTree
        val smlFileList = 
            List.filter 
                (fn (fileType, _) => 
                    case fileType of
                      Mlb.MLBFile => false
                    | Mlb.LoadedMLBFile _ => false
                    | Mlb.FailedMLBFile _ => false
                    | _ => true
                ) fileList
    in
        case smlFileList of
          topLevel::[] =>
          (
            execCompiler true topLevel;

            execLinker smlFileList file
          )
        | topLevel::other =>
          (
            app (execCompiler false) (rev other);
            execCompiler true topLevel;

            execLinker smlFileList file
          )
        | [] => print "No files to compile.\n"
    end

(**
 * Prints parse tree into intermediate file. After that it reads
 * intermediate file into second Parse tree. Finally it compares
 * both trees. The name of intermediate file is generated randomly.
 *
 * @param initialAST the initial parse tree.
 *)
fun parsePrintParseTest initialAST =
    let
        val intermediateFile = OS.FileSys.tmpName ()
        val outStream = BasicIO.open_out intermediateFile
        val _ = Mlb_functions.printAST 
            (fn str => BasicIO.output (outStream, str)) initialAST;
        val _ = BasicIO.close_out outStream
        val secondAST = Mlb_functions.loadSingleMLBFile intermediateFile
    in
        if initialAST <> secondAST then
            Log.report "test FAILED, parse trees are different"
        else
            Log.report "test PASSED, parse trees are equal"
    end

fun main () =
    let 
        val _ = Options.readCommandLine ()
        val file = Option.valOf (!Options.mlbFile)
        val parseTree = Mlb_functions.loadMlbFileTree file
    in
        case !Log.debugLevel of
          Log.PrintParseTree => Mlb_functions.printAST print parseTree
        | Log.ReadPrintReadTest => parsePrintParseTest parseTree
        | _ => buildProject file parseTree
    end

val _ = (main() before OS.Process.exit OS.Process.success)
        handle e => (print ("Error occurred:\n"^exnMessage e^"\n");
		     OS.Process.exit OS.Process.failure)

