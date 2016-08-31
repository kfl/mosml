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

(** Removes duplicates from list lst assuming it is already
 *  sorted. Result is in reverse order.
 *  @param lst list to uniq
 *)
fun listRevUniq ord lst =
    case lst of
      [] => lst
    | x::tail =>
        let
            val (a, rtail) = 
                foldl 
                    (fn (a, (current, result)) =>
                        if ord (a, current) = EQUAL then
                            (current, result)
                        else
                            (a, (current::result))
                    ) (x, []) tail
        in
            a::rtail
        end

fun execLinker sources mlbFile =
    let
        val output = "-o " ^ (Path.base mlbFile)
        val system = "-stdlib ../mosmllib -P none -P full -noheader "
        val objects = 
            map (fn (_, s) => Path.joinBaseExt {base = Path.base s, ext = SOME "uo"}) sources
        val sortedObjects = Listsort.sort String.compare objects
        val uniqObjects = listRevUniq String.compare sortedObjects
        val uniqObjects = map (fn s => " " ^ s ^ " ") uniqObjects
        val args = [system] @
            uniqObjects @ [output]
    in
        print ("OUTPUT = '" ^ output ^ "'\n");
        if OS.Process.isSuccess (
            OS.Process.system (String.concat ("../camlrunm ../mosmllnk "::args))) then
            print ("Linked " ^ output ^ ".\n")
        else
            print ("Linking of " ^ output ^ " failed.\n")
    end

fun main () =
    let 
        val args = CommandLine.arguments () 
    in
        case args of
          [] => print "Error: no .mlb file specified.\n"
        | file::_ => 
            let 
                val parseTree = Mlb_functions.loadMlbFileTree file
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
    end

val _ = (main() before OS.Process.exit OS.Process.success)
        handle e => (print ("Error occurred:\n"^exnMessage e^"\n");
		     OS.Process.exit OS.Process.failure)

