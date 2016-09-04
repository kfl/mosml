(**
 * Functions for mlb file.
 *
 * In contrast to MLton parsing, each .mlb file is scanned and
 * parsed separately. The included files are loaded only after AST
 * is fully created.
 *)
structure Mlb_functions = struct

open Mlb

(** Pretty printers of AST structure. 
  * @param print the function used to output a string
  * @param basDecs the parse tree of a single .mlb file
  *)
fun printAST print basDecs =
    let
        (* Helper function that applies f to list and prints "and" in between. *)
        fun apA f [] = ()
          | apA f (x::[]) = f x
          | apA f (x::lst) =
            (
                f x;
                print " and";
                apA f lst
            )
        fun step indent = indent ^ "   " (* 3 spaces *)
        fun printInd indent str = print (indent ^ str)
        fun printBasBind indent (BasBind (basId, basExp)) =
        (
            print basId;
            print " = ";
            printBasExp indent basExp
        )
        and printOpen indent basIds =
        (
            printInd indent "open";
            app (fn id => print (" " ^ id)) basIds;
            print "\n"
        )
        and printStructure indent binds =
        (
            printInd indent "structure";
            apA (fn (StrId id) => print (" " ^ id)
                  | (StrBind (id1, id2)) => print (" " ^ id1 ^ " = " ^ id2))
                binds;
            print "\n"
        )
        and printSignature indent binds =
        (
            printInd indent "signature";
            apA (fn (SigId id) => print (" " ^ id)
                  | (SigBind (id1, id2)) => print (" " ^ id1 ^ " = " ^ id2))
                binds;
            print "\n"
        )
        and printFunctor indent binds =
        (
            printInd indent "functor";
            apA (fn (FunId id) => print (" " ^ id)
                  | (FunBind (id1, id2)) => print (" " ^ id1 ^ " = " ^ id2))
                binds;
            print "\n"
        )
        and printBasExp indent (Bas basDecList) = 
            (
                print "\n";
                printInd indent "bas\n";
                let val inIndent = step indent in
                    app (printBasDec inIndent) basDecList
                end;
                printInd indent "end\n"
            )
          | printBasExp indent (BasId basId) = print basId
          | printBasExp indent (Let (basDecList, basExp)) =
          (
            printInd indent "let\n";
            let val inIndent = step indent in
                app (printBasDec inIndent) basDecList;
                printInd indent "in\n";
                printBasExp indent basExp
            end;
            printInd indent "end\n"
          )
        and printPath indent (_, path) =
            print (indent ^ path ^ "\n")
        and printLocal indent (from, to) =
        (
            printInd indent "local\n";
            let val inIndent = step indent in
                app (printBasDec inIndent) from;
                printInd indent "in\n";
                app (printBasDec inIndent) to
            end;
            printInd indent "end\n"
        )
        and printAnnotation indent (annList, basDecList) =
        (
            print (indent ^ "ann\n");
            let val inIndent = step indent in
                app (fn ann => print (inIndent ^ ann ^ "\n")) annList;
                print (indent ^ "in\n");
                app (printBasDec inIndent) basDecList
            end;
            printInd indent "end\n"
        )
        and printBasDec indent (Basis basBindList) =
            (
                printInd indent "basis ";
                app (printBasBind (step indent)) basBindList
            )
          | printBasDec indent (Local locals) = printLocal indent locals
          | printBasDec indent (Open ids) = printOpen indent ids
          | printBasDec indent (Structure binds) = printStructure indent binds
          | printBasDec indent (Signature binds) = printSignature indent binds
          | printBasDec indent (Functor binds) = printFunctor indent binds
          | printBasDec indent (Path path) = printPath indent path
          | printBasDec indent (Annotation annotations) = printAnnotation indent annotations
    in
        app (printBasDec "") basDecs
    end

(** Read and parse individual .mlb file, do not load included .mlb files.
 * @param filename the name of the file.
 * @return parse tree. *)
fun loadSingleMLBFile filename =
    let	
        fun createLexerStream (is : BasicIO.instream) =
            Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)
        fun parseFile lexbuf = 
            let
                val _ = Scanner.fileName := filename
                val mlbFile = Parser.mlbFile Scanner.Lexer lexbuf
                val _ = Scanner.fileName := ""
            in
                mlbFile
            end
        val is     = BasicIO.open_in filename
        val lexbuf = createLexerStream is
        val basDecs = parseFile lexbuf
        val _      = BasicIO.close_in is
    in
        basDecs
    end

(** Read .mlb file and load all included .mlb files
 *  generating complete parse tree. All paths are
 *  converted to relative to root .mlb file path.
 *
 *  @param file name of the root .mlb file
 *  @return complete parse tree of the project
 *)
fun loadMlbFileTree file =
    let
        (* stack of paths currently processed .mlb files, relative to root .mlb *)
        val pathStack = ref [file] 

        (*  Expand parse tree, loading included .mlb files. 
          *
          * @param pathF function to apply to each path of the tree
          * @param basDecList parse tree of .mlb file (list of base declarations)
          *)
        fun expandParseTree pathF basDecList =
            let
                fun 
                    expandBasDec (Mlb.Basis basBindList) = 
                        Mlb.Basis (map expandBasBind basBindList)
                  | expandBasDec (Mlb.Local (basDecList1, basDecList2)) =
                        Mlb.Local (map expandBasDec basDecList1, map expandBasDec basDecList2)

                  | expandBasDec (Mlb.Open basIdList) = Mlb.Open basIdList
                  | expandBasDec (Mlb.Structure strBindList) = Mlb.Structure strBindList
                  | expandBasDec (Mlb.Signature sigBindList) = Mlb.Signature sigBindList
                  | expandBasDec (Mlb.Functor funBindList) = Mlb.Functor funBindList
        
                  | expandBasDec (Mlb.Path path) = 
                    (
                        case pathF path of
                          ((Mlb.LoadedMLBFile ast), file) =>
                          ( (* File successfully loaded. Maybe there is a cycle? *)
                            case (List.find (fn x => (String.compare (file,x)) = EQUAL) (!pathStack)) of
                              SOME _ => 
                              (
                                let
                                    val includeStack = foldl 
                                        (fn (f, stack) =>
                                            case stack of
                                              [] => 
                                                if String.compare (file, f) = EQUAL then
                                                    [f]
                                                else
                                                    []
                                            | _ => f::stack
                                        ) [] (rev (!pathStack));
                                in
                                    Log.error (Log.MLBGraphCycle (file, (rev includeStack)))
                                end;
                                Mlb.Path (Mlb.FailedMLBFile Mlb.CyclicDependency, file)
                              )
                            | NONE =>
                              (
                                pathStack := file::(!pathStack);
                                let
                                    val ast = expandParseTree pathF ast
                                in
                                    pathStack := tl (!pathStack);
                                    Mlb.Path ((Mlb.LoadedMLBFile ast), file)
                                end
                              )
                          )
                        | path => Mlb.Path path
                    )
                  | expandBasDec (Mlb.Annotation (annList, basDecList)) =
                        Mlb.Annotation (annList, map expandBasDec basDecList)
                and expandBasBind (Mlb.BasBind (basId, basExp)) = 
                        Mlb.BasBind (basId, expandBasExp basExp)
                and expandBasExp (Mlb.Bas basDecList) =
                        Mlb.Bas (map expandBasDec basDecList)
                  | expandBasExp (Mlb.BasId basId) = Mlb.BasId basId
                  | expandBasExp (Mlb.Let (basDecList, basExp)) =
                        Mlb.Let (map expandBasDec basDecList, expandBasExp basExp)
            in
                map expandBasDec basDecList
            end

        (** Load single .mlb file, convert path to relative to root .mlb path. *)
        fun loadPath (Mlb.MLBFile, file) = 
            (
                let
                    val parentMLB = hd (!pathStack)
                    val absoluteFile =
                        if Path.isAbsolute file then
                            file
                        else
                            Path.mkCanonical 
                                (Path.concat ((Path.dir parentMLB), file))
                    val ast = loadSingleMLBFile absoluteFile
                in
                    Log.debug 1 ("Included " ^ file);
                    Log.debug 1 ("Final path " ^ absoluteFile);
                    ((Mlb.LoadedMLBFile ast), absoluteFile)
                end
                handle OS.SysErr _ => 
                (
                    Log.error (Log.FileNotRead file);
                    ((Mlb.FailedMLBFile Mlb.ReadFailure), file)
                )
                  | Parsing.ParseError _ =>
                (
                    Log.error (Log.ParseError file);
                    ((Mlb.FailedMLBFile Mlb.ReadFailure), file)
                )
            )
          | loadPath path = path
    in
        expandParseTree loadPath (loadSingleMLBFile file)
    end

(** Extract all paths that are mentioned in the file.
  * @param basDecList parse tree of .mlb file (list of base declarations)
  *)
fun extractPaths basDecList =
    let
        fun 
            pathsOfBasDec (Mlb.Basis basBindList) = 
                foldl 
                    (fn ((Mlb.BasBind (_, basExp), paths)) => 
                        (pathsOfBasExp basExp) @ paths) [] basBindList
          | pathsOfBasDec (Mlb.Local (basDecList1, basDecList2)) =
          (
            let
                fun addPaths (basDec, paths) =
                    (pathsOfBasDec basDec) @ paths
            in
                (foldl addPaths [] basDecList1) @
                (foldl addPaths [] basDecList2)
            end
          )
          | pathsOfBasDec (Mlb.Open _) = []
          | pathsOfBasDec (Mlb.Structure _) = []
          | pathsOfBasDec (Mlb.Signature _) = []
          | pathsOfBasDec (Mlb.Functor _) = []
          | pathsOfBasDec (Mlb.Path path) = [path]
          | pathsOfBasDec (Mlb.Annotation _) = []
        and pathsOfBasExp (Mlb.Bas basDecList) =
            foldl (fn (basDec, paths) => (pathsOfBasDec basDec) @ paths) [] basDecList
          | pathsOfBasExp (Mlb.BasId _) = []
          | pathsOfBasExp (Mlb.Let (basDecList, basExp)) =
          (
            (foldl 
                (fn (basDec, paths) => (pathsOfBasDec basDec) @ paths) [] basDecList) 
            @ (pathsOfBasExp basExp)
          )
    in
        foldl 
            (fn (basDec, paths) => (pathsOfBasDec basDec) @ paths) [] basDecList
    end

end
