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
            app (printBasDec "") basDecList;
            printBasExp indent basExp
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

(** Read and parse individual .mlb file. 
 * @param filename the name of the file.
 * @return parse tree. *)
fun openParseSingleFile filename =
    let	
        fun createLexerStream (is : BasicIO.instream) =
            Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)
        fun parseFile lexbuf = Parser.mlbFile Scanner.Lexer lexbuf
        val is     = BasicIO.open_in filename
        val lexbuf = createLexerStream is
        val basDecs = parseFile lexbuf
        val _      = BasicIO.close_in is
    in
        basDecs
    end

end
