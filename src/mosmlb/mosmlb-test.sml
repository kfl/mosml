(**
 * This is a simple program used for testing of .mlb parser/lexer.
 * It runs several unit tests, checking Scanner and Parser.
 *)

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

(* Unit test for listUnique function *)
fun testListUnique () =
    let
        val uniqTestList = ["hilo","tau","alpha","cau_"]
    in
        if uniqTestList <> 
            Mlb_functions.listUnique String.compare (uniqTestList @ (rev uniqTestList)) then
            print "List uniq failed.\n"
        else
            ()
    end

fun main () =
(
    testSplit ();
    testPathVarSearch ();
    testSubstitutePathVars ();
    testListUnique ()
)

val _ = (main() before OS.Process.exit OS.Process.success)
        handle e => (print ("Error occurred:\n"^exnMessage e^"\n");
                     OS.Process.exit OS.Process.failure)

