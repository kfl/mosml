(** Message types and log infrastructure.
 *  
 * TODO: add correct message descriptions and numbers.
 *)
structure Log = struct

    type position = int*int (* position in file = line, column *)

    (* location of error/warning - name of file, start position, end position *)
    type location = string*position*(position option)

    (* Types of warning/error messages - add new warnings add errors here. *)
    datatype warningMessage = DuplicatedSources of unit

    datatype errorMessage = 
        MLBGraphCycle of string * string list
      | FileNotRead of string
      | UnexpectedCommentEnd of location
      | ParseError of string

    datatype logMessage = Warning of warningMessage 
                        | Error of errorMessage

    (* The number of the warning and its description. Do not reassign
     * same number to different warnings. If you remove warning, 
     * leave a hole in numbers. Same for errors. *)
    fun warningDescription (DuplicatedSources ()) = (1, "File is included twice")

    fun errorDescription (MLBGraphCycle (file, includeStack)) = 
        (1, "Cycle in mlb includes: " ^ file ^
            (foldl (fn (f, str) => ("\n  is included from " ^ f ^ str)) "" includeStack))
      | errorDescription (FileNotRead filename)  = (2, ("File '" ^ filename ^ "' can not be loaded"))
      | errorDescription (ParseError filename)  = (3, ("File '" ^ filename ^ "' can not be parsed"))
      | errorDescription (UnexpectedCommentEnd (file, (line, _), _)) = 
        (4, ("Unexpected commend end in " ^ file ^ " at line " ^ (Int.toString line)))

    (* Global variables of the program. *)
    val debugLevel = ref (SOME 2)
    val failEarly = ref true
    val log = ref [] : logMessage list ref (* Warnings and errors, collected so far. *)

    (* Printing for debug, report and fatal messages. *)
    fun prettyPrint level msg = print (level ^ ": " ^ msg ^ ".\n")

    (* Printing for Warning and Error messages. *)
    fun prettyPrintWE level (n, msg) = 
        print (level ^ " " ^ (Int.toString n) ^ ": " ^ msg ^ ".\n")

    (* Functions to report events to log subsystem. *)
    fun debug level msg =
        case !debugLevel of
          NONE => ()
        | SOME cutoff =>
            if cutoff >= level then
                prettyPrint "Debug" msg
            else
                ()

    fun fatal msg =
    (
        prettyPrint "Fatal" msg;
        BasicIO.exit (~1)
    )

    fun report msg = prettyPrint "Report" msg

    fun warning msg =
    (
        log := (Warning msg)::(!log);
        prettyPrintWE "Warning" (warningDescription msg)
    )

    fun error msg =
    (
        log := (Error msg)::(!log);
        prettyPrintWE "Error" (errorDescription msg);
        
        if !failEarly then
            BasicIO.exit (~1)
        else
            ()
    )

end
