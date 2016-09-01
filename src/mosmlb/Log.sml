(** Message types and log infrastructure.
 *  
 * TODO: add correct message descriptions and numbers.
 *)
structure Log = struct
    datatype warningMessage = DuplicatedSources of unit

    datatype errorMessage = 
        MLBGraphCycle of unit
      | FileNotFound of unit

    datatype logMessage = Warning of warningMessage 
                        | Error of errorMessage

    (* The number of the warning and its description. Do not reassign
     * same number to different warnings. If you remove warning, 
     * leave a hole in numbers. *)
    fun warningDescription (DuplicatedSources ()) = (1, "File is included twice.")

    fun errorDescription (MLBGraphCycle ()) = (1, "Cycle in mlb includes.")
      | errorDescription (FileNotFound ())  = (2, "File not found.")

    val debugLevel = ref NONE

    val failEarly = ref false

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
