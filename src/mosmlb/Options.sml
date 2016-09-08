(** Options and their loading code. NOTE: options, that affect logging
 *  are declared in Log stucture (failEarly and debug). *)
structure Options = struct
    open Arg

    (* Global options of the program *)
    val alwaysMake = ref false
    val imitate = ref false
    val execFile = ref NONE (* if NONE, output file name is the name of .mlb *)
    val mlbFile = ref NONE

    val version : int * int = (0, 1) (* major, minor *)

    fun printVersion _ =
        let
            val (major, minor) = version
        in
            print ("Moscow MLB build tool v." ^ (Int.toString major) ^ "." ^
                   (Int.toString minor) ^ "\n");
            print "Defined path variables:\n";
            app (fn (variable, value) => 
                    print ("  " ^ variable ^ " = '" ^ value ^ "'\n"))
                (!Mlb.pathVariables);
            BasicIO.exit 0
        end

    fun setDebugLevel level =
        case level of
          "1" => Log.debugLevel := Log.Level 1
        | "2" => Log.debugLevel := Log.Level 2
        | "3" => Log.debugLevel := Log.Level 3
        | "print" => Log.debugLevel := Log.PrintParseTree
        | "test" => Log.debugLevel := Log.ReadPrintReadTest
        | _ => 
        (
            Log.debugLevel := Log.NoDebug; 
            Log.fatal "Invalid debug level." 
        )

    fun readCommandLine _ =
        let
            fun printUsage _ = 
            (
                print
                "usage: mosmlb [options] file.mlb\n\
                 \Options:\n\
                 \  -debug (1|2|3|print) Set debug level to certain level or print final Mlb tree\n\
                 \  -always-make         Rebuild all targets\n\
                 \  -keep-going          Continue as much as possible after an error\n\
                 \  -imitate             Do not actually run compiler and linker\n\
                 \  -version             Print version and predefined path variables\n\
                 \  -help                Print this message\n\
                 \  -o <file>            Place the output into <file>\n";
                 BasicIO.exit 0
            )

            fun assign r v = r := v
            fun assignTrue r () = r := true
            fun assignFalse r () = r := false
            fun assignOption r v = r := SOME v
        in
            Arg.parse 
               [("-debug", Arg.String setDebugLevel)
               ,("-always-make",Arg.Unit (assignTrue alwaysMake))
               ,("-keep-going", Arg.Unit (assignFalse Log.failEarly))
               ,("-imitate",    Arg.Unit (assignTrue imitate))
               ,("-version",    Arg.Unit printVersion)
               ,("-help",       Arg.Unit printUsage)
               ,("-o",          Arg.String (assignOption execFile))
               ] (assignOption mlbFile);
            case !mlbFile of
              NONE =>
            (
                print "Error: no .mlb file specified\n";
                printUsage ()
            )
            | _ => ()
        end
end
