(* Main.sml *)

open List BasicIO Nonstdio;
open Miscsys Memory Fnlib Config Mixture Location Units Smlperv Rtvals Smltope;

val initialFiles = ref ([] : string list);

(* Initial loop *)

fun initial_loop () =
  while true do
    let in
      msgFlush();
      (case !initialFiles of
           [] =>
             raise Toplevel
         | filename :: rest =>
             (initialFiles := rest;
              evalUse filename))
      handle
          Toplevel =>
            (msgFlush();
             raise EndOfFile)
        | Interrupt =>
            (msgIBlock 0;
             msgPrompt "Interrupted."; msgEOL();
             msgEBlock();
             msgFlush();
             raise EndOfFile)
        | x =>
           (msgFlush();
            raise x)
    end
;

(* Main loop *)

fun main_loop () =
  while true do
    let in
      msgFlush();
      if !Exec_phr.quietdec then () else outputc std_out toplevel_input_prompt;
      flush_out std_out;
      let val isLast = loadToplevelPhrase (!input_lexbuf) in
        if isLast then raise EndOfFile else ()
      end
      handle
          EndOfFile =>
              (msgIBlock 0; msgEOL(); msgEBlock ();
	       msgFlush(); BasicIO.exit 0)
        | Toplevel =>
            msgFlush()
        | Interrupt =>
            (msgIBlock 0;
             msgPrompt "Interrupted.";
             msgEOL(); msgEBlock(); msgFlush())
        | x =>
            (msgFlush();
             raise x)
    end
;

fun anonymous s =
  initialFiles := !initialFiles @ [s];

fun set_stdlib p =
  path_library := p;

fun set_value_polymorphism b _ =
  value_polymorphism := b;

fun set_quietdec b _ =
  Exec_phr.quietdec := b;

fun add_include d =
  load_path := !load_path @ [d];

fun perv_set set' =
    let val set = Fnlib.stringToLower set'
    in 
	if set = "none" then 
	    (preloadedUnits := []; preopenedPreloadedUnits := [])
	else
	    (preloadedUnits          := 
	          lookup set preloadedUnitSets @ ["Help"];
	     preopenedPreloadedUnits := 
	          lookup set preopenedPreloadedUnitSets @ ["Help"])
	    handle Subscript =>
		raise Arg.Bad ("Unknown preloaded unit set " ^ set)
    end

fun main () =
(
  msgIBlock 0;
  msgString "Moscow ML version 1.43 (April 1998)";
  msgEOL();
  msgString "Mangled by e & eMake 06 May 1998";
  msgEOL();
  msgString "Use the Enter key to evaluate an input expression.";
  msgEOL();
  msgEBlock();
  msgFlush();
  let in
    perv_set "default";
    load_path := [];
    toplevel := true;
    (* Choose the default (value polymorphism or imperative types) here: *)
    value_polymorphism := true;
    Arg.parse [("-stdlib",    Arg.String set_stdlib),
               ("-I",         Arg.String add_include),
               ("-include",   Arg.String add_include),
               ("-P",         Arg.String perv_set),
               ("-perv",      Arg.String perv_set),
               ("-imptypes",  Arg.Unit (set_value_polymorphism false)),
               ("-valuepoly", Arg.Unit (set_value_polymorphism true)),
               ("-quietdec",  Arg.Unit (set_quietdec true))]
      anonymous;
    if !path_library <> "" then
      load_path := !load_path @ [!path_library]
    else ();
    resetGlobalDynEnv();
    resetSMLTopDynEnv();
    initPervasiveEnvironments();
    setGlobalVal 16 (Obj.repr true); (* 16: cf ../runtime/globals.h *)
    startCompilingUnit "Top";
    app evalLoad (!preloadedUnits);
    initInitialEnvironments();
    execToplevelOpen nilLocation "Meta";
    Miscsys.catch_interrupt true;
    input_lexbuf := Compiler.createLexerStream std_in;
    (initial_loop() handle EndOfFile => ());
    main_loop()
  end
  handle
      Toplevel =>
        (msgFlush(); BasicIO.exit 2)
    | Impossible msg =>
        (msgIBlock 0;
         errPrompt "Internal error: "; msgString msg;
         msgEOL(); msgEBlock(); msgFlush();
         BasicIO.exit 4)
);

val () = Printexc.f main ();
