open List Fnlib Config Mixture Location Units Smlperv Compiler;
open Types; (* cvr *)

(* Compile a file *)

fun compileFile (context,s,mode) =
  let (* val s = normalizedFileName s *) in
    if Filename.check_suffix s ".sig" then
      let val filename = Filename.chop_suffix s ".sig" in
        compileSignature context 
          (normalizedUnitName (Filename.basename filename))
	  mode
          filename
      end
    else if Filename.check_suffix s ".sml" then
      let val filename = Filename.chop_suffix s ".sml" in
        compileUnitBody context
          (normalizedUnitName (Filename.basename filename))
	  mode
          filename
      end
    else
      raise (Fail "unknown file name extension")
  end
;

val initialMode = ref (!currentMode);

val initialContext = ref ([] : string list);

val initialFiles = ref ([] : (string list * string * Mode) list);

fun anonymous s =
  let val s = normalizedFileName s in
    if Filename.check_suffix s ".sig" then
      let val filename = Filename.chop_suffix s ".sig" 
      in
	  (initialFiles := 
	         (!initialFiles) @ 
		 [(!initialContext,s,!initialMode)];
	   initialContext := (!initialContext) @ [filename])
      end
    else if Filename.check_suffix s ".sml" then
      let val filename = Filename.chop_suffix s ".sml" 
      in
	  (initialFiles := 
	       (!initialFiles) @ 
	       [(remove filename (!initialContext), 
		 (* we remove filename to avoid a circular dependency on
		    the .sig file, if any *)
		 s,
		 !initialMode)];
	   initialContext := (!initialContext) @ [filename])
      end
    else if Filename.check_suffix s ".ui" then
      let val filename = Filename.chop_suffix s ".ui" 
      in
	   initialContext := (!initialContext) @ [filename]
      end (* cvr: this implies that the .ui file must be on the load path *)
    else
      raise (Fail "unknown file name extension")
  end
;

fun set_stdlib p =
  path_library := p;
;

fun set_value_polymorphism b _ =
  value_polymorphism := b;
;

fun add_include d =
  load_path := (!load_path) @ [d]
;

fun perv_set set' =
    let val set = Fnlib.stringToLower set'
    in 
	if set = "none" then 
	    (preloadedUnits := []; preopenedPreloadedUnits := [])
	else
	    (preloadedUnits          := lookup set preloadedUnitSets;
	     preopenedPreloadedUnits := lookup set preopenedPreloadedUnitSets)
	    handle Subscript =>
		raise Arg.Bad ("Unknown preloaded unit set " ^ set)
    end

fun set_msgstyle p =
  if exists (fn x => x = p) ["default", "msdev"] then
    msgStyle := p
  else
    raise Arg.Bad ("Unknown message style " ^ p)
;

fun show_version() =
(
  msgIBlock 0;
  msgString ("Moscow ML compiler version "^Config.version);
  msgEOL();
  msgString "Based in part on Caml Light and the ML Kit";
  msgEOL();
  msgEBlock();
  msgFlush();
  BasicIO.exit 0
);

fun show_inferred_types() =
  verbose := true
;

fun enable_quotation() =
  Lexer.quotation := true
;

fun topdec_mode () =
  initialMode := TOPDECmode;

fun str_mode () =
  initialMode := STRmode;

fun orthodox () = currentCompliance := Orthodox;
fun conservative () = currentCompliance := Conservative;
fun liberal () = currentCompliance := Liberal;


fun main () =
(
  perv_set "default";
  load_path := [];
  toplevel := true;
  (* Choose the default (value polymorphism or imperative types) here: *)
  value_polymorphism := true;
  (* Choose the default SML compliance checks here *)
  currentCompliance := Liberal;
  Arg.parse [("-stdlib",    Arg.String set_stdlib),
             ("-I",         Arg.String add_include),
             ("-include",   Arg.String add_include),
             ("-P",         Arg.String perv_set),
             ("-perv",      Arg.String perv_set),
             ("-v",         Arg.Unit show_version),
             ("-version",   Arg.Unit show_version),
             ("-i",         Arg.Unit show_inferred_types),
             ("-quotation", Arg.Unit enable_quotation),
             ("-q",         Arg.Unit enable_quotation),
             ("-imptypes",  Arg.Unit (set_value_polymorphism false)),
             ("-valuepoly", Arg.Unit (set_value_polymorphism true)),
             ("-msgstyle",  Arg.String set_msgstyle), 
             ("-m",         Arg.String set_msgstyle),
             ("-structure",  Arg.Unit str_mode),
             ("-toplevel",  Arg.Unit topdec_mode),
             ("-orthodox",  Arg.Unit orthodox),
             ("-conservative",  Arg.Unit conservative),
             ("-liberal",  Arg.Unit liberal)
             ]
    anonymous;
  if !path_library <> "" then
    load_path := !load_path @ [!path_library]
  else ();
  initPervasiveEnvironments();
  resetTypePrinter(); (* cvr *)
  Miscsys.catch_interrupt true;
  if null (!initialFiles) then show_version() else ();
  app compileFile (!initialFiles);
  msgFlush()
)
handle
    Toplevel =>
      (msgFlush();
       BasicIO.exit 2)
  | Interrupt =>
      (msgIBlock 0;
       errPrompt "Interrupted."; msgEOL();
       msgEBlock();
       msgFlush();
       BasicIO.exit 3)
  | Impossible msg =>
      (msgIBlock 0;
       errPrompt "Internal error: "; msgString msg; msgEOL();
       msgEBlock();
       msgFlush();
       BasicIO.exit 4)
  | SysErr(msg, _) =>
      (msgIBlock 0;
       errPrompt "I/O operation failed: ";
       msgString msg; msgEOL();
       msgEBlock();
       msgFlush();
       BasicIO.exit 2)
  | Fail msg =>
      (msgIBlock 0;
       errPrompt "Compilation failed: "; msgEOL();
       errPrompt msg; msgEOL();
       msgEBlock();
       msgFlush();
       BasicIO.exit 2)
;

val () = Printexc.f main ();





