(* The Caml Light linker. Command-line parsing. *)

local
  open List Fnlib Config Mixture Symtable Link;
in

val object_files = ref ([] : string list);
val exec_file = ref default_exec_name;

fun anonymous s =
  let val name =
    if Filename.check_suffix s ".sml" then
      Filename.chop_suffix s ".sml" ^ ".uo"
    else if Filename.check_suffix s ".uo" then
      s
    else
      raise Arg.Bad ("Don't know what to do with file "^s)
  in
    object_files := name :: !object_files
  end;

fun set_stdlib p =
  path_library := p;
;

fun add_include d =
  load_path := !load_path @ [d]
;

fun perv_set set' =
    let val set = Fnlib.stringToLower set'
    in 
	if set = "none" then 
	    preloadedUnits := []
	else
	    (preloadedUnits          := lookup set preloadedUnitSets)
	    handle Subscript =>
		raise Arg.Bad ("Unknown preloaded unit set " ^ set)
    end;

fun set_debug () =
  write_symbols := true
;

fun set_noheader () =
  no_header := true
;

fun set_standalone () =
  stand_alone := true
;

fun unset_autolink () =
  Link.autolink := false
;

fun set_verbose () =
  Link.verbose := true
;

fun set_exec_file e =
  exec_file := e
;

fun show_version() =
(
  msgIBlock 0;
  msgString ("Moscow ML linker version "^Config.version);
  msgEOL();
  msgString "Based in part on Caml Light";
  msgEOL();
  msgEBlock();
  msgFlush();
  BasicIO.exit 0
);

fun process_include filename =
  List.app anonymous (Readword.from_file filename)
;

fun main() =
(
  Miscsys.catch_interrupt true;
  perv_set "default";
  load_path := [];
  reset_linker_tables();
  Arg.parse [("-stdlib",     Arg.String set_stdlib),
             ("-I",          Arg.String add_include),
             ("-include",    Arg.String add_include),
             ("-P",          Arg.String perv_set),
             ("-perv",       Arg.String perv_set),
             ("-noautolink", Arg.Unit unset_autolink),
             ("-i",          Arg.Unit set_verbose),
             ("-g",          Arg.Unit set_debug),
             ("-debug",      Arg.Unit set_debug),
             ("-noheader",   Arg.Unit set_noheader),
             ("-standalone", Arg.Unit set_standalone),
             ("-o",          Arg.String set_exec_file),
             ("-exec",       Arg.String set_exec_file),
             ("-v",          Arg.Unit show_version),
             ("-version",    Arg.Unit show_version),
             ("-files",      Arg.String process_include),
             ("-",           Arg.String anonymous)
            ] anonymous;
  if !path_library <> "" then
    load_path := !load_path @ [!path_library]
  else ();
  if null (!object_files) then
    show_version()
  else ();
  object_files :=
    (map (fn uname => uname ^".uo") (!preloadedUnits))
    @ (rev (!object_files));
  link (!object_files) (!exec_file);
  msgFlush();
  BasicIO.exit 0

) handle
    Toplevel =>
      (msgFlush(); BasicIO.exit 2)
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
  | Fail msg =>
      (msgIBlock 0;
       errPrompt msg; msgEOL();
       msgEBlock();
       msgFlush();
       BasicIO.exit 2)
;

val () = Printexc.f main ();

end;
