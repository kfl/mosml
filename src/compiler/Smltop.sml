(* Smltop.sml *)

open List Obj BasicIO Nonstdio;
open Miscsys Memory Fnlib Config Mixture Const Smlexc Smlprim;
open Globals Location Units Types Smlperv Code_dec Emitcode Emit_phr Compiler;
open Symtable Patch;
open Rtvals Load_phr Exec_phr;

exception Already of string
and NotYet of string

fun add_suffix name suffix =
  if Filename.check_suffix name suffix
  then (Filename.chop_suffix name suffix, name)
  else (name, name ^ suffix)
;

(* Loading in core a compiled bytecode file *)

fun tryEvalLoad name =
  let
    val (simplename, filename) = add_suffix name ".uo"
    val uname = normalizedUnitName(Filename.basename simplename)
    val () =
      if member uname reservedUnitNames then
        raise Fail ("load: cannot load built-in unit "^uname)
      else ()
    val () =
      (ignore (Hasht.find (!watchDog) uname);
       raise Already uname)
      handle Subscript => ()
    val block_len = ref 0
    val code = ref ""
    val truename = find_in_path filename
    val is = open_in_bin truename
    val () =
      let
        val stop = input_binary_int is
        val start = pos_in is
        val code_len = stop - start
        val () = (block_len := code_len + 1)
        (* Now we have to check, whether the unit body is compatible *)
        (* with its compiled signature and previously loaded units. *)
        val () = seek_in is stop
        val tables = (input_value is : compiled_unit_tables)
        val () =
          Hasht.apply (fn uname' => fn stamp' =>
              let val stamp'' = Hasht.find (!watchDog) uname' in
                if stamp'' <> stamp' then
                  raise Fail ("load: compiled body of unit "^uname^
                     " is incompatible with previously loaded unit "^
                     uname')
                else ()
              end
              handle Subscript => raise NotYet uname')
            (#cu_mentions tables)
        (* The following line will put the compiled signature into the *)
        (* current table of unit signatures, if not already there:     *)
        val sign = (Hasht.find (!currentSigTable) uname
                   handle Subscript => readSig uname)
        prim_val create_string_ : int -> string = 1 "create_string";
        prim_val set_nth_char_  : string -> int -> char -> unit
                                                = 3 "set_nth_char"
      in
        if #cu_sig_stamp tables <> getOption (!(#uStamp sign)) then
           raise Fail ("load: compiled body of unit "^uname^
                       " is incompatible with its compiled signature")
        else ();
        seek_in is start;
        code := create_string_ (!block_len);
        fast_really_input is (!code) 0 code_len;
        set_nth_char_ (!code) code_len (Char.chr Opcodes.STOP);
        app
          (fn phr =>
            patch_object (!code) ((#cph_pos phr) - start) (#cph_reloc phr))
          (rev (#cu_phrase_index tables));
        exportPublicNames uname
          (#cu_exc_ren_list tables) (#cu_val_ren_list tables);
        Hasht.insert (!currentSigTable) uname sign;
        Hasht.insert (!watchDog) uname (#cu_sig_stamp tables);
        close_in is
      end
      handle x =>
        (close_in is; raise x)
    (* Initialize the unit.                                               *)
    (* In case this fails, remove it from the unit and signature tables:  *)
    val res = 
	(do_code false (!code) 0 (!block_len))
	handle x => (Hasht.remove (!currentSigTable) uname;
		     Hasht.remove (!watchDog) uname;
		     raise x)
  in () end;

fun evalLoad s =
  (catch_interrupt false; tryEvalLoad s; catch_interrupt true)
  handle
       SysErr(s, _) =>
         (catch_interrupt true; raise Fail ("load: "^s))
     | Already uname =>
         (catch_interrupt true;
	  raise Fail ("load: unit "^uname^" has been loaded already"))
     | NotYet uname =>
         (catch_interrupt true;
	  raise Fail ("load: unit "^uname^" is needed but not yet loaded"))
     | Out_of_memory =>
         (catch_interrupt true; raise Fail "load: out of memory")
     | Toplevel =>
         (catch_interrupt true;
          raise Fail "load: unable to load")
     | x => (catch_interrupt true; raise x)
;

(* A more user-friendly load function:
   * does not fail when a unit has already been loaded;
   * automatically loads any unit that a requested unit depends on.
*)

fun smartEvalLoad s =
    let fun tryload s pending =
	(catch_interrupt false; tryEvalLoad s; catch_interrupt true)
	handle
	SysErr(s, _) =>
	    (catch_interrupt true; raise Fail ("load: "^s))
      | Already _ =>
	    catch_interrupt true
      | NotYet missing =>
	    (catch_interrupt true;
	     if member missing pending then
		 raise Fail ("load: unit " ^ missing ^
			     " indirectly depends on itself")
	     else
		 (tryload missing (s :: pending);
		  tryload s pending))
      | Out_of_memory =>
	    (catch_interrupt true; raise Fail "load: out of memory")
      | Toplevel =>
	    (catch_interrupt true;
	     raise Fail "load: unable to load")
      | x => (catch_interrupt true; raise x)
    in tryload s [] end
;

fun evalLoaded () : string list =
    Hasht.fold (fn k => fn _ => fn res => k :: res) [] (!watchDog) 

fun protect_current_input fct =
  let val saved_input_name = !input_name
      and saved_input_stream = !input_stream
      and saved_input_lexbuf = !input_lexbuf
  in
    (fct();
     input_lexbuf := saved_input_lexbuf;
     input_stream := saved_input_stream;
     input_name := saved_input_name)
    handle x =>
      (input_lexbuf := saved_input_lexbuf;
       input_stream := saved_input_stream;
       input_name := saved_input_name;
       raise x)
  end
;

(* Loading an SML source file *)

fun loadToplevelPhrase lexbuf =
  let val (phrase, isLast) = parseToplevelPhrase lexbuf in
    execToplevelPhrase phrase;
    isLast
  end
;

fun evalUse filename =
  let
    val truename =
      (find_in_path filename
       handle Fail msg =>
         (msgIBlock 0; errPrompt msg; msgEOL(); msgEBlock(); msgFlush();
          raise Toplevel))
    val () = 
	if not (!Exec_phr.quietdec) then 
	    (msgIBlock 0;
	     msgString "[opening file \""; msgString truename;
	     msgString "\"]"; msgEOL(); msgEBlock(); msgFlush())
	else ()
    val is = open_in_bin truename
    val lexbuf = Compiler.createLexerStream is
    fun closeIn() =
      (close_in is;
       if not (!Exec_phr.quietdec) then 
	   (msgIBlock 0;
	    msgString "[closing file \""; msgString truename;
	    msgString "\"]"; msgEOL(); msgEBlock(); msgFlush())
       else ())

  in
    ( protect_current_input (fn () =>
        (input_name := truename;
         input_stream := is;
         input_lexbuf := lexbuf;
         while true do
           let val isLast = loadToplevelPhrase lexbuf
           in if isLast then raise EndOfFile else () end)))
    handle
        EndOfFile => closeIn()
      | x => (closeIn(); raise x)
  end
;

(* Compile a file *)

fun tryEvalCompile s =
  protect_current_input (fn () => protectCurrentUnit (fn () =>
    if Filename.check_suffix s ".sig" then
      let val filename = Filename.chop_suffix s ".sig" in
        compileSignature
          (normalizedUnitName (Filename.basename filename))
          filename
      end
    else if Filename.check_suffix s ".sml" then
      let val filename = Filename.chop_suffix s ".sml" in
        compileUnitBody
          (normalizedUnitName (Filename.basename filename))
          filename
      end
    else
      raise Fail "compile: unknown file name extension"))
;

fun evalCompile s =
  tryEvalCompile s
  handle
       Interrupt     => raise Fail "compile: interrupted by the user"
     | Out_of_memory => raise Fail "compile: out of memory"
     | Toplevel      => raise Fail "compile: error(s) in the source program"
     | SysErr _      => raise Fail "compile: file not found"
;

(* cvr: TODO
   it would be better if smltop_con_basis, sml_VE and the global dynamic
   env were initialised from a single association list instead of three
   possibly inconsistent ones 
*)

val smltop_con_basis =
[
  ("use",    { qualid={qual="Meta", id=["use"]},       info=VARname REGULARo}),
  ("load",   { qualid={qual="Meta", id=["load"]},      info=VARname REGULARo}),
  ("loadOne",{ qualid={qual="Meta", id=["loadOne"]},   info=VARname REGULARo}),
  ("loaded", { qualid={qual="Meta", id=["loaded"]},    info=VARname REGULARo}),
  ("compile",{ qualid={qual="Meta", id=["compile"]},   info=VARname REGULARo}),
  ("verbose",{ qualid={qual="Meta", id=["verbose"]},   info=VARname REGULARo}),
  ("quietdec",{ qualid={qual="Meta", id=["quietdec"]}, info=VARname REGULARo}),
  ("loadPath",{ qualid={qual="Meta", id=["loadPath"]}, info=VARname REGULARo}),
  ("quotation",
             { qualid={qual="Meta", id=["quotation"]}, info=VARname REGULARo}),
  ("valuepoly",
             { qualid={qual="Meta", id=["valuepoly"]}, info=VARname REGULARo}),
  ("exnName",
             { qualid={qual="Meta", id=["exnName"]},   info=VARname REGULARo}),
  ("exnMessage",
             { qualid={qual="Meta", id=["exnMessage"]},info=VARname REGULARo}),
  ("printVal", { qualid={qual="Meta", id=["printVal"]},info=VARname OVL1TXXo}),
  ("printDepth",
             { qualid={qual="Meta", id=["printDepth"]},info=VARname REGULARo}),
  ("printLength",
             { qualid={qual="Meta", id=["printLength"]}, info=VARname REGULARo}),
  ("system", { qualid={qual="Meta", id=["system"]},
               info=PRIMname (mkPrimInfo 1 (MLPccall(1, "sml_system"))) }),
  ("quit",   { qualid={qual="Meta", id=["quit"]},    info=VARname REGULARo}),
  ("installPP",
             { qualid={qual="Meta", id=["installPP"]}, info=VARname OVL1TPUo})
];

val smltop_VE =
[
   ("use",         trivial_scheme(type_arrow type_string type_unit)),
   ("load",        trivial_scheme(type_arrow type_string type_unit)),
   ("loadOne",     trivial_scheme(type_arrow type_string type_unit)),
   ("loaded",      trivial_scheme(type_arrow type_unit 
				             (type_list type_string))),
   ("compile",     trivial_scheme(type_arrow type_string type_unit)),
   ("verbose",     trivial_scheme(type_ref type_bool)),
   ("quietdec",    trivial_scheme(type_ref type_bool)),
   ("loadPath",    trivial_scheme(type_ref (type_list type_string))),
   ("quotation",   trivial_scheme(type_ref type_bool)),
   ("valuepoly",   trivial_scheme(type_ref type_bool)),
   ("exnName",     trivial_scheme(type_arrow type_exn type_string)),
   ("exnMessage",  trivial_scheme(type_arrow type_exn type_string)),
   ("printVal",    sc_bogus),  
   ("printDepth",  trivial_scheme(type_ref type_int)),
   ("printLength", trivial_scheme(type_ref type_int)),
   ("system",      trivial_scheme(type_arrow type_string type_int)),
   ("quit",        trivial_scheme(type_arrow type_unit type_unit)),
   ("installPP",    sc_bogus)  
];

val unit_smltop = newSig "Meta";

(*
val () =
  app
    (fn (id, status) => Hasht.insert (#uConBasis unit_smltop) id status)
    smltop_con_basis
;
*)
val () =
    app
    (fn (id, sc) => let val {qualid,info} = lookup id smltop_con_basis
                    in Hasht.insert (#uVarEnv unit_smltop) id 
                                    {qualid = qualid, info = (sc, info)}
		    end)
    smltop_VE
;

val () = Hasht.insert pervSigTable "Meta" unit_smltop;

fun resetSMLTopDynEnv() =
  loadGlobalDynEnv "Meta" [
    ("use",         repr (evalUse: string -> unit)),
    ("loadOne",     repr evalLoad),
    ("loaded",      repr evalLoaded),
    ("load",        repr smartEvalLoad),
    ("compile",     repr evalCompile),
    ("verbose",     repr verbose),
    ("quietdec",    repr Exec_phr.quietdec),
    ("loadPath",    repr Mixture.load_path),
    ("quotation",   repr Lexer.quotation),
    ("valuepoly",   repr Mixture.value_polymorphism),
    ("printVal",    repr evalPrint),
    ("exnName",     repr Smlexc.exnName),
    ("exnMessage",  repr Smlexc.exnMessage),
    ("printDepth",  repr printDepth),
    ("printLength", repr printLength),
    ("quit",        repr (fn () => (msgFlush(); BasicIO.exit 0))),
    ("installPP",   repr evalInstallPP)
];

