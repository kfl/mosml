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
        prim_val set_nth_char_ : string -> int -> char -> unit
                                                 = 3 "set_nth_char"
      in
        if #cu_sig_stamp tables <> getOption (!(#uStamp sign)) then
           raise Fail ("load: compiled body of unit "^uname^
                       " is incompatible with its compiled signature")
        else ();
        seek_in is start;
        code := static_alloc (!block_len);
        fast_really_input is (!code) 0 code_len;
        (* `set_nth_char' must not check the length of buff, *)
        (* because `code' is allocated outside the heap! *)
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

(* ****************************************************** *)

(* Make.sml *)
(* 05Sep95 e *)

(* portions stolen from... *)

(* Mosmldep -- computing dependencies in a Moscow ML source directory. *)

(* Lexer of stream *)

fun createLexerStream (is : instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)
;

fun parsePhraseAndClear parsingFun lexingFun lexbuf =
  let val phr =
    parsingFun lexingFun lexbuf
    handle x => (Parsing.clearParser(); raise x)
  in
    Parsing.clearParser();
    phr
  end;

val parseFile =
  parsePhraseAndClear Deppars.MLtext Deplex.Token;

fun addExt s ext = s ^ "." ^ ext;

(* now the new stuff... *)

(* 1- use Mosmldep to find each source file's dependencies
   2- build some data structures (see below)
   3- make the transitive closure of the dependencies
   4- sort the files in dependency order
   5- process each file in turn
        checking modified times as documented below for function ensure
        and compiling out-of-date files

  data structures...
  after parsing: (objname,srcname,[objdeps],[moddeps]) called pd
  closedeps calls pdltoa to make...
  a hash table:   objname -> index                     called hn
  and an array:   index -> pd                          called ap
  and an array:   index -> [indexes of objdeps]        called di
  closedeps makes
      an array of indexes in dependency sorted order   called oi
   and returns the value (n,hn,ap,di,oi)
   where n is the length of the arrays
  ensure uses n,hn,ap,di,oi to compile files needing it

  pd
      objname is the name of the object file
        .sml files generate .uo entry
        .sig files generate .ui entry
      srcname is the name of the file found in the directory
      objdeps is a list of object files depended upon
        dependency on a unit inserts
          <unit>.ui into deps if <unit>.sig exists
          otherwise <unit>.uo is inserted
      moddeps is a list of units (not in this directory) depended upon

  read (the file parser) keeps a hash table of previously generated pd
   it is keyed by srcname;
   the modTime of the file is kept and checked to insure accuracy
   this hashtable can be manually cleared with: reset_readht();
*)

val moolevel = ref 1;

(* moolevel
0: no messages
1: error messages
2: compile messages
3: progress messages
*)

fun moo v s1 s2 = if !moolevel >= v then (print s1; print s2; print "\n") else ();
fun muu v s     = if !moolevel >= v then  print s                     else ();

fun pdltoa pdl =
  let val hn = Hasht.new 37 : (string, int) Hasht.t
      fun lp1 n r =
        if (null r) then n
        else let val (name,_,_,_) = (hd r)
             in Hasht.insert hn name n;
                lp1 (n+1) (tl r)
             end
  in 
    let val q  = lp1 0 pdl
        val ap = Array.array(q,("","",[""],[""]))
        val di = Array.array(q,[])
        fun lp2 n r =
          if (null r) then ()
          else let val (name,_,ns,_) = (hd r)
               in Array.update(ap,n,(hd r));
                  Array.update(di,n,(List.map (Hasht.find hn) ns));
                  lp2 (n+1) (tl r)
               end
    in
      lp2 0 pdl;
      (q,hn,ap,di)
    end
  end;

fun closedeps pdl =
  let val (n,hn,ap,di) = pdltoa pdl
      val dp = Array.array (n, []) (* dependents *)
      val qd = Array.array (n, 0 ) (* dependencies *)
      fun initdeps (deps,x) =
        let fun idep m r =
              if (null r) then m
              else let val h = hd r
                   in Array.update ( dp, h, x :: (Array.sub (dp, h)) );
                      idep (m + 1) (tl r)
                   end
        in Array.update (qd, x, idep 0 deps);
           x+1
        end
      val oi = Array.array (n, 0 )
      val qi = ref 0 (* queue in *)
      val ou = ref 0 (* queue out *)
      fun enque x = (Array.update ( oi, !qi, x ); qi := !qi + 1)
      fun pass1 i =
        if (i = n) then ()
        else let val x = Array.sub (qd, i)
             in if ( x = 0 ) then enque i else ();
                pass1 (i + 1 )
             end
      fun pass2 r =
        if (null r) then ()
        else let val h = hd r
                 val x = Array.sub (qd, h) - 1
             in Array.update ( qd, h, x );
                if ( x = 0 ) then enque h else ();
                pass2 (tl r)
             end
      fun deque x = (pass2 (Array.sub (dp, x)); ou := !ou + 1)
  in
    moo 3 "\n" "Computing Dependencies";
    Array.foldl initdeps 0 di;
    pass1 0;
    while ( !ou < !qi ) do deque (Array.sub (oi, !ou));
    if (!ou = n)
    then ()
    else let val (nm,_,_,_) = Array.sub (ap,!ou)
         in moo 1 "Circularity involving: "  nm;
            raise (Fail "circle"); () 
         end;
    (n,hn,ap,di,oi)
  end;

fun read' pdl srcext objext filename =
  let val is       = open_in (addExt filename srcext)
      val lexbuf   = createLexerStream is
      val mentions = Hasht.new 37 : (string, unit) Hasht.t
      val names    = parseFile lexbuf
      val objlist = ref []
      val modlist = ref []
      fun adddep s =
            if FileSys.access (addExt s "sig", []) then
              objlist := addExt s "ui" :: !objlist
            else if FileSys.access (addExt s "sml", []) then
              objlist := addExt s "uo" :: !objlist
        else (* libr or included dir files? *)
          modlist := s :: !modlist
  in 
    close_in is;
    List.app (fn name => Hasht.insert mentions name ()) names;
    if srcext = "sml" andalso FileSys.access(addExt filename "sig", [])
        then Hasht.insert mentions filename () else ();
    Hasht.apply (fn name => fn _ => adddep name) mentions;
    pdl := ((addExt filename objext),
            (addExt filename srcext),
            !objlist,
            !modlist) :: !pdl
  end
  handle Parsing.ParseError _ => output(std_out, "Parseerror!\n");

val readht = ref (Hasht.new 67
                  : (string, (Time.time *
                              (string * string * string list * string list)))
                  Hasht.t);

fun reset_readht _ =
       readht := (Hasht.new 67
                  : (string, (Time.time *
                              (string * string * string list * string list)))
                  Hasht.t);

fun read pdl srcext objext filename =
  let val sn = (addExt filename srcext)
      val mt = FileSys.modTime sn
      fun dit s = muu 3 s
      fun oops s =
         ( dit s;
           read' pdl srcext objext filename;
           Hasht.insert (!readht) sn (mt,(hd (!pdl))) )
  in
    let val (tm,pd) = Hasht.find (!readht) sn
    in
      case (Time.compare (tm,mt)) of
         EQUAL => ( dit "."; pdl := pd :: !pdl )
       | _     => oops ";"
    end
    handle _ => oops ":"
  end;

fun checkf srcext genext base =
  let val gennam = (addExt base genext)
      val havgen = (FileSys.access (gennam,[]))
  in
    if havgen then ()
    else moo 2 "  warning: " ((addExt base srcext) ^ " but no: " ^ gennam)
  end;

fun processfile pdl filename =
  let val {base, ext} = Path.splitBaseExt filename
      val base' = Path.file base
  in 
        case ext of
            SOME "sig" =>  read pdl "sig" "ui" base'
          | SOME "sml" =>  read pdl "sml" "uo" base'
          | SOME "grm" => (checkf "grm" "sml" base'; checkf "grm" "sig" base')
          | SOME "lex" =>  checkf "lex" "sml" base'
          | SOME "mlp" =>  checkf "mlp" "sml" base'
          | _          =>  ()
  end;

fun perv_set set' =
    let val set = Fnlib.stringToLower set'
    in 
      if set = "none" then 
        (preloadedUnits := []; preopenedPreloadedUnits := [])
      else
        (preloadedUnits :=          Fnlib.lookup set preloadedUnitSets;
         preopenedPreloadedUnits := Fnlib.lookup set preopenedPreloadedUnitSets
        )
        handle Subscript =>
          raise (Fail ("Unknown preloaded unit set " ^ set))
    end

fun protect_current_options fct =
  let val saved_path_library     = !path_library
      and saved_load_path        = !load_path
      and saved_preloadedUnits   = !preloadedUnits
      and saved_poPreloadedUnits = !preopenedPreloadedUnits
      and saved_watchDog         = !watchDog
  (*  and saved_verbose          = !Compiler.verbose
      and saved_quotation        = !Lexer.quotation     *)
      and saved_autolink         = !Link.autolink
      and saved_write_symbols    = !Link.write_symbols
      and saved_no_header        = !Link.no_header
  in
    (fct();
     path_library            := saved_path_library;
     load_path               := saved_load_path;
     preloadedUnits          := saved_preloadedUnits;
     preopenedPreloadedUnits := saved_poPreloadedUnits;
     watchDog                := saved_watchDog;
  (* Compiler.verbose        := saved_verbose;
     Lexer.quotation         := saved_quotation;        *)
     Link.autolink           := saved_autolink;
     Link.write_symbols      := saved_write_symbols;
     Link.no_header          := saved_no_header
     )
    handle x =>
      (path_library            := saved_path_library;
       load_path               := saved_load_path;
       preloadedUnits          := saved_preloadedUnits;
       preopenedPreloadedUnits := saved_poPreloadedUnits;
       watchDog                := saved_watchDog;
  (*   Compiler.verbose        := saved_verbose;
       Lexer.quotation         := saved_quotation;      *)
       Link.autolink           := saved_autolink;
       Link.write_symbols      := saved_write_symbols;
       Link.no_header          := saved_no_header;
       raise x)
  end

(* ensure -- that a file is compiled if need be
   1- if there is no object
   2- if the mtime of the object is older than the epoch
   3- if the mtime of the source is newer than mtime of the object
   4- if the mtime of any dependency is newer than the mtime of the object
   
   the build order of the files is passed in oi
   trick: we keep the mtime of each object in an array, timarr, indexed
          by position in the initial list; since only files earlier in
          the list can be depended upon, only their times are needed, so
          mtimes of files are thereby memoized
   dependencies on units outside the target directory are also checked
    and memoized in a local hashtable
*)

fun ensure epoch (n,hn,ap,di,oi) =
  let val timarr = Array.array(n,Time.zeroTime)
      fun ftime x = Array.sub(timarr,x)
      val itimes = Hasht.new 37 : (string, Time.time) Hasht.t
      fun itime' m = 
        let val uiname = (addExt m "ui")
            val prname = find_in_path uiname
        in moo 3 " checking: "  m;
           FileSys.modTime prname
        end handle Fail s => (moo 1 "  uncheck: " s; epoch)
      fun itime m = Hasht.find itimes m
                    handle Subscript =>
                      let val i = itime' m  (* memoize! *)
                      in Hasht.insert itimes m i; i end
      fun nxt z =
        if z >= n then ()
        else let val x = Array.sub(oi,z)
                 val (objname,srcname,objdeps,moddeps) = Array.sub(ap,x)
                 val deps = Array.sub (di,x)
             in
                if( FileSys.access (objname,[]) andalso
                    let val otime = FileSys.modTime objname in
                      Time.>(otime,epoch) andalso
                      Time.>(otime,(FileSys.modTime srcname)) andalso
                      (* this is conservative; too conservative if make is always used!
                      (List.all (fn d => Time.>(otime,ftime d)) deps) andalso
                      *)
                      (List.all (fn d => Time.>=(otime,ftime d)) deps) andalso
                      (List.all (fn d => Time.>(otime,itime d)) moddeps) andalso
                      ( Array.update(timarr,x,otime); true )
                    end )
                then moo 3 " ensuring: " objname
                else ( moo 2 "compiling: " objname;
                       evalCompile srcname;
                       Array.update(timarr,x,FileSys.modTime objname) );
                nxt (z+1)
             end
  in nxt 0;
     moo 3 "" ""
  end;

fun make oset stdlib includes path =
  let open FileSys
      val _   = if !moolevel < 0  (* kludgy way to reset table *)
                then (reset_readht(); moolevel := (~ (!moolevel)))
                else ()
      val pdl = ref []
      val dir = openDir path
      val _   =   chDir path
      fun read "" = ()
        | read f  = ( processfile pdl f ; read (readDir dir) )
      val _ = ( read (readDir dir); closeDir dir; () )
              handle exn as OS.SysErr (msg, _) => (moo 1 msg ""; raise exn)
      val nhnapdioi = closedeps (!pdl)
  in
    protect_current_options (fn () =>
      (path_library := stdlib;
       load_path := (if stdlib <> ""
                     then includes @ [stdlib]
                     else includes);
       perv_set (if oset <> "" then oset else "default");
       ensure Time.zeroTime nhnapdioi
      ))
  end;

(* new link interface 24Jul97 e *)

fun lynk exec_file (gopt,hopt) (auto,oset) stdlib includes files =
  protect_current_options (fn () => (protect_linker_tables (fn () =>
     (path_library := stdlib;
      load_path := (if stdlib <> "" then includes @ [stdlib] else includes);
      let val fi = ref files
      in
        if auto
        then ()
        else ( perv_set (case oset of "" => "default" | _ => oset );
               fi := (map (fn un => un ^".uo") (!preloadedUnits)) @ (!fi) );
        Link.autolink       := auto;
        Link.no_header      := hopt;
        Link.write_symbols  := gopt;
        reset_linker_tables();
        watchDog := (Hasht.new 17 : (string, SigStamp) Hasht.t);
        Link.link (!fi) exec_file;
        ()
      end))));

(* ****************************************************** *)

val smltop_con_basis =
[
  ("use",    { qualid={qual="Meta", id="use"},       info=VARname REGULARo}),
  ("load",   { qualid={qual="Meta", id="load"},      info=VARname REGULARo}),
  ("loadOne",{ qualid={qual="Meta", id="loadOne"},   info=VARname REGULARo}),
  ("compile",{ qualid={qual="Meta", id="compile"},   info=VARname REGULARo}),
  ("verbose",{ qualid={qual="Meta", id="verbose"},   info=VARname REGULARo}),
  ("quietdec",{ qualid={qual="Meta", id="quietdec"}, info=VARname REGULARo}),
  ("loadPath",{ qualid={qual="Meta", id="loadPath"}, info=VARname REGULARo}),
  ("quotation",
             { qualid={qual="Meta", id="quotation"}, info=VARname REGULARo}),
  ("valuepoly",
             { qualid={qual="Meta", id="valuepoly"}, info=VARname REGULARo}),
  ("exnName",
             { qualid={qual="Meta", id="exnName"},   info=VARname REGULARo}),
  ("exnMessage",
             { qualid={qual="Meta", id="exnMessage"},info=VARname REGULARo}),
  ("printVal", { qualid={qual="Meta", id="printVal"},info=VARname OVL1TXXo}),
  ("printDepth",
             { qualid={qual="Meta", id="printDepth"},info=VARname REGULARo}),
  ("printLength",
             { qualid={qual="Meta", id="printLength"}, info=VARname REGULARo}),
  ("chDir",  { qualid={qual="Meta", id="chDir"},     info=VARname REGULARo}), (* e *)
 ("moolevel",{ qualid={qual="Meta", id="moolevel"},  info=VARname REGULARo}), (* e *)
  ("make",   { qualid={qual="Meta", id="make"},      info=VARname REGULARo}), (* e *)
  ("link",   { qualid={qual="Meta", id="link"},      info=VARname REGULARo}), (* e *)
  ("system", { qualid={qual="Meta", id="system"},
               info=PRIMname (mkPrimInfo 1 (MLPccall(1, "sml_system"))) }),
  ("quit",   { qualid={qual="Meta", id="quit"},    info=VARname REGULARo}),
  ("installPP",
             { qualid={qual="Meta", id="installPP"}, info=VARname OVL1TPUo})
];

val smltop_VE =
[
   ("use",         trivial_scheme(type_arrow type_string type_unit)),
   ("load",        trivial_scheme(type_arrow type_string type_unit)),
   ("loadOne",     trivial_scheme(type_arrow type_string type_unit)),
   ("compile",     trivial_scheme(type_arrow type_string type_unit)),
   ("verbose",     trivial_scheme(type_ref type_bool)),
   ("quietdec",    trivial_scheme(type_ref type_bool)),
   ("loadPath",    trivial_scheme(type_ref (type_list type_string))),
   ("quotation",   trivial_scheme(type_ref type_bool)),
   ("valuepoly",   trivial_scheme(type_ref type_bool)),
   ("exnName",     trivial_scheme(type_arrow type_exn type_string)),
   ("exnMessage",  trivial_scheme(type_arrow type_exn type_string)),
   ("printDepth",  trivial_scheme(type_ref type_int)),
   ("printLength", trivial_scheme(type_ref type_int)),
   ("chDir",       trivial_scheme(type_arrow type_string type_unit)), (* e *)
   ("moolevel",    trivial_scheme(type_ref type_int)),                (* e *)
   ("make",        trivial_scheme(type_arrow type_string              (* e *)
                                   (type_arrow type_string
                                   (type_arrow (type_list type_string)
                                   (type_arrow type_string type_unit))))),
   ("link",        trivial_scheme(type_arrow type_string              (* e *)
                                   (type_arrow (type_pair type_bool type_bool)
                                   (type_arrow (type_pair type_bool type_string)
                                   (type_arrow type_string
                                   (type_arrow (type_list type_string)
                                   (type_arrow (type_list type_string) type_unit))))))),
   ("system",      trivial_scheme(type_arrow type_string type_int)),
   ("quit",        trivial_scheme(type_arrow type_unit type_unit))
];

val unit_smltop = newSig "Meta";

val () =
  app
    (fn (id, status) => Hasht.insert (#uConBasis unit_smltop) id status)
    smltop_con_basis
;

val () =
  app
    (fn (id, sc) => Hasht.insert (#uVarEnv unit_smltop) id sc)
    smltop_VE
;

val () = Hasht.insert pervSigTable "Meta" unit_smltop;

fun resetSMLTopDynEnv() =
  loadGlobalDynEnv "Meta" [
    ("use",         repr (evalUse: string -> unit)),
    ("loadOne",     repr evalLoad),
    ("load",        repr smartEvalLoad),
    ("compile",     repr evalCompile),
    ("verbose",     repr verbose),
    ("quietdec",    repr Exec_phr.quietdec),
    ("loadPath",    repr Mixture.load_path),
    ("quotation",   repr Lexer.quotation),
    ("valuepoly",   repr Mixture.value_polymorphism),
    ("printVal",    repr evalPrint),
    ("exnName",     repr Rtvals.getExnName),
    ("exnMessage",  repr Rtvals.getExnMessage),
    ("printDepth",  repr printDepth),
    ("printLength", repr printLength),
    ("chDir",       repr (fn n => FileSys.chDir n)), (* e *)
    ("moolevel",    repr moolevel),                  (* e *)
    ("make",        repr make),                      (* e *)
    ("link",        repr lynk),                      (* e *)
    ("quit",        repr (fn () => (msgFlush(); BasicIO.exit 0))),
    ("installPP",   repr evalInstallPP)
];

