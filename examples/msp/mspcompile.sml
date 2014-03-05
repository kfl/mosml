(* ML serverpages -- sestoft@dina.kvl.dk 2001-04-29 v 1.1   
                  -- 2014-03-05 Ken Friis Larsen minor changes, v 1.2

   Under Unix/Linux/MacOSX compile with 
      mosmlc -standalone -o mspcompile mspcompile.sml

   Under MS Windows compile with 
      mosmlc -o mspcompile.exe mspcompile.sml
   and note the lines marked MSWINDOWS below.
 *)

(* Directory for compiling and storing scripts; must agree with Makefile *)

val SCRIPTCACHE = "/var/cache/mspscripts"

(* The path to the Moscow ML compiler.  Add .exe under MSWINDOWS: *)

val mosmlc = "/usr/bin/mosmlc"

(* Transform [x1, x2, ..., xn] to [f x1, sep, f x2, sep, ..., sep, f xn] *)

fun delimit sep f res xs =
    case xs of 
       []       => []
     | x1 :: xr => f x1 :: List.foldr (fn (x, r) => sep :: f x :: r) res xr

(* The end of the HTTP response header *)

val httpheader : string option ref = ref NONE;

(* Process plain HTML-code, which surrounds the ML script fragments.
   Create an SML string constant with newlines and line
   continuations, and with quotes and backslashes properly escaped. *)

fun dohtml out sus = 
    let open Substring
        val lines = fields (fn c => c = #"\n") sus
	val linesres = delimit "\\n\\\n\\" (translate Char.toString)
	               ["\";\n"] lines
    in 
	(* Do not terminate HTTP response header until HTML code begins *)
        if not (isEmpty sus) then 
	   (Option.app out (!httpheader); httpheader := NONE;
	    List.app out ("val _ = print \"" :: linesres))
	else ()
    end

(* This function takes care of XML Processing Instructions, including MSP: *)

local 
    open Substring

    val xmlpihandlers : (string * (substring -> substring list)) list =
	[("MSP", 
	  fn ml => 
	  (case first ml of
	       SOME #"=" => [all "val _ = print(", triml 1 ml, all ");\n"]
	     | SOME #"$" => [all "val _ = Msp.printseq(", 
			     triml 1 ml, all ");\n"]
	     | _         => [ml]))
	 ]
in 			  
    fun doxmlpi outsus pifrag =
	let fun process [] = raise Fail "Unknown XML PI type"
	      | process ((target, processor) :: rest) = 
		if isPrefix target pifrag then 
		    List.app outsus 
			  (processor (slice(pifrag, String.size target, NONE)))
		else
		    process rest 
	in 
	    process xmlpihandlers
	end

    (* Find and process XML Processing Instructions; output HTML code: *)
			  
    fun findxmlpi dohtml doxmlpi (sus : substring) =
	let val (pre1, suf1) = position "<?" sus
	    val _ = dohtml pre1
	    val (pre2, suf2) = position "?>" suf1
	in 
	    if not (isEmpty suf2) then 
		(doxmlpi (triml 2 pre2); 
		 findxmlpi dohtml doxmlpi (triml 2 suf2))
	    else
		dohtml suf1 
	end
end;

(* Turn fbase.msp into fbase.sml, then compile and link it: *)

fun compile src bin =
    let val is     = TextIO.openIn src
	val mspsrc = Substring.all (TextIO.inputAll is)
	fun addext ext = Path.joinBaseExt{ base = bin, ext = SOME ext }
	val smlsrc = addext "sml"
	val os    = TextIO.openOut smlsrc
	fun out s = TextIO.output(os, s)
	fun outsus sus = TextIO.outputSubstr(os, sus)
        fun remove ext = (FileSys.remove (addext ext)) handle SysErr _ => ()
    in 
	httpheader := SOME (String.concat
	     ["val _ = print \"Content-type: text/html\\n\\n\\\n\
	      \\\<!-- Script ", bin, " compiled from ", src, " on ", 
	      Date.toString (Date.fromTimeLocal (Time.now())), " -->\\n\"\n"]);
	findxmlpi (dohtml out) (doxmlpi outsus) mspsrc;
	TextIO.closeOut os;
	TextIO.closeIn is;
	(Mosml.run mosmlc ["-q", "-o", bin, "-I", Path.dir src, smlsrc] "")
	before (remove "ui"; remove "uo"; remove "sml")
    end

(* Log to httpd.error_log *)

fun log s = 
    let val client = Option.getOpt(Process.getEnv "REMOTE_ADDR", "N/A")
    in TextIO.output(TextIO.stdErr, "MSP [" ^ client ^ "]: " ^ s ^ "\n") end

(* Report an error back to the invoking browser *)

fun err msg =
    (print "Content-type: text/plain\n\n"; 
     print (msg ^ "\n");
     log msg)

(* Compile the .msp file if it does not exist or is out of date: *)

fun update path : string option = 
    let val src = Path.joinBaseExt{base = path, ext = SOME "msp" }
	fun hexify c = if Char.isAlphaNum c then str c
		       else Int.fmt StringCvt.HEX (ord c)
	val bin = String.translate hexify (Mosml.md5sum path)
	(* MSWINDOWS add: 
           val bin = Path.joinBaseExt {base = bin, ext = SOME "exe"} *)
    in 
	if not (FileSys.access(bin, []))
	    orelse Time.>(FileSys.modTime src, FileSys.modTime bin) then
	    case compile src bin of
		Mosml.Success _ => SOME bin
	      | Mosml.Failure s => 
		    (err ("Compilation of " ^ src ^ " failed:\n" ^ s); NONE)
	else
	    SOME bin
    end;

(* Invoke a compiled script *)

fun invoke path bin = 
    let fun intOf NONE     = NONE
	  | intOf (SOME s) = Int.fromString s
	val len = Option.getOpt(intOf (Process.getEnv("CONTENT_LENGTH")), 0)
	val input = TextIO.inputN(TextIO.stdIn, len)
    in 
	FileSys.chDir (Path.dir path);
	case Mosml.run (Path.concat(SCRIPTCACHE, bin)) [] input of
	    Mosml.Failure s => err ("Invocation of " ^ path ^ " failed:\n" ^ s)
	  | Mosml.Success s => print s
    end

(* Main program *)

val _ = 
    (FileSys.chDir SCRIPTCACHE;
     case Process.getEnv("PATH_TRANSLATED") of
	 NONE      => err "Invocation failed.  Contact server administrator"
       | SOME path => 
	     let val base = Path.base path
	     in 
		 case update base of
		     NONE     => ()
		   | SOME bin => invoke path bin
	     end)
    handle Fail s => err ("Script failed: " ^ s)
	 | Io {function, name, ...} => 
	   err ("Io error: " ^ function ^ " failed on " ^ name)
	 | SysErr _ => err "SysErr"
