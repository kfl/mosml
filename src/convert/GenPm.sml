(* GenPm -- computes .pm for a (list of) old style Moscow ML source file(s). 
   Handles strings and nested comments correctly; 

   Based on Peter Sestoft's mosmldep tool, first modified for Holmake,
   then for moscm, and finally(?) for this purpose.

   Last modified: $Date: 2000-07-19 15:54:48 $ by $Author: kla $

   DOES NOT normalizes file names under DOS. (yet)

   Usage: gen_pm file1 file2 ...
*)

structure GenPm =
struct

   fun manglefilename s = s

   fun eq x y = x=y
		 
   fun ext filename  = #ext(Path.splitBaseExt filename)
   fun base filename = #base(Path.splitBaseExt filename)
   fun unitname filename = base(#file(Path.splitDirFile filename))

   (* returns true if file exists *)					
   fun exists file = OS.FileSys.access (file,[])


   (* Lexer of stream *)
   fun createLexerStream (is : BasicIO.instream) =
       Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

   fun parsePhraseAndClear parsingFun lexingFun lexbuf =
       let val phr =
	       parsingFun lexingFun lexbuf
	       handle x => (Parsing.clearParser(); raise x)
       in
	   Parsing.clearParser();
	   phr
       end

   val parseFile =  parsePhraseAndClear Deppars.MLtext Deplex.Token
		    
   fun errMsg str = BasicIO.output(BasicIO.std_err, str ^ "\n\n")
   fun fail()     = Process.exit Process.failure

   fun impossible fct = 
       ( TextIO.output(TextIO.stdErr, "IMPOSSIBLE: "^fct^"\n\n")
       ; fail())

   fun warning msg = TextIO.output(TextIO.stdErr, "Warning: "^msg^"\n\n")
   fun warn file msg = warning ("Errors processing file: "^file^"\n"^msg)
		    
   fun addExt s ext = Path.joinBaseExt{base = s, ext = SOME ext}
		      
   fun addDir dir s = Path.joinDirFile{dir=dir,file=s}
		      
   val srev = fn s => String.implode(List.rev(String.explode s))
				    
   local 
       val path    = ref [""]
       val objDir  = ref ""

       fun lpcl []                = (errMsg "No filenames"; fail())
	 | lpcl ("-I"::dir::tail) = (path := dir :: !path ; lpcl tail)
	 | lpcl ("-D"::dir::tail) = (objDir := dir        ; lpcl tail)
	 | lpcl l                 = l
   in

   (* no options yet *)
   fun parseComLine l = l 
       (*let val s = lpcl l
       in
	   path := List.rev (!path);
	   s
       end *)

   fun chat s = print(s^"\n")
	   
   fun access s ext =
       let val sext = addExt s ext
	   fun inDir dir = FileSys.access (addDir dir sext, [])
       in case List.find inDir (!path) of
	      SOME dir => SOME(addDir (Path.concat(dir, !objDir)) s)
	    | NONE     => NONE 
       end
	   
   fun mktarget s = addDir (!objDir) s
   end
	   	   
   fun read mentions file =
       let val is       = BasicIO.open_in file 
			  before chat("Analyzing file: "^file)
	   val lexbuf   = createLexerStream is
	   fun insert s = Polyhash.insert mentions (s,())
	   val names    = parseFile lexbuf
       in  BasicIO.close_in is
         ; List.app insert names
       end handle Parsing.ParseError _ => warn file "Parseerror!"
		| OS.SysErr (msg, _)   => warn file msg

   fun outimports pr imports =
       let fun outname (name, col) = if col + size name >= 77 then 
					 ( pr("\n   "^ name ^ " ")
					 ; 4 + size name)
				     else ( pr(name ^ " ")
					  ; col + size name + 1)
	   val imp = List.map (fn n => addExt n "pm") imports
       in  pr "import "
         ; List.foldl outname 7 imp
         ; pr "\nin\n"
       end

   fun processgroup grp =
       let val mentions = Polyhash.mkPolyTable (37, Subscript)
	   val rd       = read mentions
	   val _        = List.app rd grp
	   val imports  = map #1 (Polyhash.listItems mentions)
	   val unit     = unitname(List.hd grp)
	   val files    = op@(List.partition (eq "sig" o valOf o ext) grp)
	   val pmfile   = addExt unit "pm"
	   val dev      = TextIO.openOut pmfile
			  before chat("Writing file: "^pmfile)
	   fun pr s     = TextIO.output(dev, s)
	   fun prline s = pr(s^"\n")
       in  outimports pr imports
         ; List.app prline files
         ; prline "end"
         ; TextIO.closeOut dev
       end

   fun processFile file =
       let val mentions = Polyhash.mkPolyTable (37, Subscript)
	   val _        = read mentions file
	   val imports  = map #1 (Polyhash.listItems mentions)
	   fun locexists n = exists(addExt n "sml")
	   val localimp = List.filter locexists imports (* only those in the current dir *)
	   val unit     = unitname file
	   val pmfile   = addExt unit "pm"
	   val dev      = TextIO.openOut pmfile
			  before chat("Writing file: "^pmfile)
	   fun pr s     = TextIO.output(dev, s)
	   fun prline s = pr(s^"\n")
       in  outimports pr localimp
         ; prline ("structure "^file)
         ; prline "end"
         ; TextIO.closeOut dev
       end	
    
   fun mosfile filename = ext filename = SOME "sml"
(*
	case ext filename of
	    SOME s => List.exists (eq s) ["sml","sig"]
	  | NONE   => false 
*)
		      
    fun group filenames =
	let fun inGrp file = (eq (unitname file)) o unitname
	    fun addGrp (f :: fs) acc = 
		let val (grp, rest) = List.partition (inGrp f) fs
		in  addGrp rest ((f::grp):: acc)
		end
	      | addGrp _ acc = acc
	in  addGrp filenames []
	end
	    
    fun main args =
	let val filenames = parseComLine args
	    val (mosfiles, other) = List.partition mosfile filenames
	    val groups = group mosfiles
	in  List.app processFile mosfiles
	end 


       val _ = main(CommandLine.arguments())
end

