structure PMCompile =
struct
    local open PMBasic in

    fun error msg = raise Fail msg

    val quiet = ref false
    fun chat msg = if !quiet then ()
		   else (app print msg; print "\n")

    val debugFlag = ref false
    fun debug msg = if !debugFlag then (app print msg; print "\n")
		    else ()


    fun insertSep sep []      = []
      | insertSep sep (x::xs) = x :: sep :: insertSep sep xs     

    local structure P = OS.Path
    in
    fun newExt ext filename = 
	let val base = P.base filename
	in  P.joinBaseExt{base = base, ext = SOME ext}
	end
    end

    val toUi  = newExt "ui"
    val toUo  = newExt "uo"
    val toSig = newExt "sig"
	    
    fun checkExt ext filename = OS.Path.ext filename = SOME ext
    val isSML = checkExt "sml"
    val isSIG = checkExt "sig"
    fun isML file = isSML file orelse isSIG file

    fun unwindChDir toDir body =
	let val current = OS.FileSys.getDir() 
	                  before OS.FileSys.chDir toDir 
	in  body() before OS.FileSys.chDir current
	    handle e => (OS.FileSys.chDir current; raise e)
	end

    fun normalizeName filename = OS.FileSys.fullPath filename
		  
    (* Functions fo manipulating context *)
    type context = bool * filename list list

    val initCont = (false, [[]])
    fun scope (dirty, cont)           = (dirty, [] :: cont)
    fun dropLocal (dirty, (x::_::xs)) = (dirty, x::xs)
    fun dropImports (dirty, files)    = (dirty, List.hd files)
    fun files (_,fs)                  = fs
    fun dirty (d,_)                   = d
    fun setDirty dirty (_, files)     = (dirty, files)
    fun addImport (d1, fs1) (d2, fs2) = (d1 orelse d2, fs1 :: fs2)
    fun addUI file (dirty, x::xs)     = (dirty, (toUi file :: x) :: xs)


    (* returns true if file exists *)					
    fun exists file = OS.FileSys.access (file,[])

    fun isNewer file1 file2 =
	not (exists file2) orelse
	let val t1 = OS.FileSys.modTime file1
	    val t2 = OS.FileSys.modTime file2
	in  Time.> (t1, t2)
	end

    fun isNewer2 file (f1, f2) = isNewer file f1 orelse isNewer file f2

    fun isNewerList file []        = false
      | isNewerList file (f :: fs) = isNewer file f orelse isNewerList file fs 



    (* recompile returns a dirty flag for the following context and a
       list of files to recompile.  
    *)
    fun recompile pmfile context smlfile =
	let val sigfile  = toSig smlfile
	    val binfiles as (ui, uo) = (toUi smlfile, toUo smlfile)
	    val sigExist = exists sigfile
	    val allfiles = if sigExist
			   then [sigfile, smlfile]
			   else [smlfile]
	in 
	    if dirty context 
	       orelse (if sigExist 
		       then isNewer sigfile ui
		       else isNewer2 smlfile binfiles)
	       orelse exists ui andalso isNewer pmfile ui
	       orelse exists uo andalso isNewer pmfile uo
	    then (true,  allfiles)
	    else (false, if sigExist andalso isNewer smlfile uo 	       
			 then [smlfile]
			 else [])
	end


    local structure P = OS.Process
    in
    fun mosmlc debug options files file =
	let val cont  = options @ (files @ [file])
	    val args  = insertSep " " cont
	    val sargs = String.concat ("mosmlc -c "::args)
	in  
	    debug [sargs] 
	  ; P.system sargs = P.success
	end
    end



(*
    fun makeTempName file =
	let val {base,ext} = OS.Path.splitBaseExt file
	    val base = String.concat[base,"-",Option.valOf ext]
	in  OS.Path.joinBaseExt{base = base, ext = SOME "tmp"}
	end

    (* move file.ui to file.ui.tmp *)
    fun move_ui_file file =
      let val ui  = toUi file
	  val new = makeTempName ui
      in  if exists ui then
	      OS.FileSys.rename {old=ui,new=new}
	  else ()
      end

    fun filesEqual file1 file2 =
	let fun loop dev1 dev2 =
		(BinIO.endOfStream dev1 andalso BinIO.endOfStream dev2)
		orelse
		(BinIO.inputN(dev1,128) = BinIO.inputN(dev2,128)
		 andalso loop dev1 dev2)
	    val dev1 = BinIO.openIn file1
	    val dev2 = (BinIO.openIn file2) handle e => ( BinIO.closeIn dev1
                                                        ; raise e)
	in  (loop dev1 dev2) handle e => ( BinIO.closeIn dev1
					 ; BinIO.closeIn dev2
                                         ; false)
	end handle _ => false

    fun check_ui_file file : bool (*true if dirty *) =
      let val ui = toUi file
	  val ui' = makeTempName ui
      in
	  if filesEqual ui ui' then
	      (OS.FileSys.rename {old=ui',new=ui}; false)
	  else (OS.FileSys.remove ui';             true)
      end handle _ => true
*)

(*    (* context is in reverse order *)
    fun mosmlc toplevel context file = 
	let val up2date = if toplevel then uptodate else strUptodate
	in
	    if up2date context file then 
		( chat ["Reusing: ", file]
		; (false, true)
                )
	    else
		let structure P = OS.Process
		    (*val _ = move_ui_file file
		     *)
		    val cont = List.concat ([file] :: files context)
		    val cont = rev(insertSep " " cont)
		    val mode = if toplevel then " -toplevel "
			                   else " -structure "
		    val args = String.concat("mosmlc -c":: mode 
					     :: cont)
		    val return = ( chat ["Compiling: ", file]
                                 ; debug [args] 
				 ; P.system args = P.success
                                 )
		in (*(check_ui_file file, return)
		    *) (true, return)
		end
	end
*)

    fun check f = List.foldl (fn (arg, res) => res andalso f arg) true

    fun compileBody path pmfile body context =
	let fun compileFile toplevel file next =
                if isSML file
		then
		    let val name = OS.Path.mkAbsolute(file,path)
			val options = if toplevel then ["-toplevel"]
				      else ["-structure"]
			fun compile file =
			    ( chat ["Compiling: ", file] 
			    ; mosmlc debug options (rev(List.concat(files context))) file
			    )
			val (dirty, files) = recompile pmfile context name
			val status = check compile files 
			val context = addUI name (setDirty dirty context)
		    in  if status then 
			    compileBody path pmfile next context
			else error ("Could not compile: "^name)
		    end
		else error ("Cannot handle "^file^" in "^pmfile^
			    "\nCan only handle .sml files.\n")
	in
	    case body of
		SRC (file,b)    => compileFile true file b
	      | STRSRC (file,b) => compileFile false file b
	      | LOCAL(b1,b2,b3) =>
		    let val context = compileBody path pmfile b1 (scope context)
			val context = compileBody path pmfile b2 (scope context)
			val context = dropLocal context
		    in  compileBody path pmfile b3 context
		    end 
	      | NULL => context 
	end

    fun compile filename =
	let val table  = Polyhash.mkPolyTable(37, Subscript)
	    val peek   = Polyhash.peek table
	    val update = Polyhash.insert table

	    fun isIn name = List.exists (fn n => n = name)
	    fun indent files =
		String.concat(foldl (fn(n,r) => "   "::n::"\n"::r) [] files)

	    fun compileImports openFiles imps context = 
		let fun oneImport (imp,cont) =
			let val name = normalizeName imp 
			in case peek name of
			       SOME fs => addImport fs cont
			     | NONE    => 
			       let val res = compileFile openFiles name
			       in  update (name,res)
			         ; addImport res cont
			       end
			end
		in  List.foldl oneImport context imps
		end
		    
	    and compilePM openFiles prefix pmfile (PM{imports,body}) =
		let val context = scope(compileImports openFiles imports 
					                         initCont)
		in  dropImports(compileBody prefix pmfile body context)
		end
		    
	    and compileFile openFiles filename = 
		let val name    = normalizeName filename
		    val path    = OS.Path.dir name
		in  if isIn name openFiles then
		       error ("Cycle dectected:\n"^
			      indent (name :: openFiles))
		    else unwindChDir path (fn() =>
		          compilePM (name :: openFiles) path name (parseFile name))
			
		end
	in  compileFile [] filename
	end

    
    fun findFilesBody path body accu =
	let fun findFilesFile file body = 
		let val name = OS.Path.mkAbsolute(file,path)
		in  findFilesBody path body (name :: accu) 
		end
	in 
	    case body of
		SRC (file, body)    => findFilesFile file body
	      | STRSRC (file, body) => findFilesFile file body
	      | LOCAL(b1,b2,b3) =>
		    let val accu = findFilesBody path b1 accu
			val accu = findFilesBody path b2 accu
		    in             findFilesBody path b3 accu
		    end
	      | NULL => accu
	end

    
    fun findFiles filename = 
	let val table  = Polyhash.mkPolyTable(37, Subscript)
	    fun isIncluded file = Option.isSome(Polyhash.peek table file)
	    fun mark file = Polyhash.insert table (file,())
    
	    fun findFilesImports imports accu =
		let fun oneImport (file, accu) =
			let val name = normalizeName file
			in  if isIncluded name then accu
			    else ifindFiles name accu before mark name
			end
		in  List.foldl oneImport accu imports
		end
		    
	    and findFilesPM path (PM{imports,body}) =
		(findFilesBody path body) o (findFilesImports imports)
		
	    and ifindFiles filename accu =
		let val name    = normalizeName filename
		    val path    = OS.Path.dir name
		in  unwindChDir path (fn() =>
	       	    findFilesPM path (parseFile name) accu)
		end
	in  List.rev(ifindFiles filename [])
	end

    (* For now assume that everything is compiled and upto date *)
    fun link options filename outfile =
	let val smlfiles = findFiles filename
	    fun makeUo file = if isSML file then SOME(toUo file)
			      else NONE
	    val uofiles  = List.mapPartial makeUo smlfiles
	    val args = 
		String.concat("mosmlc -toplevel -o ":: outfile :: " " ::
				     options @ (insertSep " " uofiles))
	in  chat ["Linking: ", outfile]
          ; debug [args]
          ; Process.system args = Process.success
	end

    end
end
