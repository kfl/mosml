structure PMCompile =
struct
    local open PMBasic in

    fun error msg = raise Fail msg
    val verbose = true
    fun chat msg = if verbose then
		      (app print msg; print "\n")
		   else ()

    fun insertSep sep []      = []
      | insertSep sep (x::xs) = x :: sep :: insertSep sep xs     

    fun newExt ex filename = 
	let open Path
	in  case splitBaseExt filename of
		{base, ext=SOME"sml"} => joinBaseExt{base=base,ext=SOME ex}
	      | _ => error "Can only handle .sml files for now"
	end
	    
    val smlToUi = newExt "ui"
    val smlToUo = newExt "uo"

    fun normalizeName filename = FileSys.fullPath filename
		  
    (* Functions fo manipulating context *)
    type context = bool * filename list list

    val initCont = (false, [[]])
    fun scope (dirty, cont)           = (dirty, [] :: cont)
    fun dropLocal (dirty, (x::_::xs)) = (dirty, x::xs)
    fun dropImports (dirty, files)    = (dirty, List.hd files)
    fun files (_,fs)                  = fs
    fun setDirty dirty (_, files)     = (dirty, files)
    fun addImport (d1, fs1) (d2, fs2) = (d1 orelse d2, fs1 :: fs2)

    fun uptodate (dirty,_) file =
	not dirty andalso (FileSys.access (smlToUo file,[]) andalso
			   let val t1 = FileSys.modTime file
			       val t2 = FileSys.modTime (smlToUo file)
			   in  Time.<(t1,t2)
			   end)

    fun addUI file (dirty, x::xs) = (dirty, (smlToUi file :: x) :: xs)


    (* context and files sould be given in reverse order *)
    fun mosmlc context file = 
	if uptodate context file then 
	    (chat ["Reusing: ", file];
	     (false, true))
	else
	    let open Process
		val cont = List.concat ([file] :: files context)
		val cont = rev(insertSep " " cont)
		val args = String.concat("mosmlc -c -toplevel ":: cont)
	    in  chat ["Compiling: ", file]
	      ; (true, system args = success)
	    end

    fun srcSeq (SRC(file,b)) acc = srcSeq b (file::acc)
      | srcSeq b             acc = (b, List.rev acc)  

    (* not ready for context with dirty bit *)
    (*fun compileBodyOpt path b context =
	case b of
	    SRC _ =>
	        let val (b,seq) = srcSeq b [] 
		    fun filename file = Path.mkAbsolute(file,path)
		    val files = List.map filename seq
		    val context' = foldl (fn(f,c) => addUI f c) context files
	        in  if mosmlc context (rev files) then 
			compileBodyOpt path b context'
		    else error (String.concat("Could not compile: "::files))
		end
	  | LOCAL(b1,b2,b3) =>
	        let val context = compileBodyOpt path b1 (scope context)
		    val context = compileBodyOpt path b2 (scope context)
		    val context = dropLocal context
		in  compileBodyOpt path b3 context
		end 
	  | NULL => context 
		*)

    fun compileBody path b context =
	case b of
	    SRC (file,b) =>
	        let val name = Path.mkAbsolute(file,path)
		    val (dirty, status) = mosmlc context name
		    val context = addUI name (setDirty dirty context)
	        in  if status then 
		         compileBody path b context
		    else error ("Could not compile: "^file)
		end
	  | LOCAL(b1,b2,b3) =>
	        let val context = compileBody path b1 (scope context)
		    val context = compileBody path b2 (scope context)
		    val context = dropLocal context
		in  compileBody path b3 context
		end 
	  | NULL => context 


    fun compile filename =
	let val table  = Polyhash.mkPolyTable(37, Subscript)
	    val peek   = Polyhash.peek table
	    val update = Polyhash.insert table
			 
			 
	    fun compileImports imps context = 
		let fun oneImport (imp,cont) =
			let val name = normalizeName imp 
			in case peek name of
			       SOME fs => addImport fs cont
			     | NONE    => 
			       let val res = compileFile name
			       in  update (name,res)
			         ; addImport res cont
			       end
			end
		in  List.foldl oneImport context imps
		end
		    
	    and compilePM prefix (PM{imports,body}) =
		let val context = scope(compileImports imports initCont)
		in  dropImports(compileBody prefix body context)
		end
		    
	    and compileFile filename = 
		let val name    = normalizeName filename
		    val path    = Path.dir name
		    val current = FileSys.getDir() before FileSys.chDir path 
		in compilePM path (parseFile name) before FileSys.chDir current
		   handle e => (FileSys.chDir current; raise e)
		end
	in  compileFile filename
	end

    
    fun findFilesBody path body accu =
	case body of
	    SRC (file, body) =>
	        let val name = Path.mkAbsolute(file,path)
		in  findFilesBody path body (name :: accu) 
		end
	  | LOCAL(b1,b2,b3) =>
	        let val accu = findFilesBody path b1 accu
		    val accu = findFilesBody path b2 accu
		in             findFilesBody path b3 accu
		end
	  | NULL => accu

    
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
		    val path    = Path.dir name
		    val current = FileSys.getDir() before FileSys.chDir path 
		in  findFilesPM path (parseFile name) accu 
		                                   before FileSys.chDir current
		    handle e => (FileSys.chDir current; raise e)
		end
	in  List.rev(ifindFiles filename [])
	end

    (* For now assume that everything is compiled and upto date *)
    fun link options filename =
	let val smlfiles = findFiles filename
	    val uofiles  = List.map smlToUo smlfiles
	    val args = String.concat("mosmlc -toplevel "::
				     options @ (insertSep " " uofiles))
	in  chat [args]
          ; Process.system args = Process.success
	end




    end
end
