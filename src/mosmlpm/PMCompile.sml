structure PMCompile =
struct
    local open PMBasic in

    fun error msg = raise Fail msg
    val verbose = true
    fun say msg = if verbose then
		      (app print msg; print "\n")
		  else ()


    fun insertSep sep []      = []
      | insertSep sep (x::xs) = x :: sep :: insertSep sep xs     

    fun smlToUi filename = 
	let open Path
	in  case splitBaseExt filename of
		{base, ext=SOME"sml"} => joinBaseExt{base=base,ext=SOME"ui"}
	      | _ => error "Can only handle .sml files for now"
	end

    fun mosmlc context files = 
	let open Process
	    val cont = List.concat (files::context)
	    val cont = rev(insertSep " " cont)
	    val args = String.concat("mosmlc -c -toplevel ":: cont)
	in  say ["Compiling: "]
          ; app say files
	  ; system args = success
	end

    fun srcSeq (SRC(file,b)) acc = srcSeq b (file::acc)
      | srcSeq b             acc = (b, List.rev acc)  

    fun scope cont = [] :: cont
    fun dropLocal (x::_::xs) = x::xs


    fun addUI file (x::xs) = (smlToUi file :: x) :: xs

    fun compileBody path b context =
	case b of
	    SRC _ =>
	        let val (b,seq) = srcSeq b [] 
		    fun filename file = Path.mkAbsolute(file,path)
		    val files = List.map filename seq
		    val context' = foldl (fn(f,c) => addUI f c) context files
	        in  if mosmlc context (rev files) then 
			compileBody path b context'
		    else error (String.concat("Could not compile: "::files))
		end
	  | LOCAL(b1,b2,b3) =>
	        let val context = compileBody path b1 (scope context)
		    val context = compileBody path b2 (scope context)
		    val context = dropLocal context
		in  compileBody path b3 context
		end 
	  | NULL => context 

    local 	
	val table  = Polyhash.mkPolyTable(37, Domain)
	val peek   = Polyhash.peek table
	val update = Polyhash.insert table
    in
    
    fun import imps context = 
	let fun oneImport (imp,cont) =
		let val fullname = FileSys.fullPath imp 
		in case peek fullname of
		       SOME fs => fs::cont
		     | NONE    => 
		            let val res = compile fullname
			    in  update (fullname,res)
			      ; res::cont
			    end
		end
	in  List.foldl oneImport context imps
	end

    and compilePM prefix (PM{imports,body}) =
	let val context = scope(import imports [[]])
	in  hd(compileBody prefix body context)
	end

    and compile filename = 
	let val fullname = FileSys.fullPath filename
	    val path     = Path.dir fullname
	    val current  = FileSys.getDir() before FileSys.chDir path 
	in  compilePM path (parseFile fullname) before  FileSys.chDir current
	    handle e => (FileSys.chDir current; raise e)
	end

    end


    end
end
