structure Convert = 
struct
    fun manglefilename s = s

    (* Lexer of stream *)
    fun createLexerStream (is : BasicIO.instream) =
	Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

    fun parseFile lexbuf = Parser.Seq Scanner.Token lexbuf

    val quiet = ref false
    fun chat s = if !quiet then ()
		 else (print s; print "\n")


    local 
	open Parser Substring
	val hasStruc = List.exists (fn STRUCTURE => true | _ => false)  
	val hasSig   = List.exists (fn SIGNATURE => true | _ => false)  
	val sign     = all "signature"
	val stru     = all "structure"
	val ends     = all "\nend\n"
	val isComment = isPrefix "(*"
	val allWS    = foldl (fn (c,res) => res andalso Char.isSpace c) true
    in 
	fun roll (s :: sl) = if isComment s then s :: ends :: sl
			     else if allWS s then s :: roll sl
				  else ends :: s :: sl
	  | roll _         = [ends]
  
	fun ending rollback sl =
	    if not rollback then ends :: sl
	    else roll sl

	fun toSub rollback seq subs =
	    let fun getSub x y = slice(subs,x,SOME(y-x))
		fun loop (SIGNATURE :: ts) acc = loop ts (sign :: acc)
		  | loop (STRUCTURE :: ts) acc = loop ts (stru :: acc)
		  | loop (MLSTUFF(x,y) :: ts) acc = loop ts (getSub x y :: acc)
		  | loop (COMMENT(x,y) :: ts) acc = loop ts (getSub x y :: acc)
		  | loop _ acc = acc
	    in  rev(ending rollback (loop seq []))
	    end
	
	fun unitname filename = Path.base(Path.file filename)

	fun addStruc withsig seq filename =
	    let val dev = TextIO.openIn filename
		val s   = all (TextIO.inputAll dev)
	    in  if hasStruc seq then string s
		else let val cont = toSub false seq s
			 val unit = unitname filename
			 val sign = if withsig then [" :> ",unit," = \nstruct\n"]
				    else [" = \nstruct\n"]
			 val beg = all(String.concat("structure "::unit::sign))
		     in  concat(beg :: cont)
		     end
	    end 

	fun addSig rollback seq filename =
	    let val dev = TextIO.openIn filename
		val s   = all (TextIO.inputAll dev)
	    in  if hasSig seq then string s
		else let val cont = toSub rollback seq s
			 val unit = unitname filename
			 val beg  = all("signature "^unit^" = \nsig\n")
		     in  concat(beg :: cont)
		     end
	    end
	
    end

    fun read filename =
	let	val is     = BasicIO.open_in filename
	    val lexbuf = createLexerStream is
	    val seq    = parseFile lexbuf
	    val _      = BasicIO.close_in is
	in  seq
	end

    fun write outdir filename content =
	let val filen = Path.concat(outdir, Path.file filename)
	    val dev   = TextIO.openOut filen 
	in  TextIO.output(dev, content)
	    ; TextIO.closeOut dev
	end

    fun dostuff doit filename outdir =
	let val seq = read filename
	    val cont = doit seq filename
	in  write outdir filename cont
	end

    fun hasSig filename =
	let val b = Path.base filename
	    val s = Path.joinBaseExt{base = b, ext = SOME"sig"}
	in  FileSys.access(s,[])
	end

    fun processfile rollback outdir filename = 
	(case Path.ext filename of
	     SOME"sig" => dostuff (addSig rollback) filename outdir
	   | SOME"sml" => dostuff (addStruc (hasSig filename)) filename outdir
	   | SOME"mlp" => dostuff (addStruc (hasSig filename)) filename outdir
	   | _         => print("Don't know how to handle "^filename^"\n"))
	handle Fail msg => app print ["Error processing file: ", filename,
				      "\n", msg, "\n"]
	     | OS.SysErr (msg, err) =>
	          let val msg = case err of 
		                    SOME e => OS.errorMsg e
				  | NONE   => msg
		  in
		      app print ["OS Error processing file: ",
				 filename, "\n", msg, "\n"]
		  end

    fun getOpt option def ls =
	let fun get (opt :: (rest as arg :: xs)) =
		if opt = option then (arg, xs)
		else let val (res, ls) = get rest
		     in  (res, opt :: ls)
		     end
	      | get ls = (def, ls)
	in  get ls
	end

    fun main args =
	let fun eq x y = x = y
	    val (rb, args) = List.partition (eq "-rollback") args
	    val rollback = not (null rb)
	    val (q, args)  = List.partition (eq "-q") args
	    val _ = quiet := not (null q)
	    fun outdir ("-D" :: d :: ls) = (d, ls)
	      | outdir (s :: ls)         = let val (d,ls) = outdir ls
					   in  (d, s::ls)
					   end
	      | outdir []                = (".",[])
	    val (dir, files) = getOpt "-D" "." args
	in  app (processfile rollback dir) files
	end

    val _ = 
	case CommandLine.arguments() of
	    [] => print "usage: convert [-rollback] [-D outdir] files\n"
	  | args => main args

end
