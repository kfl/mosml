(* Htmlsigs: some hacks to turn Moscow ML annotated signature files into 
   HTML-files.  Peter Sestoft 1997-05-08, 1997-07-31, 2000-01-10
*)

fun indexbar out =
    out "<HR><TABLE WIDTH=100%>\
     \<TR ALIGN = CENTER>\n\
     \<TH><A HREF=\"idIndex.html\">Identifier index</A>\n\
     \<TH><A HREF=\"index.html\">Structure index</A>\n\
     \</TABLE><HR>\n";

val smlIdCharSym = Char.contains "'_!%&$#+-/:<=>?@\\~`^|*"
fun smlIdChar c = Char.isAlphaNum c orelse smlIdCharSym c


fun processSig version bgcolor sigfile htmlfile =
    let val strName = Path.base (Path.file sigfile)
	val is = TextIO.openIn sigfile
	val lines = Substring.fields (fn c => c = #"\n") 
	                             (Substring.all (TextIO.inputAll is))
	val _ = TextIO.closeIn is

	(* First pass over the file: record anchors of identifier defs *)

	val anchors = Polyhash.mkPolyTable (71, Fail "Htmlsigs.processSig")

	fun pass1 susline lineno = 
	    let open Substring
	    in
		if isPrefix "   [" susline then
		    let val (id, _) = splitl smlIdChar (triml 4 susline)
		    in Polyhash.insert anchors (string id, ()) end
		else ()
	    end

	(* Second pass over the file *)

	val os = TextIO.openOut htmlfile
	fun out s = TextIO.output(os, s)
	fun encode #"<" = "&lt;"
	  | encode #">" = "&gt;"
	  | encode #"&" = "&amp;"
	  | encode c    = str c
	fun outSubstr s = TextIO.output(os, Substring.translate encode s)
    
	val seenDefinition = ref false

	fun nameSubstr anchor target = 
	    (out "<A NAME=\""; outSubstr target; out "\"><B>"; 
	     outSubstr anchor; out "</B></A>")
	fun name anchor target = 
	    (out "<A NAME=\""; out target; out "\">"; out anchor; out "</A>")

	fun definition susline =
	    let open Substring
		val (id, rest) = splitl smlIdChar (triml 4 susline)
	    in
		if isEmpty id then (* no identifier defined here *)
		    outSubstr susline
		else
		    (out "   ["; nameSubstr id id; outSubstr rest)
	    end

	fun idhref id = 
	    (out "<A HREF=\"#"; outSubstr id; out "\">"; 
	     outSubstr id; out"</A>")

	fun declaration lineno space1 decl =
	    let open Substring 
		val (kind, rest)   = splitl Char.isAlpha decl
		val (space2, rest) = splitl Char.isSpace rest
		val (id, rest)     = splitl smlIdChar rest
	    in 
		outSubstr space1;
		outSubstr kind;
		outSubstr space2; 
		name "" ("line" ^ Int.toString lineno);
		if isEmpty id then () 
		else if Polyhash.peek anchors (string id) = NONE then 
		    outSubstr id
		else idhref id;
		outSubstr rest
	    end
	    
	fun pass2 susline lineno = 
	    let open Substring
	    in 
		(if isPrefix "   [" susline then 		    
		     (definition susline; 
		      seenDefinition := true)
		 else if not (!seenDefinition) then
		     (name "" ("line" ^ Int.toString lineno);
		      let val (space, suff) = splitl Char.isSpace susline
		      in
			  if isPrefix "val " suff 
			      orelse isPrefix "type " suff
			      orelse isPrefix "eqtype " suff
			      orelse isPrefix "datatype " suff 
			      orelse isPrefix "structure " suff 
			      orelse isPrefix "exception " suff then 
			      declaration lineno space suff
			  else 
			      outSubstr susline
		      end)
		 else
		     outSubstr susline);
		 out "\n"		   
	    end

	fun traverse process = 
	    let fun loop []        lineno = ()
		  | loop (ln::lnr) lineno = 
		    (process ln lineno; loop lnr (lineno+1))
	    in loop lines 1 end
    in
	print "Creating "; print htmlfile; print " from "; 
	print sigfile; print "\n"; 
	traverse pass1;
	out "<HTML><HEAD><TITLE>Structure ";
	out strName; out "</TITLE></HEAD>\n";
	out "<BODY BGCOLOR=\""; out bgcolor; out "\">\n";
	out "<H1>Structure "; out strName; out "</H1>\n";
	indexbar out;
	out "<PRE>\n";
	traverse pass2;
	out "</PRE>";
	indexbar out;
	out "<BR><EM>"; out version; out "</EM>";
	out "</BODY></HTML>\n";
	TextIO.closeOut os
    end

fun processSigfile version bgcolor stoplist sigdir htmldir sigfile =
    let val {base, ext} = Path.splitBaseExt sigfile
	val htmlfile = Path.joinBaseExt{base=base, ext=SOME "html"}
    in 
	case ext of
	    SOME "sig" => 
		if List.exists (fn name => base = name) stoplist then ()
		else processSig version bgcolor
		                (Path.concat(sigdir, sigfile))
		                (Path.concat(htmldir, htmlfile))
	  | _          => ()
    end

fun sigsToHtml version bgcolor stoplist (sigdir, htmldir) =
    let open FileSys
	fun mkdir htmldir =
	    if access(htmldir, [A_WRITE, A_EXEC]) andalso isDir htmldir then
		(print "Directory "; print htmldir; print " exists\n")
	    else
		(FileSys.mkDir htmldir; 
		 print "Created directory "; print htmldir; print "\n");
    in 
	mkdir htmldir;
	app (processSigfile version bgcolor stoplist sigdir htmldir) 
	    (Mosml.listDir sigdir)
    end
    handle exn as OS.SysErr (str, _) => (print(str ^ "\n\n"); raise exn)

fun printHTMLBase version bgcolor (sigfile, outfile) =
    let open Database
	val db = readbase sigfile
	val os = TextIO.openOut outfile
	fun out s = TextIO.output(os, s)
	fun href anchor target = 
	    app out ["<A HREF=\"", target, "\">", anchor, "</A>"]
	fun idhref file line anchor = 
	    href anchor (concat [file, ".html#line", Int.toString line])
	fun strhref file anchor = 
	    href anchor (file ^ ".html")
	fun mkalphaindex () =
	    let fun letterlink c = 
		if c > #"Z" then ()
		else (href (str c) ("#" ^ str c); out "&nbsp;&nbsp;"; 
		      letterlink (Char.succ c))
	    in 
		out "<HR>\n<CENTER><B>"; letterlink #"A"; 
		out "</B></CENTER><HR>\n" 
	    end
	fun subheader txt = app out ["\n<H2>", txt, "</H2>\n"]
	    
	(* Insert a subheader when meeting a new initial letter *)
	val lastc1 = ref #" "
	fun separator k1 = 
	    let val c1 = Char.toUpper k1
	    in 
		if Char.isAlpha c1 andalso c1 <> !lastc1 then 
		    (lastc1 := c1;
		     app out ["\n</UL>\n\n<A NAME=\"", str c1, "\">"];
		     subheader (str c1);
		     out "</A>\n<UL>")
		else ()
	    end
	fun mkref line file = idhref file line file 
	fun nextfile last []                                   = out ")\n"
	  | nextfile last ((e1 as {comp, file, line}) :: erest) =
	    if comp = last then
		(out ", "; mkref line file; nextfile last erest)
	    else 
		(out ")\n"; newitem e1 erest)
	and newitem (e1 as {comp, file, line}) erest =
	    let val key = Database.getname e1
	    in 
		separator (String.sub(key, 0));
		out "<LI><B>"; out key; out "</B> (";
		(case comp of
		     Str    => strhref key "structure"
		   | Val id => (out "value; ";       
				mkref line file)
		   | Typ id => (out "type; ";        
				mkref line file)
		   | Exc id => (out "exception; ";   
				mkref line file)
		   | Con id => (out "constructor; "; 
				mkref line file)
		   | Term (id, NONE) => mkref line file
		   | Term (id, SOME kind) => (out kind; out "; ";
					      mkref line file);
		nextfile comp erest)
	    end
	fun prentries []            = ()
	  | prentries (e1 :: erest) = newitem e1 erest
	fun prtree Empty = ()
	  | prtree (Node(key, entries, t1, t2)) = 
	    (prtree t1;
	     prentries entries;
	     prtree t2)
    in 
	out "<HTML>\
	 \<HEAD><TITLE>Moscow ML Library identifiers</TITLE></HEAD>\n";
	out "<BODY BGCOLOR=\""; out bgcolor; out "\">\n";
	out "<H1>Moscow ML Library identifiers</H1>\n";
	indexbar out;
	mkalphaindex();
	subheader "Symbolic identifiers";
	out "<UL>";
	prtree db; 
	out "</UL>\n";
	mkalphaindex();
	out "<BR><EM>"; out version; out "</EM>";
	out "</BODY></HTML>\n";
	TextIO.closeOut os 
    end
