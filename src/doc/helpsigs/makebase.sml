(* makebase -- create the signature database from the Moscow ML 
 * library signatures.  PS 1995-11-19, 2000-06-29
 *)

(* The version number inserted in generated files: *)
val version = "<A HREF=\"http://www.dina.kvl.dk/~sestoft/mosml.html\">\
    \Moscow ML</A> 2.00";

(* Default directory containing the signature files: *)
val libdirDef = "../../../lib/"

(* Default filename for the resulting help database: *)
val helpfileDef = "helpsigs.val"

(* Default filename for the ASCII format database: *)
val txtIndexDef = "index.txt"

(* Default filename for the LaTeX format database: *)
val texIndexDef = "../index.tex"

(* Default directory for signatures in HTML format: *)
val htmlDirDef = "htmlsigs"

(* Default filename for the HTML format database: *)
val htmlIndexDef = htmlDirDef ^ "/idIndex.html"

(* Default filename for the LaTeX signatures: *)
val texSigs = "../texsigsigs.tex"

(* Signatures not to be included in the help database: *)
val stoplist = ["Misc", "Strbase", "Splaytree"];

(* The background colour in generated HTML files (HTML colour code): *)
val bgcolor = "#fbf2e7";

(* To make the database, sort entries on normalized (lower case) name,
 * lump together equal normalized names in entry lists, and sort these by
 * structure name (except that a Str entry always precedes the others): 
 *)

fun mkbase (entries : Database.entry list) =
    let open Database
	fun compname (e1, e2) = keycompare (getname e1, getname e2)
	val entries = Listsort.sort compname entries
	infix THEN
	fun (comp1 THEN comp2) arg =
	    case comp1 arg of
		EQUAL => comp2 arg
	      | res   => res
	fun kindof Str     = "structure"
	  | kindof (Val _) = "val"
	  | kindof (Typ _) = "type"
	  | kindof (Exc _) = "exn"
	  | kindof (Con _) = "con"
	  | kindof (Term (_, NONE)) = ""
	  | kindof (Term (_, SOME kind)) = kind
	fun kindCompare (e1 as {comp=c1, ...}, e2 as {comp=c2, ...}) = 
	    String.compare(kindof c1, kindof c2)
	fun nameCompare (e1, e2) = 
	    String.compare(getname e1, getname e2)
	fun fileCompare (e1, e2) = String.compare(#file e1, #file e2)
	val entryCompare = kindCompare THEN nameCompare THEN fileCompare
	(* Share strings if result=argument: *)
	fun toLower s = 
	    let val s' = String.map Char.toLower s
	    in if s=s' then s else s' end
	(* Lump together names that differ only in capitalization;
           then sort each lump using entryCompare                   *)
	fun lump [] = []
	  | lump (x1 :: xr) = 
	    let fun mkLump lumpname lump = (toLower (getname lumpname), 
					    Listsort.sort entryCompare lump)
		fun h lump1name lump1 []       = [mkLump lump1name lump1]
		  | h lump1name lump1 (x1::xr) = 
		    if compname(x1, lump1name) = EQUAL then 
			h lump1name (x1 :: lump1) xr
		    else
			mkLump lump1name lump1 :: h x1 [x1] xr
	    in h x1 [x1] xr end
	val lumps = lump entries : (string * entry list) list
	fun mkOrderedTree xs = 
	    let fun h 0 xs = (Empty, xs)
		  | h n xs =
		    let val m = n div 2
			val (t1, (key, value) :: yr) = h m xs
			val (t2, zs)                 = h (n-m-1) yr
		    in (Node(key, value, t1, t2), zs) end
	    in #1 (h (length xs) xs) end
    in
	mkOrderedTree lumps
    end

fun dirToBase (dir, filename) =
    let val res = List.foldl (Parsspec.processfile stoplist dir) 
	                     [] (Mosml.listDir dir)
	val _ = print ("\nProcessed " ^ Int.toString (length res) 
		       ^ " entries in total.\n");
	val _ = print ("Building database...\n");
	val db = mkbase res
	val _ = print ("Writing database to file " ^ filename ^ "\n");
    in 
	Database.writebase(filename, db)
    end
    handle exn as OS.SysErr (str, _) => (print(str ^ "\n\n"); raise exn)

fun process (libdir, helpfile, txtIndex, texIndex, htmldir, htmlIndex) =
    (print ("Reading signatures in directory " ^ libdir ^ 
	    "\nand writing help database in file " ^ helpfile ^ "\n");
     dirToBase (libdir, helpfile);
     print ("\nWriting ASCII signature index in file " ^ txtIndex ^ "\n");
     Printbase.printASCIIBase(helpfile, txtIndex);
     print ("\nWriting Latex signature index in file " ^ texIndex ^ "\n");
     Printbase.printLatexBase(helpfile, texIndex);
     print ("\nCreating HTML versions of signature files\n");
     Htmlsigs.sigsToHtml version bgcolor stoplist helpfile (libdir, htmldir);
     print ("\nWriting HTML signature index in file " ^ htmlIndex ^ "\n");
     Htmlsigs.printHTMLBase version bgcolor (helpfile, htmlIndex);
     print ("\nWriting LaTeX signature bodies in file " ^ texSigs ^ "\n");
     Texsigs.sigsToLatex (stoplist @ ["BasicIO"]) 
                         libdir helpfile texSigs)    

val _ = 
    case CommandLine.arguments () of
	[]       => 
	    process (libdirDef, helpfileDef, 
		     txtIndexDef, texIndexDef, htmlDirDef, htmlIndexDef)
      | [libdir] => 
	    process (libdir, helpfileDef, 
		     txtIndexDef, texIndexDef, htmlDirDef, htmlIndexDef)
      | _ => print "Usage: makebase\n"
