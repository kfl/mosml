(*  Simple Moscow ML HTTP server (needs Socket) sestoft@dina.kvl.dk 1999

    Compile:     mosmlc mosmlserver.sml -o mosmlserver
    Start:       mosmlserver
    Stop:        ctrl-C

    This server understands only GET requests, no CGI, and no ~users.
*)

val server  = "Moscow ML HTTP Server version 0.6 of 1999-06-28"
val myaddr  = "130.225.40.253"
val port    = 8080 
val docroot = "/home/sestoft/.public_html"
val admin   = "sestoft@dina.kvl.dk"

fun log msg = print msg;

local
    val mimetypes = 
        [(["html", "htm"],          "text/html"),
         (["txt"],                  "text/plain"),
         (["gif"],                  "image/gif"),
         (["jpeg", "jpg", "jpe"],   "image/jpeg"),
         (["png"],                  "image/png"),
         (["tiff", "tif"],          "image/tiff"),
         (["css"],                  "text/css"),
         (["eps", "ps"],            "application/postscript"),
         (["pdf"],                  "application/pdf"),
         (["dvi"],                  "application/x-dvi"),
         (["bin", "exe", "class"],  "application/octet-stream"),
         (["gtar"],                 "application/x-gtar"),
         (["latex"],                "application/x-latex"),
         (["tar"],                  "application/x-tar"),
         (["tex"],                  "application/x-tex"),
         (["texinfo", "texi"],      "application/x-texinfo"),
         (["zip"],                  "application/zip"),
         (["gz"] ,                  "application/gzip"),
         (["mws"],                  "application/x-maple"),
         (["doc"],                  "application/msword"),
         (["ppt"],                  "application/powerpoint")
         ]

    val mimehash : (string, string) Polyhash.hash_table = 
        Polyhash.mkPolyTable (length mimetypes, Fail "")
    fun addexts (exts, ty) =
        List.app (fn ext => Polyhash.insert mimehash (ext, ty)) exts
    val _ = List.app addexts mimetypes
    fun lowercase s = CharVector.map Char.toLower s
in
    fun mimetype ext = 
        case Option.mapPartial (Polyhash.peek mimehash o lowercase) ext of
            NONE      => "text/plain"
          | SOME mime => mime
end

fun date () = Date.toString(Date.fromTimeUniv(Time.now()))
   
fun mkheader header status fields mimetype len = 
    (log " "; log status; log " "; log (Int.toString len); log "\n"; 
     concat ([header, "\n", "Date: ", date(), "\n", "Server: ", server, "\n"]
             @ fields 
             @ ["Content-type: ", mimetype, "\n",
                "Content-length: ", Int.toString len, "\n\n"]))

val okheader = 
    mkheader "HTTP/1.0 200 OK" "200" []
fun movpermheader newpath = 
    mkheader "HTTP/1.0 301 Moved permanently" "301" 
             ["Location: " ^ newpath ^ "\n"] "text/html" 
val forbiddenheader = 
    mkheader "HTTP/1.0 403 Forbidden" "403" [] "text/html"
val notfoundheader = 
    mkheader "HTTP/1.0 404 Not Found" "404" [] "text/html"
val errorheader = 
    mkheader "HTTP/1.0 500 Internal server error" "500" [] "text/html"
val notimplheader = 
    mkheader "HTTP/1.0 501 Not implemented" "501" [] "text/html"

exception Notfound and Forbidden and Notimplemented and Directory of string

fun indexdoc dirpath =
    let val pre = ["<HTML><BODY><H1>Index of ", dirpath,
                   "</H1>\n<TABLE BORDER=0>"]
        val post = ["\n</TABLE></BODY></HTML>"]
        val entries = Listsort.sort String.compare (Mosml.listDir dirpath)
        fun fhref f r = 
            "<A HREF=\"" :: f :: "\"><CODE>" :: f :: "</CODE></A>" :: r
        fun fmtentry (f, r) = 
            "\n<TR><TD>" :: 
            (if f <> ".." andalso FileSys.isDir(Path.concat(dirpath, f)) then 
                 fhref (f ^ "/") r
             else
                 fhref f r)
    in concat (pre @ foldr fmtentry post (".." :: entries)) end 

(* Convert %xy to the character with hex code xy.  No checks done. *)

fun xurldecode s = 
    let fun hexval c = 
	    if #"0" <= c andalso c <= #"9" then ord c - 48 
	    else (ord c - 55) mod 32;
	fun loop []           acc = implode (rev acc)
	  | loop (#"%" :: cr) acc = 
	    (case cr of
		 c1 :: c2 :: cr' => 
		     loop cr' (chr (16 * hexval c1 + hexval c2) :: acc)
	       | _ => loop cr (#"%" :: acc))
	  | loop (c :: cr) acc = loop cr (c :: acc)
    in loop (explode s) [] end

datatype result = 
    String of string * string              (* a response and its MIME type  *)
  | File of BinIO.instream * int * string  (* instream, size, and MIME type *)
  | Failure of exn                         (* failure or exceptional result *)

fun response inp : result =
    let open Substring
        val _ = log (string (takel (Char.notContains "\r\n") (all inp)))
        val (method, sus1) = splitl Char.isAlpha (all inp)
        val _ = if string method <> "GET" then raise Notimplemented else ()
        val path1 = string (trimr 1 (#1 (position "HTTP" (triml 2 sus1))))
	val path2 = xurldecode path1
        (* Allow access only to docroot and its subdirectories: *)
        val path3 = case #arcs(Path.fromString (Path.mkCanonical path2)) of
                        ".." :: _ => raise Forbidden
                      | _ => Path.concat(docroot, path2)
        val indexes = ["index.html", "index.htm", "INDEX.HTML", "INDEX.HTM"]
        open FileSys
        fun tryindex path idx = access (Path.concat(path, idx), [])
        fun exists p = if access (p, []) then () else raise Notfound
        fun readbl p = if access (p, [A_READ]) then () else raise Forbidden
        fun execbl p = if access (p, [A_EXEC]) then () else raise Forbidden
        fun readdoc p = 
            (exists p; readbl p; 
             File (BinIO.openIn p, fileSize p, mimetype (Path.ext p)))
    in 
        if Path.file path3 = "" then
            (execbl path3;
             case List.find (tryindex path3) indexes of
                 SOME idx => readdoc (Path.concat(path3, idx))
               | NONE     => String (indexdoc path3, "text/html"))
        else if access(path3, []) andalso FileSys.isDir path3 then 
            raise Directory (Path.concat(path1, ""))
        else
            readdoc path3
    end handle e => Failure e

fun send sock vec = (Socket.sendVec(sock, { buf=vec, ofs=0, size=NONE }); ())

fun sendstr sock s = send sock (Byte.stringToBytes s)

fun senddoc sock header contents = 
    (sendstr sock (header (size contents)); sendstr sock contents)
    
fun htmldoc title contents =
    String.concat("<HTML><HEAD><TITLE>" :: title :: "</TITLE></HEAD>\n"
                  :: "<BODY><H1>" :: title :: "</H1>\n" 
                  :: contents :: ["</BODY></HTML>\n"])

fun addheader sock data = 
    case data of 
        String (s, mime) => senddoc sock (okheader mime) s 
      | File (is, size, mime) => 
            ((sendstr sock (okheader mime size);
	      while not (BinIO.endOfStream is) do 
		  send sock (BinIO.inputN(is, 1024)))
	     handle e => (BinIO.closeIn is; raise e);
	     BinIO.closeIn is)
      | Failure (Directory newpath) => senddoc sock (movpermheader newpath)
            (htmldoc "301 Moved permanently" "")
      | Failure Forbidden => senddoc sock forbiddenheader
            (htmldoc "403 Forbidden" "You have no access to that document.")
      | Failure Notfound => senddoc sock notfoundheader
            (htmldoc "404 Not found" "The requested URL was not found.")
      | Failure Notimplemented => senddoc sock notimplheader
            (htmldoc "501 Not implemented"
                     ("The server could not handle your request. Contact "
                      ^ admin ^ " if you think this is too bad."))
      | Failure _ => senddoc sock errorheader
            (htmldoc "500 Internal Server Error"
                     ("The server encountered an internal error.  Contact "
                      ^ admin))

val _ = 
    let val sock = Socket.inetStream ()
        val addr = Socket.inetAddr myaddr port
        val buf = Word8Array.array(10000, 0w0)
        fun gethttprequest sock =
            let val got = Socket.recvArr(sock, {buf = buf, ofs=0, size=NONE})
            in Byte.unpackString(buf, 0, SOME got) end
        fun next () = 
            let val (sock', a) = Socket.accept sock
	    in 
		log (Socket.getinetaddr a); log " ["; log (date()); log "] ";
		(addheader sock' (response (gethttprequest sock')))
		handle Fail s => print ("[" ^ s ^ "]");
		Socket.close sock';
                next ()
            end
        prim_val catch_interrupt : bool -> unit = 1 "sys_catch_break"
    in
        catch_interrupt true;
        Socket.bind(sock, addr);
        Socket.listen(sock, 150);
        log "Starting "; log server; log " on port "; 
	log (Int.toString port); log "\n";
        (next ()) handle Interrupt => () | Fail s => print s;
        log "Shutting down HTTP server\n";
        Socket.close sock
    end handle Fail s => print ("HTTP server failed: " ^ s ^ "\n");
