(* Minimal HTTP server in Moscow ML (needs Socket) sestoft@dina.kvl.dk 

   Compile:     mosmlc minimalserver.sml -o minimalserver
   Start:       minimalserver
   Stop:        ctrl-C

   Pretends every request is a GET, does not understand MIME, CGI, or ~user.
   Very primitive, but illustrates basic HTTP and sockets.
*)

val server  = "Moscow ML Minimal Server version 0.1 of 1999-03-07"
val myaddr  = "130.225.40.253"
val port    = 8088
val docroot = "/home/sestoft/.public_html"
val admin   = "sestoft@dina.kvl.dk"

fun mkheader header len = 
    let val date = Date.toString(Date.fromTimeUniv(Time.now()))
    in concat [header, "\n",
               "Date: ", date, "\n",
               "Server: ", server, "\n",
               "Content-type: text/html\n",
               "Content-length: ", Int.toString len, "\n\n"]
    end

val okheader    = mkheader "HTTP/1.0 200 OK" 
val errorheader = mkheader "HTTP/1.0 500 Internal server error" 

fun response inp =
    let open Substring
        val _ = print (string (takel (fn c => c <> #"\n") (all inp)) ^ "\n")
        val (method, sus1) = splitl Char.isAlpha (all inp)
        val p1 = string (trimr 1 (#1 (position "HTTP" (triml 2 sus1))))
        val p2 = case #arcs(Path.fromString (Path.mkCanonical p1)) of
                     ".." :: _ => raise Fail "illegal path"
                   | _ => Path.concat(docroot, p1)
        val p3 = if FileSys.isDir p2 then Path.concat(p2, "index.html") else p2
        val is = TextIO.openIn p3
    in (TextIO.inputAll is) before (TextIO.closeIn is) end

datatype result = String of string | Fail of exn

fun send n s = Socket.sendVec(n, {buf=Byte.stringToBytes s, ofs=0, size=NONE})
fun senddoc n header data = (send n (header (size data)); send n data)
    
fun addheader n data = 
    case data of 
        String s => senddoc n okheader s 
      | Fail _   => senddoc n errorheader
            ("<HTML><HEAD>\n<TITLE>500 Internal Server Error</TITLE></HEAD>\n\
             \<BODY>\n<H1>Internal Server Error</H1>\n\
             \The server could not handle your request.\n</BODY></HTML>\n")

val _ =
    let val sock = Socket.inetStream ()
        val addr = Socket.inetAddr myaddr port
        fun next () = 
            let val (sock', _) = Socket.accept sock
                val req = Byte.bytesToString(Socket.recvVec(sock', 10000))
                val data = (String (response req)) handle e => Fail e
            in
                addheader sock' data;
                Socket.close sock';
                next ()
            end
        prim_val catch_interrupt : bool -> unit = 1 "sys_catch_break"
    in
        catch_interrupt true;
        Socket.bind(sock, addr); 
        Socket.listen(sock, 150);
        print "Starting HTTP server\n";
        (next ()) handle Interrupt => ();
        print "Shutting down HTTP server\n";
        Socket.close sock
    end
