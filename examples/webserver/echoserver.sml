(* HTTP echo server in Moscow ML (needs Socket) sestoft@dina.kvl.dk

   Compile:	mosmlc echoserver.sml -o echoserver
   Start:       echoserver
   Stop:        ctrl-C
*)

val server  = "Moscow ML HTTP echo server of 1999-03-07"
val myaddr  = "130.225.40.253"
val port    = 8081 
val admin   = "sestoft@dina.kvl.dk"

fun mkheader header len = 
    let val date = Date.toString(Date.fromTimeUniv(Time.now()))
    in
	concat [header, "\n",
		"Date: ", date, "\n",
		"Server: ", server, "\n",
		"Content-type: text/html\n",
		"Content-length: ", Int.toString len, "\n\n"]
    end

val okheader    = mkheader "HTTP/1.0 200 OK" 
val errorheader = mkheader "HTTP/1.0 500 Internal server error" 

fun response inp = concat ["<HTML><BODY><PRE>", inp, "</PRE></BODY></HTML>"]

datatype result = String of string | Fail of exn

fun send sock s = 
    Socket.sendVec(sock, {buf=Byte.stringToBytes s, ofs=0, size=NONE})

fun senddoc sock header contents = 
    let val hdr = header (size contents)
    in print hdr; send sock hdr; send sock contents end
    
fun addheader sock data = 
    case data of 
        String s => senddoc sock okheader s 
      | Fail _   => senddoc sock errorheader
            ("<HTML><HEAD>\n<TITLE>500 Internal Server Error</TITLE></HEAD>\n\
             \<BODY>\n<H1>Internal Server Error</H1>\n\
             \The server could not handle your request.\n</BODY></HTML>\n")

val _ = 
    let val sock = Socket.inetStream ()
	val addr = Socket.inetAddr myaddr port
	fun next () = 
	    let val (sock',a) = Socket.accept sock
		val req = Byte.bytesToString(Socket.recvVec(sock',10000))
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
	print "Starting HTTP echo server\n";
	(next ()) handle Interrupt => ();
	print "Shutting down HTTP echo server\n";
	Socket.close sock
    end
