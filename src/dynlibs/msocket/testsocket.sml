(* File mosml/src/dynlibs/msocket/testsocket.sml
   This is a collect of socket examples, not a systematic test suite.
 *)

val _ = app load ["FileSys", "Socket", "Byte"];

(*
fun fileclient name s =
    let val sock = Socket.fileStream ()
	val addr = Socket.fileAddr name
	val vec  = Byte.stringToBytes s
	
	val _    = Socket.connect(sock, addr)
    in
	Socket.sendVec (sock, vec);
	Socket.recvVec (sock, size s)
    end

fun fileserver name =
    let val sock = Socket.fileStream ()
	val addr = Socket.fileAddr name
	val _    = Socket.bind(sock, addr)
	val _    = Socket.listen(sock, 150);
	val (n,a) = Socket.accept sock
	val vec = Socket.recvVec(n,20)
	val ls = Word8Vector.foldl op:: nil vec
	val s   = String.implode (map Byte.byteToChar ls)
    in
	Socket.sendVec(n,Byte.stringToBytes s);
	vec
    end
*)

fun buff v = { buf=v, ofs=0, size=NONE}

fun fileclient ipno s =
    let val sock = Socket.fileStream () before print "gif1\n"
	val addr = Socket.fileAddr ipno before print "gif2\n"
	val vec  = Byte.stringToBytes s
	
	val _    = Socket.connect(sock, addr) before print "gif3\n"
    in
	Socket.sendVec (sock, buff vec); 
	print "gif4\n";
	print(Byte.bytesToString(Socket.recvVec (sock, size s)));
	print "gif5\n";
	Socket.close sock; 
	print "gif6\n"
    end
 

fun fileserver ipno =
    let val sock = Socket.fileStream ()
	val addr = Socket.fileAddr ipno
	val _    = Socket.bind(sock, addr)
	val c    = ref 5;
    in
	Socket.listen(sock, 150);
	while !c > 0 do
	    let val (n,a) = Socket.accept sock
		val vec = Socket.recvVec(n,20)
		val ls = Word8Vector.foldl op:: nil vec
		val s   = String.implode (map Byte.byteToChar ls)
	    in
		print (Byte.bytesToString vec^"\n");
		Socket.sendVec(n, buff(Byte.stringToBytes s));
		Socket.close n;
		c := !c - 1
	    end;
	Socket.close sock
    end

fun inetclient ipno port s =
    let val sock = Socket.inetStream ()
	val addr = Socket.inetAddr ipno port
	val vec  = Byte.stringToBytes s
    in
	Socket.connect(sock, addr);
	Socket.sendVec (sock, buff vec); 
	Byte.bytesToString (Socket.recvVec (sock, size s)) before
	Socket.close sock
    end
 

fun inetserver ipno port =
    let val sock = Socket.inetStream ()
	val addr = Socket.inetAddr ipno port
	val _    = Socket.bind(sock, addr)
	val _    = Socket.listen(sock, 150);
	val c    = ref 7;
    in
	((while !c > 0 do
	    let
		val (n,a) = Socket.accept sock
		val vec = Socket.recvVec(n,20)
		(* Reverse the bytes in the vector *)
		val ls = Word8Vector.foldl op:: nil vec
		val s   = String.implode (map Byte.byteToChar ls)
	    in
		Socket.sendVec(n, buff(Byte.stringToBytes s));
		print ("Server: sent response " ^ s ^ " to client\n");
		Socket.close n;
		c := !c - 1
	    end)
	    handle Interrupt => ());
	Socket.close sock
    end

fun revmsg from =
    let val (sock,a) = Socket.accept from
	val vec = Socket.recvVec(sock,20)
	val ls = Word8Vector.foldl op:: nil vec
	val s   = String.implode (map Byte.byteToChar ls)
    in
	print ("Server: "^Byte.bytesToString vec^"\n");
	Socket.sendVec(sock, buff(Byte.stringToBytes s));
	Socket.close sock
    end

fun selinet ipno port1 port2 =
    let val sock1 = Socket.inetStream ()
	val sock2 = Socket.inetStream ()
	val addr1 = Socket.inetAddr ipno port1
	val addr2 = Socket.inetAddr ipno port2
	val _     = Socket.bind(sock1, addr1)
	val _     = Socket.bind(sock2, addr2)
	val _     = Socket.listen(sock1, 150);
	val _     = Socket.listen(sock2, 150);
	val socks = [sock1, sock2]
	val rls   = map Socket.sockDesc socks
	fun getrdys [] _ = []
	  | getrdys (sck1::sckr) (rdys as (rdy1::rdyr)) = 
	    if Socket.sameDesc(Socket.sockDesc sck1, rdy1) then 
		sck1 :: getrdys sckr rdyr
	    else 
		getrdys sckr rdys
    in 
	((while true do
	    let val {rds, ...} = 
		Socket.select { rds=rls, wrs=nil, exs=nil, timeout=NONE }
	    in  
		app revmsg (getrdys socks rds)
	    end) handle _ => ());
	Socket.close sock1;
	Socket.close sock2
    end;

val _ = 
    print 
    "\nSimple client/server example with sockets\n\
     \-----------------------------------------\n\
     \Assume that the IP address of your machine is 130.225.40.253.\n\
     \and let us choose port number 3000 for this experiment.\n\
     \(1) Run this in an Moscow ML session on your machine:\n\
     \    inetserver \"130.225.40.253\" 3000;\n\
     \(2) Run this in another mosml session, possibly on another machine:\n\
     \    val f = inetclient \"130.225.40.253\" 3000;\n\
     \    f \"abcde\";\n\
     \    f \"reversed\";\n\
     \    f \"12345\";\n\n"

