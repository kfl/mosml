(* File mosml/src/dynlibs/msocket/testsocket.sml
   This is a collect of socket examples, not a systematic test suite.
 *)

val _ = app load ["FileSys", "Socket", "Byte"];

fun buff v = { buf=v, ofs=0, size=NONE}

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

val _ = 
    print 
    "\nSimple client/server example with sockets\n\
     \-----------------------------------------\n\
     \Loaded inetclient function\n\n\
     \You must start the inetclient function with\n\
     \    val f = inetclient <ipaddress> <portno>;\n\
     \Then evaluate e.g.\n\
     \    f \"abcde\";\n\
     \    f \"reversed\";\n\
     \    f \"12345\";\n\n"
