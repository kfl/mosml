(* File mosml/src/dynlibs/msocket/testserver.sml
   This is a collect of socket examples, not a systematic test suite.
 *)

val inetaddr = "127.0.0.1"
val portno   = 3000;

val _ = app load ["FileSys", "Socket", "Byte", "Int"];

fun buff v = { buf=v, ofs=0, size=NONE}

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
		val s  = String.implode (map Byte.byteToChar ls)
	    in
		Socket.sendVec(n, buff(Byte.stringToBytes s));
		print ("Server: sent response " ^ s ^ " to client\n");
		Socket.close n;
		c := !c - 1
	    end)
	    handle Interrupt => print "Server terminated by user.\n\n");
	Socket.close sock
    end 

val _ = 
    print 
    ("\nSimple client/server example with sockets\n\
       \-----------------------------------------\n\
     \Starting example inetserver on IP address " 
     ^ inetaddr ^ " port " ^ Int.toString portno 
     ^ "\n\nIn another terminal, start a mosml session by typing\n\
     \    mosml testclient.sml\n\
     \and in that session, execute e.g.\n\
     \    val f = inetclient \"" ^ inetaddr ^ "\" " ^ Int.toString portno 
     ^ ";\n\
     \    f \"Test\";\n\
     \    f \"madam, I'm adam\";\n\n\
     \or similar\n\n")

val _ = inetserver inetaddr portno;
