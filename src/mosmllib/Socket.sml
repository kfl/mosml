(* Ken Larsen kla@it.dtu.dk 1998-10-26 *)

(* The initial implementation was financed by the PROSPER project. *)

(* Beautification and documentation by sestoft@dina.kvl.dk 
   1999-02-01, 2000-05-16 *)

structure Socket :> Socket =
struct

    prim_type sock_   (* an abstract value containing a socket descriptor   *)
    prim_type addr    (* union saddr = sockaddr + sockaddr_un + sockaddr_in *)

    datatype ('addressfam, 'socktype) sock = SOCK of sock_
    datatype 'addressfam sock_addr = ADDR of addr

    (* witness types for the socket parameter *)
    type dgram     = unit
    type 'a stream = unit
    type passive   = unit
    type active    = unit

    (* witness types for the addressfam parameter *)
    type pf_file = unit
    type pf_inet = unit

    local 
	open Dynlib
	val hdl  = dlopen {lib = "libmsocket.so",
			   flag = RTLD_LAZY, global = false}
	val symb = Dynlib.dlsym hdl
	fun app1 name = Dynlib.app1 (symb ("msocket_"^name))
	fun app2 name = Dynlib.app2 (symb ("msocket_"^name))
	fun app3 name = Dynlib.app3 (symb ("msocket_"^name))
	fun app4 name = Dynlib.app4 (symb ("msocket_"^name))
	fun app5 name = Dynlib.app5 (symb ("msocket_"^name))

	val constants_ : unit  -> int * int * int * int 
	    * int * int * int * int 
	    * int * int             
	           = app1 "constants" 

	val newfileaddr_ : string -> addr       = app1 "newfileaddr"
	val newinetaddr_ : string ->int -> addr = app2 "newinetaddr"

	            (* domain, type *)
	val socket_   : int -> int -> sock_      = app2 "socket"

	val accept_   : sock_ -> sock_ * addr    = app1 "accept"
	val bind_     : sock_ -> addr -> unit    = app2 "bind"
	val connect_  : sock_ -> addr -> unit    = app2 "connect"
	val listen_   : sock_ -> int -> unit     = app2 "listen"
	val close_    : sock_ -> unit            = app1 "close"
	val shutdown_ : sock_ -> int -> unit     = app2 "shutdown"

	val getinetaddr_ : addr -> string        = app1 "getinetaddr"

	type buff = Word8Vector.vector	(* Even for arrays; see a2v *)

	(* The integer arguments are: offset, size, flags *)

	val send_     : sock_ -> buff -> int -> int -> int -> int 
	                = app5 "send"
	val sendto_   : sock_ -> buff -> int * int -> int -> addr -> int
	                = app5 "sendto"
	val recv_     : sock_ -> buff -> int -> int -> int -> int 
                        = app5 "recv"
	val recvfrom_ : sock_ -> buff -> int -> int -> int -> int * addr
                        = app5 "recvfrom"

	val select_   : sock_ vector -> sock_ vector -> sock_ vector -> 
                        int -> int -> sock_ list * sock_ list * sock_ list
                        = app5 "select"
			
	val desccmp_  : sock_ -> sock_ -> int = app2 "desccmp";

	val (SOCK_STREAM,
	     SOCK_DGRAM,
	     PF_UNIX,
	     PF_INET,
	     NO_RECVS_,
	     NO_SENDS_, 
	     NO_RECVS_OR_SENDS_,
	     MSG_OOB,
	     MSG_PEEK,
	     MSG_DONTROUTE) = constants_ ()
	
	val (MSG_OOB,
	     MSG_PEEK,
	     MSG_DONTROUTE) = (Word.fromInt MSG_OOB,
			       Word.fromInt MSG_PEEK,
			       Word.fromInt MSG_DONTROUTE)

	prim_val vector_ : int -> Word8Vector.vector = 1 "create_string"

	fun extract vec len = Word8Vector.extract(vec, 0, SOME len)
    in
	fun getinetaddr (ADDR a : pf_inet sock_addr) = getinetaddr_ a

	fun fileAddr s = ADDR(newfileaddr_ s)
	fun inetAddr s port = ADDR(newinetaddr_ s port)

	fun fileStream () = SOCK(socket_ PF_UNIX SOCK_STREAM)
	fun fileDgram ()  = SOCK(socket_ PF_UNIX SOCK_DGRAM)
	fun inetStream () = SOCK(socket_ PF_INET SOCK_STREAM)
	fun inetDgram ()  = SOCK(socket_ PF_INET SOCK_DGRAM)

	fun accept (SOCK sock) = 
	    let val (s,a) = accept_ sock
	    in  (SOCK s, ADDR a)
	    end

	fun bind (SOCK sock, ADDR addr) = bind_ sock addr

	fun connect (SOCK sock, ADDR addr) = connect_ sock addr

	fun listen (SOCK sock, queuelen) = listen_ sock queuelen

	fun close (SOCK sock) = close_ sock

	datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS
	
	fun shutdown (SOCK sock, NO_RECVS) = 
	    shutdown_ sock NO_RECVS_
	  | shutdown (SOCK sock, NO_SENDS) = 
	    shutdown_ sock NO_SENDS_
	  | shutdown (SOCK sock, NO_RECVS_OR_SENDS) = 
	    shutdown_ sock NO_RECVS_OR_SENDS_

	(* Buffers = subvectors and subarrays for output and input *)
   
	type 'a buf = {buf : 'a, ofs : int, size : int option}

	fun chkvbuf {buf : Word8Vector.vector, ofs : int, size : int option} = 
	    let val len = Word8Vector.length buf
		val sz  = case size of NONE => len - ofs | SOME n => n
	    in
		if ofs<0 orelse sz<0 orelse ofs+sz>len then raise Subscript
		else (ofs, sz) 
	    end		

	fun chkabuf {buf : Word8Array.array, ofs : int, size : int option} = 
	    let val len = Word8Array.length buf
		val sz  = case size of NONE => len - ofs | SOME n => n
	    in
		if ofs<0 orelse sz<0 orelse ofs+sz>len then raise Subscript
		else (ofs, sz) 
	    end		

	(* Output flags *)
	
	type out_flags = {don't_route : bool, oob : bool}

	fun getoflags {don't_route, oob} =
	    Word.toInt (Word.orb(if don't_route then MSG_DONTROUTE else 0w0,
				 if oob         then MSG_OOB       else 0w0))

	fun sendVec (SOCK sock, vbuf) =
	    let val (ofs, sz) = chkvbuf vbuf 
	    in send_ sock (#buf vbuf) ofs sz 0 end

	fun sendVec' (SOCK sock, vbuf, oflags) =
	    let val (ofs, sz) = chkvbuf vbuf 
	    in send_ sock (#buf vbuf) ofs sz (getoflags oflags) end

	fun sendVecTo (SOCK sock, ADDR addr, vbuf) =
	    let val (ofs, sz) = chkvbuf vbuf 
	    in sendto_ sock (#buf vbuf) (ofs, sz) 0 addr end

	fun sendVecTo' (SOCK sock, ADDR addr, vbuf, oflags) =
	    let val (ofs, sz) = chkvbuf vbuf 
	    in sendto_ sock (#buf vbuf) (ofs, sz) (getoflags oflags) addr end

	(* Warning: this crucially depends on the representation of a
  	   Word8Array.array as a reference to a Word8Vector.vector;
	   see mosml/src/mosmllib/Word8Array.mlp: *)

	fun a2v (a : Word8Array.array) = 
	    let prim_val deref : Word8Array.array -> Word8Vector.vector
		= 1 "field0";
	    in deref a end

	fun sendArr (SOCK sock, abuf) =
	    let val (ofs, sz) = chkabuf abuf 
	    in send_ sock (a2v (#buf abuf)) ofs sz 0 end

	fun sendArr' (SOCK sock, abuf, oflags) =
	    let val (ofs, sz) = chkabuf abuf 
	    in send_ sock (a2v (#buf abuf)) ofs sz (getoflags oflags) end

	fun sendArrTo (SOCK sock, ADDR addr, abuf) =
	    let val (ofs, sz) = chkabuf abuf 
	    in sendto_ sock (a2v (#buf abuf)) (ofs, sz) 0 addr end

	fun sendArrTo' (SOCK sock, ADDR addr, abuf, oflags) =
	    let val (ofs, sz) = chkabuf abuf 
	    in 
		sendto_ sock (a2v (#buf abuf)) (ofs, sz) 
		        (getoflags oflags) addr 
	    end

	(* Input flags *)

	type in_flags = {peek : bool, oob : bool}

	fun getiflags {peek, oob} =
	    Word.toInt(Word.orb(if peek then MSG_PEEK else 0w0,
				if oob  then MSG_OOB  else 0w0))

	fun recvVec (SOCK sock, len) =
	    let val vec  = vector_ len 
		val size = recv_ sock vec 0 len 0; 
	    in extract vec size end

	fun recvArr (SOCK sock, abuf) =
	    let val (ofs, sz) = chkabuf abuf
	    in recv_ sock (a2v (#buf abuf)) ofs sz 0 end

	fun recvVec' (SOCK sock, len, iflags) =
	    let val vec  = vector_ len 
		val size = recv_ sock vec 0 len (getiflags iflags) 
	    in extract vec size end

	fun recvArr' (SOCK sock, abuf, iflags) =
	    let val (ofs, sz) = chkabuf abuf
	    in recv_ sock (a2v (#buf abuf)) ofs sz (getiflags iflags) end

	fun recvVecFrom (SOCK sock, len) =
	    let val vec  = vector_ len 
		val (size, addr) = recvfrom_ sock vec 0 len 0; 
	    in (extract vec size, ADDR addr) end

	fun recvArrFrom (SOCK sock, abuf) =
	    let val (ofs, sz) = chkabuf abuf
		val (size, addr) = recvfrom_ sock (a2v (#buf abuf)) ofs sz 0
	    in (size, ADDR addr) end

	fun recvVecFrom' (SOCK sock, len, iflags) =
	    let val vec  = vector_ len
		val (size, addr) = recvfrom_ sock vec 0 len (getiflags iflags)
	    in (extract vec size, ADDR addr) end

	fun recvArrFrom' (SOCK sock, abuf, iflags) =
	    let val (ofs, sz) = chkabuf abuf
		val (size, addr) = 
		    recvfrom_ sock (a2v (#buf abuf)) ofs sz (getiflags iflags)
	    in (size, ADDR addr) end

	(* We can let sock_desc be a synonym for sock_, and compare
	   sock_descs by looking at the value (in Unix, int) inside
	   the sock_ *)

	type sock_desc = sock_ 

	fun sockDesc (SOCK sock_) = sock_
	fun sameDesc (sock1_, sock2_) = (desccmp_ sock1_ sock2_ = 0)

	fun compare (sock1_, sock2_) =
	    let val cmp = desccmp_ sock1_ sock2_ 
	    in if cmp < 0 then LESS
	       else if cmp > 0 then GREATER
	       else EQUAL
	    end

	(* Note: This must agree with the particular representation of
	   Time.time found in mosml/src/mosmllib/Time.sml: *)

        prim_val fromtime : Time.time -> {sec : int, usec : int} = 1 "identity"
	val time_timebase = ~1073741824; 

	fun select { rds, wrs, exs, timeout } =
	    let val (tsec, tusec) = 
		    case timeout of
			NONE   => (~1,0)
		      | SOME t => let val {sec, usec} = fromtime t 
				  in (sec - time_timebase, usec) end
		val rvec = Vector.fromList rds
		val wvec = Vector.fromList wrs
		val evec = Vector.fromList exs
		val (rds', wrs', exs') = select_ rvec wvec evec tsec tusec
	    in { rds = rds', wrs = wrs', exs = exs' } end
    end
end
