/* File mosml/src/dynlibs/msocket/msocket.c
   Ken Larsen 1998: design and implementation
   The initial implementation was financed by the PROSPER project
   Peter Sestoft 1999: beautification and robustness
   Doug Currie and Sergei Romanenko: adaptation to MacOS and MS Win32 
   Last update: 1999-08-30
 */

/* General includes */

#include <errno.h>
#include <sys/types.h>
#ifdef WIN32
#include <winsock2.h>
#else
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#endif
#include <signal.h>

/* Moscow ML includes */

#ifdef macintosh
#undef bcopy
#endif

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <str.h>
#include <signals.h>

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

/* ML representations of values used in this interface: 

Type sock_        = Socket.sock_ = ML abstract object representing a socket
                    Corresponds to the C type int used to represent a socket
     0: socket     = the socket as a C int

Type addr         = Socket.addr = ML 3-tuple representing a socket address:
     0: size       = ML int specifying the size of the data C representation
     1: nspace     = ML int specifying the name space AF_UNIX or AF_INET
     2: data       = ML abstract object, either
                     for AF_UNIX: an ML string giving the file name
                     for AF_INET: an ML object of `type' sinaddrport

Type sinaddrport = ML 2-tuple representing INET socket address and port number
                   Corresponds to the C type struct sockaddr_in.
     0: ml_s_addr  = ML abstract object of `type' ml_s_addr
     1: port       = ML int specifying the port number

Type ml_s_addr    = ML abstract object containing an INET socket's address
                    Corresponds to the C type s_addr_t = unsigned long int
     0: s_addr     = the socket's s_addr, of C type unsigned long int
*/

/* Decomposition of sock_ values: */
#ifdef macintosh
#define Sock_val(x) (Field(x,0))
#else
#define Sock_val(x) ((int) Field(x,0))
#endif

/* Decomposition of addr values: */
#define Size_addrval(a)   Field(a, 0)
#define Nspace_addrval(a) Field(a, 1)
#define Data_addrval(a)   Field(a, 2)

/* Decomposition of sinaddrport values: */
#define Mlsaddr_sapval(sap) Field(sap, 0)
#define Port_sapval(sap)    Field(sap, 1)

/* Decomposition of ml_s_addr values: */
#define Saddr_mlsaddrval(mlsa) Field(mlsa, 0)

union saddr {
  struct sockaddr sockaddr_gen;
#ifndef WIN32
  struct sockaddr_un sockaddr_unix;
#endif
  struct sockaddr_in sockaddr_inet;
};

/* ML type: unit -> int * int * int * int * int * int * int * int * int *int */
EXTERNML value msocket_constants(value dummy) {
  value res = alloc_tuple(10);
  Field(res, 0) = Val_long(SOCK_STREAM);
  Field(res, 1) = Val_long(SOCK_DGRAM);
  Field(res, 2) = Val_long(PF_UNIX);
  Field(res, 3) = Val_long(PF_INET);
  Field(res, 4) = Val_long(0); /* NO_RECVS */
  Field(res, 5) = Val_long(1); /* NO_SENDS */
  Field(res, 6) = Val_long(2); /* NO_RECVS_OR_SENDS */
  Field(res, 7) = Val_long(MSG_OOB);
  Field(res, 8) = Val_long(MSG_PEEK);
  Field(res, 9) = Val_long(MSG_DONTROUTE);
  return res;
}

/* Warning: allocates in the heap, may cause a GC */
/* ML return type: sock_ */
static value newsocket(int sock) {
  value result = alloc(1, Abstract_tag);
  Sock_val(result) = sock;
  return result;
}

typedef unsigned long s_addr_t;

/* Warning: allocates in the heap, may cause a GC */
static value newinaddr(s_addr_t sa) {
  int bsize = sizeof(s_addr_t);
  int wsize = (bsize + sizeof(value) - 1) / sizeof(value); /* rounding up */
  value result = alloc(wsize, Abstract_tag);
  *((s_addr_t*) result) = sa;
  return result;
}

/* Maps a : addr to s : union saddr */
static void make_saddr(union saddr *s, value a) {
  int size = Int_val(Size_addrval(a));

  switch(Int_val(Nspace_addrval(a))) {
  case AF_UNIX:
#ifndef WIN32
    s->sockaddr_unix.sun_family = AF_UNIX;
    bcopy(String_val(Data_addrval(a)), s->sockaddr_unix.sun_path, size + 1);
#else
    failwith("AF_UNIX not implemented");
#endif
    break;
  case AF_INET: {
    value sinaddrport = Data_addrval(a);
    s->sockaddr_inet.sin_family = AF_INET;
    s->sockaddr_inet.sin_addr.s_addr = 
      *((s_addr_t*) Mlsaddr_sapval(sinaddrport));
    /* Maybe this should be htonl? / sestoft */
    s->sockaddr_inet.sin_port = htons(Int_val(Port_sapval(sinaddrport)));
    break;
  }
  }
} 

/* Warning: allocates in the heap, may cause a GC */
/* ML result type: addr */
static value newaddr(int len, int namespace, value addrdata) {
  value res;
  Push_roots(r,1)
  r[0] = addrdata;
  res = alloc_tuple(3);
  Data_addrval(res) = r[0];
  Size_addrval(res) = Val_int(len);
  Nspace_addrval(res) = Val_int(namespace);
  Pop_roots();
  return (value) res;
} 

/* Warning: allocates in the heap, may cause a GC */
/* Return type: sinaddrport = int * ml_s_addr */
value newsinaddrport(s_addr_t sa, value port) {
  value res;
  Push_roots(r,1);  
  r[0] = alloc_tuple(2); 
    
  Field(r[0], 0) = 0; /* to please the gc */
  Field(r[0], 1) = 0;
  
  modify(&Mlsaddr_sapval(r[0]), newinaddr(sa));
  modify(&Port_sapval(r[0]), port);
  res = r[0];
  Pop_roots();
  return res;
}

/* Warning: allocates in the heap, may cause a GC */
/* ML result type: addr */
static value from_saddr(union saddr *s, int len) {
  switch(s->sockaddr_gen.sa_family) {
  case AF_UNIX: { 
#ifndef WIN32
    value name = copy_string(s->sockaddr_unix.sun_path);
    return newaddr(len, AF_UNIX, name);
#else
    failwith("AF_UNIX not implemented for Win32");
#endif
  }
  case AF_INET: {
    value sinaddrport = 
      newsinaddrport(s->sockaddr_inet.sin_addr.s_addr,
                     Val_int(ntohs(s->sockaddr_inet.sin_port)));
    /* The native representation of a sinaddrport is struct sockaddr_in */
    return newaddr(sizeof(struct sockaddr_in), AF_INET, sinaddrport);
  }
  } 
}

void failure()
{
#ifndef WIN32
  switch (errno) {
  case EPROTONOSUPPORT : 
    failwith("EPROTONOSUPPORT"); break;
  case EMFILE :
    failwith("EMFILE"); break;
  case ENFILE : 
    failwith("ENFILE"); break;
  case EACCES : 
    failwith("EACCES"); break;
  case ENOBUFS : 
    failwith("ENOBUFS"); break;
  case EBADF:
    failwith("EBADF"); break;
  case ENOTSOCK:
    failwith("ENOTSOCK"); break;
  case EOPNOTSUPP:
    failwith("EOPNOTSUPP"); break;
  case EWOULDBLOCK:
    failwith("EWOULDBLOCK"); break;
  case EADDRNOTAVAIL:
    failwith("EADDRNOTAVAIL"); break;
  case EADDRINUSE:
    failwith("EADDRINUSE"); break;
  case EINVAL:
    failwith("EINVAL"); break;
  case EAFNOSUPPORT:
    failwith("EAFNOSUPPORT"); break;
  case EISCONN:
    failwith("EISCONN"); break;
  case ETIMEDOUT:
    failwith("ETIMEDOUT"); break;
  case ECONNREFUSED:
    failwith("ECONNREFUSED"); break;
  case ENETUNREACH:
    failwith("ENETUNREACH"); break;
  case EINPROGRESS:
    failwith("EINPROGRESS"); break;
  case EALREADY:
    failwith("EALREADY"); break;
  case ENOENT:
    failwith("ENOENT"); break;
  case EINTR:
    failwith("EINTR"); break;
  case EMSGSIZE:
    failwith("EMSGSIZE"); break;
  case ENOTCONN:
    failwith("ENOTCONN"); break;
  case EPIPE:
    failwith("EPIPE"); break;
  case ECONNRESET:
    failwith("ECONNRESET"); break;
  default:
    failwith("EUNSPECIFIED"); break;
  }

#else

  switch (WSAGetLastError())
  {
  case EMFILE :
    failwith("EMFILE"); break;
  case ENFILE : 
    failwith("ENFILE"); break;
  case EACCES : 
    failwith("EACCES"); break;
  case EBADF:
    failwith("EBADF"); break;
  case EINVAL:
    failwith("EINVAL"); break;
  case ENOENT:
    failwith("ENOENT"); break;
  case EINTR:
    failwith("EINTR"); break;
  case EPIPE:
    failwith("EPIPE"); break;

  case WSAEINTR: failwith("WSAEINTR"); break;
  case WSAEBADF: failwith("WSAEBADF"); break;
  case WSAEACCES: failwith("WSAEACCES"); break;
  case WSAEFAULT: failwith("WSAEFAULT"); break;
  case WSAEINVAL: failwith("WSAEINVAL"); break;
  case WSAEMFILE: failwith("WSAEMFILE"); break;

/*
 * Windows Sockets definitions of regular Berkeley error constants
 */
  case WSAEWOULDBLOCK: failwith("WSAEWOULDBLOCK"); break;
  case WSAEINPROGRESS: failwith("WSAEINPROGRESS"); break;
  case WSAEALREADY: failwith("WSAEALREADY"); break;
  case WSAENOTSOCK: failwith("WSAENOTSOCK"); break;
  case WSAEDESTADDRREQ: failwith("WSAEDESTADDRREQ"); break;
  case WSAEMSGSIZE: failwith("WSAEMSGSIZE"); break;
  case WSAEPROTOTYPE: failwith("WSAEPROTOTYPE"); break;
  case WSAENOPROTOOPT: failwith("WSAENOPROTOOPT"); break;
  case WSAEPROTONOSUPPORT: failwith("WSAEPROTONOSUPPORT"); break;
  case WSAESOCKTNOSUPPORT: failwith("WSAESOCKTNOSUPPORT"); break;
  case WSAEOPNOTSUPP: failwith("WSAEOPNOTSUPP"); break;
  case WSAEPFNOSUPPORT: failwith("WSAEPFNOSUPPORT"); break;
  case WSAEAFNOSUPPORT: failwith("WSAEAFNOSUPPORT"); break;
  case WSAEADDRINUSE: failwith("WSAEADDRINUSE"); break;
  case WSAEADDRNOTAVAIL: failwith("WSAEADDRNOTAVAIL"); break;
  case WSAENETDOWN: failwith("WSAENETDOWN"); break;
  case WSAENETUNREACH: failwith("WSAENETUNREACH"); break;
  case WSAENETRESET: failwith("WSAENETRESET"); break;
  case WSAECONNABORTED: failwith("WSAECONNABORTED"); break;
  case WSAECONNRESET: failwith("WSAECONNRESET"); break;
  case WSAENOBUFS: failwith("WSAENOBUFS"); break;
  case WSAEISCONN: failwith("WSAEISCONN"); break;
  case WSAENOTCONN: failwith("WSAENOTCONN"); break;
  case WSAESHUTDOWN: failwith("WSAESHUTDOWN"); break;
  case WSAETOOMANYREFS: failwith("WSAETOOMANYREFS"); break;
  case WSAETIMEDOUT: failwith("WSAETIMEDOUT"); break;
  case WSAECONNREFUSED: failwith("WSAECONNREFUSED"); break;
  case WSAELOOP: failwith("WSAELOOP"); break;
  case WSAENAMETOOLONG: failwith("WSAENAMETOOLONG"); break;
  case WSAEHOSTDOWN: failwith("WSAEHOSTDOWN"); break;
  case WSAEHOSTUNREACH: failwith("WSAEHOSTUNREACH"); break;
  case WSAENOTEMPTY: failwith("WSAENOTEMPTY"); break;
  case WSAEPROCLIM: failwith("WSAEPROCLIM"); break;
  case WSAEUSERS: failwith("WSAEUSERS"); break;
  case WSAEDQUOT: failwith("WSAEDQUOT"); break;
  case WSAESTALE: failwith("WSAESTALE"); break;
  case WSAEREMOTE: failwith("WSAEREMOTE"); break;

/*
 * Extended Windows Sockets error constant definitions
 */
  case WSASYSNOTREADY: failwith("WSASYSNOTREADY"); break;
  case WSAVERNOTSUPPORTED: failwith("WSAVERNOTSUPPORTED"); break;
  case WSANOTINITIALISED: failwith("WSANOTINITIALISED"); break;
  case WSAEDISCON: failwith("WSAEDISCON"); break;
  case WSAENOMORE: failwith("WSAENOMORE"); break;
  case WSAECANCELLED: failwith("WSAECANCELLED"); break;
  case WSAEINVALIDPROCTABLE: failwith("WSAEINVALIDPROCTABLE"); break;
  case WSAEINVALIDPROVIDER: failwith("WSAEINVALIDPROVIDER"); break;
  case WSAEPROVIDERFAILEDINIT: failwith("WSAEPROVIDERFAILEDINIT"); break;
  case WSASYSCALLFAILURE: failwith("WSASYSCALLFAILURE"); break;
  case WSASERVICE_NOT_FOUND: failwith("WSASERVICE_NOT_FOUND"); break;
  case WSATYPE_NOT_FOUND: failwith("WSATYPE_NOT_FOUND"); break;
  case WSA_E_NO_MORE: failwith("WSA_E_NO_MORE"); break;
  case WSA_E_CANCELLED: failwith("WSA_E_CANCELLED"); break;
  case WSAEREFUSED: failwith("WSAEREFUSED"); break;
  default:
    failwith("EUNSPECIFIED"); break;
  }
#endif
}

/* ML type: sock_ -> sock_ -> int */
EXTERNML value msocket_desccmp(value sockval1, value sockval2) {
  int sock1 = Sock_val(sockval1);
  int sock2 = Sock_val(sockval2);
  if (sock1 < sock2)
    return Val_long(-1);
  else if (sock1 > sock2)
    return Val_long(1);
  else   
    return Val_long(0);
}

/* ML type: string -> addr */
EXTERNML value msocket_newfileaddr(value name) { 
#ifndef WIN32
  struct sockaddr_un dummy;

  mlsize_t len = string_length(name);
  int addr_len = offsetof (struct sockaddr_un, sun_path) + len + 1;

  if (len >= sizeof(dummy.sun_path)) {
    failwith("ENAMETOOLONG");
  }

  return newaddr(addr_len, AF_UNIX, name);
#else
  failwith("AF_UNIX not implemented for Win32");
  return Val_unit;
#endif
}

/* Solaris 2.5, MacOS and MS Win32 lack inet_aton: */

int my_aton(const char* name, struct in_addr *inp) {
#if ( defined(sun) && defined(__svr4__) ) || defined(WIN32)
  inp->s_addr = inet_addr(name);
  /* Pretend that everything went well: */
  return 1;
#elif defined(macintosh)
  inp->s_addr = inet_addr(name).s_addr;
  /* Pretend that everything went well: */
  return 1;
#else
  return inet_aton(name, inp);
#endif
}

/* ML type: string -> int -> addr */
EXTERNML value msocket_newinetaddr(value name, value port) {
  struct sockaddr_in addr;
  value res;
  if (my_aton(String_val(name), &addr.sin_addr)) {
    value sinaddrport = newsinaddrport(addr.sin_addr.s_addr, port);
    res = newaddr(sizeof(struct sockaddr_in), AF_INET, sinaddrport);
  } else
    failwith("Invalid address");

  return res;
}

/* ML type: int -> int -> sock_ */
EXTERNML value msocket_socket(value namespace, value style) {
  int result = socket(Int_val(namespace), Int_val(style), 0);
#ifndef WIN32
  if (result < 0) 
#else
  if( result == INVALID_SOCKET ) 
#endif
    failure();
  return newsocket(result);
}

/* ML type: addr -> string */
EXTERNML value msocket_getinetaddr(value addr) {
  /* Assumes that Nspace_addrval(addr) = AF_INET */
  value sinaddrport = Data_addrval(addr);
  value ml_s_addr = Mlsaddr_sapval(sinaddrport);
  struct in_addr in;
  in.s_addr = (s_addr_t)(Saddr_mlsaddrval(ml_s_addr));
  return copy_string(inet_ntoa(in));
}

/* ML type: sock_ -> sock_ * addr */
EXTERNML value msocket_accept(value sock) {
  int ret;
  union saddr addr;
  value res;

  int len = sizeof(addr);
  enter_blocking_section();
  ret = accept(Sock_val(sock), &addr.sockaddr_gen, &len);
  leave_blocking_section();
#ifndef WIN32
  if (ret == -1) 
#else
  if( ret == INVALID_SOCKET )
#endif
    failure();
  else {
    Push_roots(roots,2);
    roots[0] = from_saddr(&addr, len);
    roots[1] = newsocket(ret);
    res = alloc_tuple(2);
    modify(&Field(res, 0), roots[1]);
    modify(&Field(res, 1), roots[0]);
    Pop_roots();
  }
  return res;
}

/* ML type: sock_ -> addr -> unit */
EXTERNML value msocket_bind(value socket, value address) {
  int ret, size;
  union saddr addr;
  make_saddr(&addr, address);
  size  = Int_val(Size_addrval(address));
  ret = bind(Sock_val(socket), &addr.sockaddr_gen, size);
#ifndef WIN32
  if (ret == -1)
#else
  if( ret == SOCKET_ERROR )
#endif
    failure();
  return Val_unit;
}

/* ML type: sock_ -> addr -> unit */
EXTERNML value msocket_connect(value socket, value address) {
  int ret, size;
  union saddr addr;
 
  make_saddr(&addr, address);
  size  = Int_val(Size_addrval(address));

  /* should enter_blocking_section() be inserted? */
  ret = connect(Sock_val(socket), &addr.sockaddr_gen, size);
#ifndef WIN32
  if (ret == -1) 
#else
  if (ret == SOCKET_ERROR)
#endif
    failure();
  return Val_unit;
}

/* ML type: sock_ -> int -> unit */
EXTERNML value msocket_listen(value sock, value queuelength) {
  int ret;
  ret =listen(Sock_val(sock), Int_val(queuelength));
#ifndef WIN32
  if (ret == -1) 
#else
  if (ret == SOCKET_ERROR)
#endif
    failure();
  return Val_unit;
}

/* ML type: sock_ -> unit */
EXTERNML value msocket_close(value sock) {
#ifndef WIN32
  if (close(Sock_val(sock)) == -1) 
    failwith("msocket: error closing socket");
#else
  if (closesocket(Sock_val(sock)) == SOCKET_ERROR) 
    failwith("msocket: error closing socket");
#endif
  return Val_unit;
}

/* ML type: sock_ -> int -> unit */
EXTERNML value msocket_shutdown(value sock, value how) {
  int ret;
  ret = shutdown(Sock_val(sock), Int_val(how));
#ifndef WIN32
  if (ret == -1) 
#else
  if (ret == SOCKET_ERROR)
#endif
    failure();
  return Val_unit;
}

/* ML type: sock_ -> string -> int -> int -> int -> int */
EXTERNML value msocket_send(value sock, value buff, value offset, value size, 
                            value flags) {
  int ret;

#ifndef WIN32
  /* Ignore SIGPIPE signals; instead send will return -1: */
  signal(SIGPIPE, SIG_IGN);
#endif

  enter_blocking_section();
  ret = send(Sock_val(sock), &Byte(buff, Long_val(offset)), Int_val(size), 
             Int_val(flags));
  leave_blocking_section();
#ifndef WIN32
  if (ret == -1) 
#else
  if (ret == SOCKET_ERROR)
#endif
    failure();
  return Val_int(ret);
}

/* ML type: sock_ -> Word8Vector.vector -> int * int -> int -> addr -> int */
EXTERNML value msocket_sendto(value sock, value buff, value tup, value flags, 
                              value address) {
  int ret;
  union saddr addr;

#ifndef WIN32
  /* Ignore SIGPIPE signals; instead sendto will return -1: */
  signal(SIGPIPE, SIG_IGN);
#endif
 
  make_saddr(&addr, address);
  enter_blocking_section();
  ret = sendto(Sock_val(sock), &Byte(buff, Long_val(Field(tup,0))), 
               Int_val(Field(tup, 1)), Int_val(flags), 
               &addr.sockaddr_gen, Int_val(Size_addrval(address)));
  leave_blocking_section();
#ifndef WIN32
  if (ret == -1) 
#else
  if (ret == SOCKET_ERROR)
#endif
    failure();
  return Val_int(ret);
}

/* ML type: sock_ -> Word8Vector.vector -> int -> int -> int -> int */
EXTERNML value msocket_recv(value sock, value buff, value offset, 
                            value len, value flags) {
  int ret;

  enter_blocking_section();
  ret = recv(Sock_val(sock), &Byte(buff, Long_val(offset)), Int_val(len),
             Int_val(flags));
  leave_blocking_section();
#ifndef WIN32
  if (ret == -1) 
#else
  if (ret == SOCKET_ERROR)
#endif
    failure();
  return Val_int(ret);
}

/* ML type: sock_ -> Word8Vector.vector -> int -> int -> int -> int * addr */
EXTERNML value msocket_recvfrom(value sock, value buff, value offset, 
                                value size, value flags) {
  int ret;
  value res;
  union saddr addr;

  int len = sizeof(addr);

  enter_blocking_section();
  ret = recvfrom(Sock_val(sock), &Byte(buff, Long_val(offset)), 
                 Int_val(size),
                 Int_val(flags), &addr.sockaddr_gen, &len);
  leave_blocking_section();

#ifndef WIN32
  if (ret == -1) 
#else
  if (ret == SOCKET_ERROR)
#endif
    failure();
  else {
    Push_roots(roots, 1);
    roots[0] = from_saddr(&addr, len);
    res = alloc_tuple(2);
    modify(&Field(res, 0), Val_int(len));
    modify(&Field(res, 1), roots[0]);
    Pop_roots();
  }

  return res;
}

/* This makes fd_set a set of the sockets in vector sockv */

static void vec_to_fdset(value sockv, fd_set *fds) {
  int i, vlen = Wosize_val(sockv);

  FD_ZERO(fds);
  for(i = 0; i < vlen; i++) {
    FD_SET(Sock_val(Field(sockv, i)), fds);
  } 
}


#define NILval Atom(0)
#define CONStag 1

/* This returns a list of those elements of vector sockv which are
   also in fd_set fds, in the order in which they appear in sockv.  
   The list is built from the end of sockv towards its head.       */

/* Warning: allocates in the heap, may cause a GC */
/* ML return type: sock list */
static value fdset_to_list(value sockv, fd_set *fds) {
  int i, fd, vlen = Wosize_val(sockv);
  value res;

#define xs ls[0]
#define ys ls[1]
#define sockv_ ls[2]
#define sock_  ls[3]
  Push_roots(ls, 4);
  sockv_ = sockv;
  xs = NILval;
  for (i = vlen-1; i >= 0; i--) {
    sock_ = Field(sockv_, i);
    fd = Sock_val(sock_);
    if (FD_ISSET(fd, fds)) {
      ys = alloc(2, CONStag);
      modify(&Field(ys, 0), sock_);
      modify(&Field(ys, 1), xs);
      xs = ys;
    }
  }
  res = xs;
  Pop_roots();
#undef xs
#undef ys

  return res;
}

/* Warning: allocates in the heap, may cause a GC */
/* ML return type: sock list * sock list * sock list */
EXTERNML value msocket_select(value rsocks, value wsocks, value esocks, 
                     int tsec, int tusec) {
  int ret;
  fd_set rfd, wfd, efd;
  struct timeval timeout, *top;
  value res;

  vec_to_fdset(rsocks, &rfd);
  vec_to_fdset(wsocks, &wfd);
  vec_to_fdset(esocks, &efd);

  if (Int_val(tsec) < 0) {
    top = NULL;
  }
  else {    
    timeout.tv_sec = Int_val(tsec);
    timeout.tv_usec = Int_val(tusec);
    top = &timeout;
  }
  ret = select(FD_SETSIZE, &rfd, &wfd, &efd, top);

#ifndef WIN32
  if (ret == -1) 
#else
  if (ret == SOCKET_ERROR)
#endif
    failure();

  {
    Push_roots(ls, 6);
    ls[3] = rsocks;
    ls[4] = rsocks;
    ls[5] = rsocks;
    ls[0] = fdset_to_list(ls[3], &rfd);
    ls[1] = fdset_to_list(ls[4], &wfd);
    ls[2] = fdset_to_list(ls[5], &efd);
    res = alloc_tuple(3);
    modify(&Field(res, 0), ls[0]);
    modify(&Field(res, 1), ls[1]);
    modify(&Field(res, 2), ls[2]);
    Pop_roots();
  }
  return res;
}


#ifdef WIN32
BOOL WINAPI DllMain(
  HINSTANCE hinstDLL,  // handle to DLL module
  DWORD fdwReason,     // reason for calling function
  LPVOID lpReserved )  // reserved
{
  WSADATA wsadata;
  // Perform actions based on the reason for calling.
  switch( fdwReason ) 
  { 
  case DLL_PROCESS_ATTACH:
    // Initialize once for each new process.
    // Return FALSE to fail DLL load.
    WSAStartup(2, &wsadata);
    break;
    
  case DLL_THREAD_ATTACH:
    // Do thread-specific initialization.
    break;
    
  case DLL_THREAD_DETACH:
    // Do thread-specific cleanup.
    break;
    
  case DLL_PROCESS_DETACH:
    // Perform any necessary cleanup.
    WSACleanup();
    break;
  }
  return TRUE;  // Successful DLL_PROCESS_ATTACH.
}
#endif
