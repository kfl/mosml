/* dynlib.c -- foreign function interface, dynamically loadable libraries */

#include "mlvalues.h"
#include "fail.h"
#if defined(macintosh)
#include <CodeFragments.h>
#include <Errors.h>
#include "os_mac_str.h"
#elif defined(WIN32)
#include "windows.h"
#elif defined(hpux)
#include <dl.h>
#include <errno.h>
#else
#include <dlfcn.h>
#endif

/* Ken Larsen (kla@it.dtu.dk) 1998-01-08 */

/* Doug Currie (e@flavors.com) 1998May06 Macintosh specific changes */

/* Sergei Romanenko 1998 Windows 95/NT specific code */

/* Peter Sestoft (sestoft@dina.kvl.dk) 
   1998 clean-up, documentation, and OSF/1
   1999 HP-UX specific code */

#if defined(hpux)
void hpux_error() {
  switch (errno) {
  case ENOEXEC:
    failwith("Not a shared library, or format error");
  case ENOSYM:
    failwith("Symbol not found in shared library");
  case EINVAL:
    failwith("Invalid shared library handle");
  case ENOMEM:
    failwith("Not enough memory to load shared library");
  case ENOENT:
    failwith("Shared library does not exist");
  case EACCES:
    failwith("Insufficient permissions to load shared library");
  default:
    failwith("Unknown error");
  }
}
#endif

value dynlib_dlopen(value libname, value flagval) /* ML */
{
  void *handle;

#if defined(macintosh)

  #pragma unused(flagval)
  #if __POWERPC__
  #define archType kPowerPCCFragArch
  #else
  #define archType kMotorola68KCFragArch
  #endif
  unsigned int len = string_length( libname );
  unsigned char libName[64];
  Ptr mainAddr;
  Str255 errMessage;
  OSErr err;
  buf_to_p( len > 63 ? 63 : len, String_val( libname ), libName );
  err = GetSharedLibrary( libName, archType, kReferenceCFrag,
                          &(CFragConnectionID )handle, &mainAddr, errMessage );
  if( err != noErr )
  {
    p_to_c( errMessage, (char *)errMessage );
    failwith( (char *)errMessage );
  }
  
#elif defined(WIN32)

  handle = LoadLibrary( String_val(libname) );
  if (!handle)
  {
    char MsgBuf[512];

    FormatMessage(
      FORMAT_MESSAGE_FROM_SYSTEM, NULL,
      GetLastError(),
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
      (LPTSTR) MsgBuf, 512, NULL );

    failwith(MsgBuf);
  }

#elif defined(hpux)

  {
    int mlflags = Long_val(flagval);
    int cflags;
    handle = (void*)(malloc(sizeof(shl_t)));
    if (1 & mlflags) 
      cflags = BIND_IMMEDIATE;
    else
      cflags = BIND_DEFERRED;
    *((shl_t*)handle) = shl_load(String_val(libname), cflags, 0L);
    if (!handle) 
      hpux_error();
  }

#else

  int mlflags = Long_val(flagval);
  int cflags;
  if (1 & mlflags) 
    cflags = RTLD_NOW;
  else
    cflags = RTLD_LAZY;
#ifndef __osf__     
  if (2 & mlflags)
    cflags += RTLD_GLOBAL;
#endif 
  handle = dlopen (String_val(libname), cflags);
  if (!handle) {
    failwith(dlerror());
  }

#endif

  /* Since handle is a void pointer, we can just cast it to value */
  return (value) handle;
}

value dynlib_dlclose(value handle) /* ML */
{
#if defined(macintosh)
  CloseConnection( (CFragConnectionID *)&handle );
#elif defined(WIN32)
  FreeLibrary( (HMODULE) &handle);
#elif defined(hpux)
  shl_unload((shl_t) *((shl_t*)handle));
#else
  dlclose((void *) handle);
#endif
  return Val_unit;
}

value dynlib_dlsym(value handle, value sym) /* ML */
{
  void *symhdl;

#if defined(macintosh)

  Str255 symName;
  unsigned int len = string_length( sym );
  OSErr err;
  CFragSymbolClass symClass;
  buf_to_p( len, String_val( sym ), symName );
  err = FindSymbol( (CFragConnectionID )handle, symName, &symhdl, &symClass );
  switch( err )
  {
    case noErr:
      break;
    case cfragConnectionIDErr:
      failwith( "FindSymbol: The connection ID was not valid." );
      break;
    case cfragNoSymbolErr:
      failwith( "FindSymbol: The specified symbol was not found." );
      break;
    default:
      failwith( "FindSymbol: unknown error" );
      break;
  }
  
#elif defined(WIN32)

  symhdl = GetProcAddress((HMODULE) handle, String_val(sym));
  if( !symhdl )
  {
    char MsgBuf[512];

    FormatMessage(
      FORMAT_MESSAGE_FROM_SYSTEM, NULL,
      GetLastError(),
      MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), /* Default language */
      (LPTSTR) MsgBuf, 512, NULL );

    failwith(MsgBuf);
  }

#elif defined(hpux)

  {
    shl_t myhandle = *((shl_t*)handle);
    int res = shl_findsym(&myhandle, String_val(sym), TYPE_UNDEFINED, &symhdl);
    if (res != 0)
      hpux_error();
  }

#else

  symhdl = dlsym((void *) handle, String_val(sym));
  {   
    char *error = dlerror();
    if (error != NULL)  
      failwith(error);
  }

#endif

  return (value) symhdl;
}
