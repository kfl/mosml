/* unix.c -- Moscow ML C primitives available only under Unix */

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef WIN32
#include <direct.h>
#else
#include <sys/param.h>
#include <sys/utsname.h>
#include <unistd.h>
#endif

#include "mlvalues.h"
#include "fail.h"
#include "memory.h"
#include "str.h"
#include "alloc.h"

value sml_realpath(value v)          /* ML */
{ 
#if defined(ultrix) || defined(__osf__) || defined(WIN32)
  failwith("realpath not supported");
  return Val_unit;
#else
  char buffer[MAXPATHLEN];
  char *result;

  result = realpath(String_val(v), buffer);
  if (result == NULL) 
      failwith("realpath");  
  return copy_string(result);
#endif
}

value sml_uname(value v)          /* ML */
{    
#ifdef WIN32
  failwith("uname");
  return Val_unit;
#else
  struct utsname buf;
  value res;
  Push_roots(r, 1);

  if (uname(&buf) == -1) 
      failwith("uname");  
  res = alloc (3, 0);
  r[0] = res;
  Field (r[0], 0) = copy_string (buf.machine);
  Field (r[0], 1) = copy_string (buf.sysname);
  Field (r[0], 2) = copy_string (buf.release);
  res = r[0];
  Pop_roots();
  return res;
#endif
}

value sml_islink(value path)          /* ML */
{ 
#ifdef WIN32
  failwith("islink");
  return Val_unit;
#else
  struct stat buf;

  if (lstat(String_val(path), &buf) == -1)
      failwith("lstat");
  return Val_bool((S_IFLNK & buf.st_mode) == S_IFLNK);
#endif
}

value sml_readlink(value v)          /* ML */
{ 
#ifdef WIN32
  failwith("readlink");
  return Val_unit;
#else
  char buffer[MAXPATHLEN];
  long result;

  result = readlink(String_val(v), buffer, MAXPATHLEN);
  if (result == -1 || result >= MAXPATHLEN) 
      failwith("readlink");  
  buffer[result] = '\0';
  return copy_string(buffer);
#endif
}

value sml_devinode(value path)          /* ML */
{ 
#ifdef WIN32
  failwith("devinode");
  return Val_unit;
#else
  struct stat buf;
  double dev_inode;

  if (stat(String_val(path), &buf) == -1)
      failwith("stat");
  /* Combine the device and inode into a real number */
  dev_inode = ((double)buf.st_dev * (1 << 17)) + (double)buf.st_ino;
  return copy_double(dev_inode);
#endif
}

value sml_setdisk(value volno)        /* ML */
{
#ifdef WIN32
  _chdrive(Long_val(volno) + 1);
  return Val_unit;
#else
  failwith("setdisk");
  return Val_unit;		/* Can't reach return */
#endif
}
