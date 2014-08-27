/* Moscow ML primitives */

#include <math.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <time.h>
#include <ctype.h>

#ifdef WIN32
#include <sys/timeb.h>
#include <sys/utime.h>
#include <io.h>
#include <direct.h>
#include <windows.h>
#else
#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>
#include <dirent.h>
#include <sys/param.h> 
#include <unistd.h>
#include <utime.h>
#endif

#include "mlvalues.h"
#include "fail.h"
#include "memory.h"
#include "str.h"
#include "runtime.h"
#include "alloc.h"
#include "major_gc.h"
#include "intext.h"
#include "debugger.h"
#include "interp.h"
#include "globals.h"
#include "mosml.h"

/* SunOS 4 appears not to have mktime: */
#if defined(sun) && !defined(__svr4__)
#define tm2cal(tptr)	timelocal(tptr)
#else
#define tm2cal(tptr)	mktime(tptr)
#endif

#define Raise_float_if(cond) \
   if( cond ) \
      { raiseprimitive0(float_exn); }

#define Check_float(dval) \
   Raise_float_if( (dval > maxdouble) || (dval < -maxdouble) )

/* Structural equality on trees. */
/* Note how reference cells are treated! */

static int sml_equal_aux(value v1, value v2)
{
  mlsize_t i;
  value * p1, * p2;

 again:
  if (v1 == v2) return 1;
  if (Is_long(v1) || Is_long(v2)) return 0;
  if (!Is_in_heap(v1) && !Is_young(v1)) return 0;
  if (!Is_in_heap(v2) && !Is_young(v2)) return 0;
  if (Tag_val(v1) != Tag_val(v2)) return 0;
  switch(Tag_val(v1)) {
  case String_tag:
    { // Faster string comparison 2002-12-03
      register int len = string_length(v1);
      register unsigned char * p1, * p2;
      if (len != string_length(v2))
	return 0;
      for (p1 = (unsigned char *) String_val(v1),
	   p2 = (unsigned char *) String_val(v2);
	   len > 0;
	   len--, p1++, p2++)
	if (*p1 != *p2)
	  return 0;
      return 1;
    }
  case Double_tag:
    return (Double_val(v1) == Double_val(v2));
  case Reference_tag:  /* Different reference cells are not equal! */
  case Abstract_tag:
  case Final_tag:
    return 0;
  case Closure_tag:
    invalid_argument("sml_equal: functional value");
  default:
    i = Wosize_val(v1);
    if (i != Wosize_val(v2)) return 0;
    for(p1 = Op_val(v1), p2 = Op_val(v2);
        i > 1;
        i--, p1++, p2++)
      if (!sml_equal_aux(*p1, *p2)) return 0;
    v1 = *p1;
    v2 = *p2;                   /* Tail-call */
    goto again;
  }
}

value sml_equal(value v1, value v2) /* ML */
{
  return Atom(sml_equal_aux(v1,v2));
}

value sml_not_equal(value v1, value v2) /* ML */
{
  return Atom(!sml_equal_aux(v1,v2));
}

value sml_system(value cmd)        /* ML */
{
  value res;
  errno = 0;
  res = system(String_val(cmd));
  if (errno == ENOENT)
    return -1;
  else
    return Val_int(res);
}

value sml_abs_int(value x)          /* ML */
{ value tmp, v;
  tmp = Long_val(x);
  if( tmp < 0 ) tmp = -tmp;
  v = Val_long(tmp);
  if( Long_val(v) != tmp )
    raise_overflow();
  return v;
}

value sml_floor(value f)              /* ML */
{ double r;
  long i;
  value v;
  r = Double_val(f);
  if( r >= 0.0 )
    { if( r >= ((double) Max_long + 1) ) goto raise_floor;
      i = (long) r;
    }
  else
    { 
      if( r < ((double) Min_long) ) goto raise_floor;
      i = (long) r;
      if( r < ((double) i) ) i -= 1;
    }
  v = Val_long(i);
  if( Long_val(v) != i )  goto raise_floor;
  return v;

raise_floor:
    raise_overflow();
    return Val_unit;		/* Can't reach return */
}

value sml_ceil(value f)              /* ML */
{ double r;
  long i;
  value v;
  r = Double_val(f);
  if( r >= 0.0 )
    { if( r > ((double) (Max_long)) ) goto raise_ceil;
      i = (long) r;
      if( r > ((double) i) ) i += 1;
    }
  else
    { if( r <= ((double) (Min_long-1)) ) goto raise_ceil;
      i = (long) r;
    }
  v = Val_long(i);
  if( Long_val(v) != i )  goto raise_ceil;
  return v;

raise_ceil:
    raise_overflow();
    return Val_unit;		/* Can't reach return */
}

#ifdef __MWERKS__
#if __MWERKS__ < 0x0400
#include <Types.h>
double_t nearbyint ( double_t x );
#define rint nearbyint
#endif
#endif

value sml_round(value f)              /* ML */
{ double r;
  long i;
  value v;
  /* Apparently no rint() in djgpp's libm: */
#if defined(MSDOS) || defined(hpux) || defined(WIN32)
  double delta;
  r = floor(Double_val(f));
  if (r < (double)(Min_long-1) || r > (double)(Max_long)) goto raise_round;  
  i = (long)r;
  delta = Double_val(f) - r;   // belongs to [0, 1[
  // Round to nearest even integer.
  // If delta > 0.5, round up; if delta == 0.5, round to nearest even:
  if (delta > 0.5 || delta == 0.5 && i % 2 != 0)
    i++;
  v = Val_long(i); 
  if( Long_val(v) != i )  goto raise_round; 
#else
  r = rint(Double_val(f));
  if ((r > (double) (Max_long)) || (r < (double)(Min_long))) goto raise_round;
  i = (long) r;
  v = Val_long(i);
#endif

  return v;

raise_round:
    raise_overflow();
    return Val_unit;		/* Can't reach return */
}

value sml_trunc(value f)              /* ML */
{ double r;
  long i;
  value v;
  r = Double_val(f);
  if ((r >= (double) (Max_long+1)) || (r <= (double)(Min_long-1))) 
    goto raise_trunc;
  i = (long) r;
  v = Val_long(i);
  return v;

raise_trunc:
  raise_overflow();
  return Val_unit;		/* Can't reach return */
}

value sml_abs_real(value f)              /* ML */
{ double r;
  float_exn = SYS__EXN_OVERFLOW;
  r = Double_val(f);
  if( r >= 0.0 )
    return f;
  else
    r = -r;
  Check_float(r);
  return copy_double(r);
}

value sml_sqrt(value f)         /* ML */
{ double r;
  float_exn = SYS__EXN_DOMAIN;
  r = Double_val(f);
  Raise_float_if( r < 0.0 );
  r = sqrt(r);
  Check_float(r);
  return copy_double(r);
}

value sml_sin(value f)         /* ML */
{ double r;
  r = Double_val(f);
  r = sin(r);
  if( r != r || r > 1.0 || r < -1.0 )
    failwith("sin: argument too large");
  return copy_double(r);
}

value sml_cos(value f)         /* ML */
{ double r;
  r = Double_val(f);
  r = cos(r);
  if( r != r || r > 1.0 || r < -1.0 )
    failwith("cos: argument too large");
  return copy_double(r);
}

value sml_exp(value f)           /* ML */
{ double r;
  float_exn = SYS__EXN_OVERFLOW;
  r = exp(Double_val(f));
  Check_float(r);
  return copy_double(r);
}

value sml_ln(value f)           /* ML */
{ double r;
  float_exn = SYS__EXN_DOMAIN;
  r = Double_val(f);
  Raise_float_if( r <= 0.0 );
  r = log(r);
  Check_float(r);
  return copy_double(r);
}

unsigned long scandec(char * p, unsigned long max)
{ unsigned long res;
  int c, d;
  res = 0;
  while (1) {
    c = *p;
    if (c >= '0' && c <= '9')
      d = c - '0';
    else
      break;
    if( (res > (max/10)) ||
        ((res == (max/10) && ((max % 10) <= d))) )
      goto raise_failure;
    res = 10 * res + d;
    p++;
  }
  if (*p != 0)
    goto raise_failure;
  return res;

  raise_failure:
    failwith("scandec");
    return 0;		/* Can't reach return */
}

unsigned long scanhex(char * p, unsigned long max)
{ unsigned long res;
  int c, d;
  res = 0;
  while (1) {
    c = toupper(*p);
    if (c >= '0' && c <= '9')
      d = c - '0';
    else if (c >= 'A' && c <= 'F')
      d = c + (10 - 'A');
    else
      break;
    if( (res > (max/16)) ||
        ((res == (max/16) && ((max % 16) <= d))) )
      goto raise_failure;
    res = 16 * res + d;
    p++;
  }
  if (*p != 0)
    goto raise_failure;
  return res;

  raise_failure:
    failwith("scanhex");
    return 0;		/* Can't reach return */
}

value sml_int_of_string(value s)          /* ML */
{ value v;
  long res;
  int sign;
  char * p;

  p = String_val(s);
  sign = 1;
  if (*p == '~') {
    sign = -1;
    p++;
  }
  res = sign * scandec(p, (unsigned long)Min_long);
  v = Val_long(res);
  if( Long_val(v) != res )
    goto raise_failure;
  return v;

  raise_failure:
    failwith("sml_int_of_string");
    return Val_unit;		/* Can't reach return */
}

value sml_concat(value s1, value s2)        /* ML */
{
  mlsize_t len1, len2, len;
  value s;
  len1 = string_length(s1);
  if (len1 == 0) 
    return s2;
  len2 = string_length(s2);
  if (len2 == 0)
    return s1;
  { 
    Push_roots(r, 2);
    r[0] = s1;
    r[1] = s2;
    len = len1 + len2;
    if( (len + sizeof (value)) / sizeof (value) > Max_wosize )
      raiseprimitive0(SYS__EXN_SIZE); 
    s = alloc_string(len);
    bcopy(&Byte(r[0],0), &Byte(s,0), len1);
    bcopy(&Byte(r[1],0), &Byte(s,len1), len2);
    Pop_roots();
    return s;
  }
}

value sml_chr(value v)          /* ML */
{
  long i;
  value s;
  i = Long_val(v);
  if( i < 0 || i > 255 )
    raiseprimitive0(SYS__EXN_CHR);
  s = alloc_string(1);
  *(&Byte(s,0)) = (unsigned char) i;
  return s;
}

value sml_ord(value s)          /* ML */
{
  long i;
  if( string_length(s) == 0 )
    raiseprimitive0(SYS__EXN_ORD);
  i = (unsigned char) *(&Byte(s,0));
  return Val_long(i);
}

value sml_float_of_string(value s)        /* ML */
{
  char buff[64];
  mlsize_t len;
  int i, e_len;
  char c;
  char *p;
  double r;

  len = string_length(s);
  if(len > sizeof(buff) - 1)
    failwith("sml_float_of_string: argument too large");
  p = String_val(s);
  e_len = -1;
  for (i = 0; i<len; i++) {
    c = *p++;
    switch( c ) {
        case '~':
          buff[i] = '-'; break;
        case 'E':
          buff[i] = 'e'; e_len = 0; break;
        default:
          buff[i] = c;
          if( e_len >= 0 ) e_len++;
          Raise_float_if( e_len > 5 )
          break;
    }
  }
  buff[len] = 0;
  r = atof(buff);
  if( (r > maxdouble) || (r < -maxdouble) )
    failwith("sml_float_of_string: result too large");
  return copy_double(r);
}

static int countChar(int c, char * s)
{
  char *p; int count;

  count = 0;
  for( p=s; *p != '\0'; p++ ) {
    if( *p == c ) count++;
  }
  return count;
}

/* Here we remove all '+', and replace '-' and 'e' with '~' and 'E'.
   Also, drop a single leading zero from the exponent. */

static void mkSMLMinus(char * s)
{
  char *p = s, *q = s;
  int justafterexp = 0;		/* After exponent but before digits */
  
  for ( ; *p != '\0'; p++) {
    switch( *p ) {
    case '+': break;
    case '-': *q++ = '~'; break;
    case 'e': *q++ = 'E'; justafterexp = 1; break;
    case '0': 
      if (!justafterexp)	/* Don't copy zero just after exponent */
	*q++ = '0';
      justafterexp = 0;
      break;
    default: *q++ = *p; justafterexp = 0; break;
    }
  }
  *q = '\0';
  return;
}

value sml_string_of_int(value arg)      /* ML */
{
  char format_buffer[32];

  sprintf(format_buffer, "%ld", Long_val(arg));
  mkSMLMinus(format_buffer);
  return copy_string(format_buffer);
}

/* Convert real x to SMLish format in format_buffer */

void string_of_float_aux(char* format_buffer, double x)
{
  sprintf(format_buffer, "%.12g", x);
  mkSMLMinus(format_buffer);
  if( countChar('.', format_buffer) == 0 &&
      countChar('E', format_buffer) == 0 )
    strcat(format_buffer, ".0");
}

value sml_string_of_float(value arg)    /* ML */
{
  char format_buffer[64];
  string_of_float_aux(format_buffer, Double_val(arg));
  return copy_string(format_buffer);
}

#ifdef __MWERKS__
#pragma mpwc_newline on
#endif

value sml_makestring_of_char(value arg)      /* ML */
{
  unsigned char c;
  char buff[8];

  c = Int_val(arg);
  switch (c)
    {
    case '"':   return copy_string("#\"\\\"\"");
    case '\\':  return copy_string("#\"\\\\\"");
    case '\a':  return copy_string("#\"\\a\"");
    case '\b':  return copy_string("#\"\\b\"");
    case '\t':  return copy_string("#\"\\t\"");
    case '\n':  return copy_string("#\"\\n\"");
    case '\v':  return copy_string("#\"\\v\"");
    case '\f':  return copy_string("#\"\\f\"");
    case '\r':  return copy_string("#\"\\r\"");
    default:
      buff[0] = '#'; buff[1] = '"';
      if( c <= 31 ) {
        buff[2] = '\\'; buff[3] = '^'; buff[4] = c + 64;
        buff[5] = '"'; buff[6] = 0;
        return copy_string(buff);
        }
      else if( (32 <= c && c <= 126) || (128 <= c && c <= 254) ) {
        buff[2] = c; buff[3] = '"'; buff[4] = 0;
        return copy_string(buff);
        }
      else {
        buff[2] = '\\';
        buff[3] = 48 + c / 100;
        buff[4] = 48 + (c / 10) % 10;
        buff[5] = 48 + c % 10;
        buff[6] = '"';
        buff[7] = 0;
        return copy_string(buff);
        }
    }
}

value sml_makestring_of_string(value arg)      /* ML */
{
  mlsize_t arg_len, len, i;
  value res;
  char *a; char *b;
  unsigned char c;
  Push_roots(r, 1);

  r[0] = arg;
  arg_len = string_length(arg);

  a = String_val(r[0]);
  len = 0;
  for( i = 0; i < arg_len; i++ ) {
    c = a[i];
    switch (c)
      {
      case '"': case '\\': 
      case '\a': case '\b': case '\t': case '\n': case '\v': 
      case '\f': case '\r':
        len += 2; break;
      default:
        if( c <= 31)
          len += 3;
        else if( (32 <= c && c <= 126) || (128 <= c && c <= 254) )
          len += 1;
        else
          len += 4;
        break;
      }
    }

  if( (len + 2 + sizeof (value)) / sizeof (value) > Max_wosize )
    failwith("sml_string_for_read: result too large");
  res = alloc_string(len + 2);

  a = String_val(r[0]);
  b = String_val(res);
  *b++ = '"';
  for( i = 0; i < arg_len; i++) {
    c = a[i];
    switch (c)
      {
      case '"':   *b++ = '\\'; *b++ = '"';  break;
      case '\\':  *b++ = '\\'; *b++ = '\\'; break;
      case '\a':  *b++ = '\\'; *b++ = 'a';  break;
      case '\b':  *b++ = '\\'; *b++ = 'b';  break;
      case '\t':  *b++ = '\\'; *b++ = 't';  break;
      case '\n':  *b++ = '\\'; *b++ = 'n';  break;
      case '\v':  *b++ = '\\'; *b++ = 'v';  break;
      case '\f':  *b++ = '\\'; *b++ = 'f';  break;
      case '\r':  *b++ = '\\'; *b++ = 'r';  break;
      default:
        if( c <= 31 )
          { *b++ = '\\'; *b++ = '^'; *b++ = c + 64; break; }
        else if( (32 <= c && c <= 126) || (128 <= c && c <= 254) )
          { *b++ = c; break; }
        else
          { *b++ = '\\';
            *b++ = 48 + c / 100;
            *b++ = 48 + (c / 10) % 10;
            *b++ = 48 + c % 10;
            break; }
      }
    }
  *b++ = '"';
  Pop_roots();
  return res;
}

#ifdef __MWERKS__
#pragma mpwc_newline off
#endif

/* There is another problem on the Mac: with a time base of 1904,
   most times are simply out of range of mosml integers. So, I added
   the macros below to compensate. 07Sep95 e
*/

#ifndef macintosh

#define SYStoSMLtime
#define SMLtoSYStime

#endif

/* Return time as (double) number of usec since the epoch */

value sml_getrealtime (value v) /* ML */
{
#ifdef WIN32
  value res;
  struct timeb t;

  /*
  // It seems that the time returned by 'ftime' under MS Windows
  // disagree with that returned by 'gettimeofday' under MS DOS!
  // The following lines are written according the specification
  // of 'ftime' though...
  // Experiments show, that in Moscow the result returned by
  // 'ftime' is recalculated into the correct local time, while
  // the time calculated from the result of 'gettimeofday' is
  // 1 hour late.
  //  Sergei Romanenko
  */

  ftime(&t);
  return copy_double(t.time*1000000.0 + t.millitm*1000.0);
#else
  struct timeval tp;

  gettimeofday(&tp, NULL);
  return copy_double((SYStoSMLtime(tp.tv_sec))*1000000.0 + (double)tp.tv_usec);
#endif
}

value sml_getrutime (value v) /* ML */
{
  value res;

#if defined(__MWERKS__)
  res = e_getrusage();
#else
#ifdef WIN32
  /*
  // Here I return sysTime = usrTime.
  // Perhaps, win32 enables sysTime and usrTime to be mesured
  // in an accurate way...
  //  Sergei Romanenko
  */
  struct timeb t;
  ftime(&t);
  res = alloc (6, 0);
  Field (res, 2) = Val_long (t.time);
  Field (res, 3) = Val_long (((long) t.millitm) * 1000);
  Field (res, 4) = Val_long (t.time);
  Field (res, 5) = Val_long (((long) t.millitm) * 1000);
#elif defined(hpux) || defined(__svr4__)
  struct tms buffer;

  long persec = sysconf(_SC_CLK_TCK);
  times(&buffer);
  res = alloc (6, 0);
  Field (res, 2) = Val_long (buffer.tms_stime / persec);
  Field (res, 3) = Val_long ((buffer.tms_stime % persec) * (1000000 / persec));
  Field (res, 4) = Val_long (buffer.tms_utime / persec);
  Field (res, 5) = Val_long ((buffer.tms_utime % persec) * (1000000 / persec));
#else
  struct rusage rusages;
  getrusage(RUSAGE_SELF, &rusages);
  res = alloc (6, 0);
  Field (res, 2) = Val_long (rusages.ru_stime.tv_sec);
  Field (res, 3) = Val_long (rusages.ru_stime.tv_usec);
  Field (res, 4) = Val_long (rusages.ru_utime.tv_sec); 
  Field (res, 5) = Val_long (rusages.ru_utime.tv_usec);
#endif

  Field (res, 0) = Val_long (gc_time.tv_sec);
  Field (res, 1) = Val_long (gc_time.tv_usec); 
#endif

  return res;
}


value sml_errno(value arg)          /* ML */
{
  return Val_long(errno);
}

value sml_getdir(value arg)		/* ML */
{
#ifdef WIN32
  char directory[_MAX_PATH];
  char *res;

  errno = 0;
  /* Unlike Unix and DJ GPP, the path is returned with the drive letter, */
  /* and with '\', rather then '/'! */
  res = getcwd(directory, _MAX_PATH);
  if (res == NULL)
     failwith("getcwd");
  for( ; *res; res++ )
    if( *res == '\\' ) 
      *res = '/';
  return copy_string(directory);
#else
 char directory[MAXPATHLEN];
 char *res;

 errno = 0;
 res = getcwd(directory, MAXPATHLEN);
 if (res == NULL)
    failwith("getcwd");
 return copy_string(directory);
#endif
}

value sml_mkdir(value path)          /* ML */
{
#ifdef WIN32
  /* Unlike Unix and DJ GPP, the path may contain a drive letter, */
  /* and must contain '\' rather than '/'. */
  if (mkdir(String_val(path)) == -1)
      failwith("mkdir");
  return Val_unit;
#else
  if (mkdir(String_val(path), 0777) == -1) 
      failwith("mkdir");
  return Val_unit;
#endif
}

value sml_rmdir(value path)          /* ML */
{
  if (rmdir(String_val(path)) == -1) 
      failwith("rmdir");
  return Val_unit;
}

#ifdef WIN32
typedef struct
{
  WIN32_FIND_DATA FileData;
  char szSearchPath[MAX_PATH];
  HANDLE hSearch;
  BOOL fFinished;
  char d_name[MAX_PATH];
} MY_DIR;

MY_DIR *my_opendir(const char* dirname)
{
  MY_DIR *dstr;

  dstr = malloc(sizeof(MY_DIR));
  if( dstr == NULL ) return NULL;
  memset(dstr, 0, sizeof(MY_DIR));

  strncpy(dstr->szSearchPath, dirname, MAX_PATH);
  strncat(dstr->szSearchPath, "\\*.*", MAX_PATH);
  dstr->szSearchPath[MAX_PATH-1] = '\0';

  dstr->hSearch = FindFirstFile(dstr->szSearchPath, &dstr->FileData); 
  if (dstr->hSearch == INVALID_HANDLE_VALUE) 
  { 
    free(dstr);
    return NULL;
  } 
  dstr->fFinished = FALSE;
  return dstr;
}

void my_readdir(MY_DIR *dstr)
{
  if( dstr->fFinished )
    dstr->d_name[0] = '\0';
  else
  {
    strncpy(dstr->d_name, dstr->FileData.cFileName, MAX_PATH);
    if (!FindNextFile(dstr->hSearch, &dstr->FileData))
    {
      dstr->fFinished = TRUE;
      FindClose(dstr->hSearch);
    }
  }
}

void my_closedir(MY_DIR *dstr)
{
  if( !dstr->fFinished )
    FindClose(dstr->hSearch);
  free(dstr);
}

BOOL my_rewinddir(MY_DIR *dstr)
{
  if( !dstr->fFinished )
    FindClose(dstr->hSearch);

  dstr->hSearch = FindFirstFile(dstr->szSearchPath, &dstr->FileData); 
  if (dstr->hSearch == INVALID_HANDLE_VALUE) 
  { 
    free(dstr);
    return FALSE;
  } 
  dstr->fFinished = FALSE;
  return TRUE;
}
#endif

value sml_opendir(value path)          /* ML */
{
#ifdef WIN32
  MY_DIR *dstr;

  dstr = my_opendir(String_val(path));
  if (dstr == NULL)
      failwith("opendir");
  return (value) dstr;
#else
  DIR * dstr;

  dstr = opendir(String_val(path));
  if (dstr == NULL)
      failwith("opendir");
#ifdef MSDOS
  if (readdir(dstr) == NULL) 
      failwith("opendir");
  else
      rewinddir(dstr);
#endif
  return (value) dstr;
#endif
}

value sml_rewinddir(value v)          /* ML */
{ 
#ifdef WIN32
  if( !my_rewinddir((MY_DIR *) v) )
    failwith("opendir");
  return Val_unit;
#else
  rewinddir((DIR *) v);
  return Val_unit;
#endif
}

value sml_readdir(value v)          /* ML */
{ 
#ifdef WIN32
  MY_DIR *dstr;

  dstr = (MY_DIR *) v;
  my_readdir(dstr);
  if( dstr->d_name[0] == '\0' )
    return copy_string("");
  return copy_string(dstr->d_name);
#else
  struct dirent *direntry;

  direntry = readdir((DIR *) v);
  if (direntry == NULL) 
      return copy_string("");
  return copy_string((*direntry).d_name);
#endif
}

value sml_closedir(value v)          /* ML */
{ 
#ifdef WIN32
  my_closedir((MY_DIR *) v);
  return Val_unit;
#else
  if (closedir((DIR *) v) == -1)
      failwith("closedir");
  return Val_unit;
#endif
}

value sml_isdir(value path)          /* ML */
{ 
#ifdef WIN32
  DWORD dwFileAttributes = GetFileAttributes( String_val(path) );
  if( dwFileAttributes == 0xFFFFFFFF )
    failwith("isdir");
  return (Val_bool(dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY));
#else
  struct stat buf;

  if (stat(String_val(path), &buf) == -1)
      failwith("stat");
  return (Val_bool(S_ISDIR(buf.st_mode)));
#endif
}

value sml_modtime(value path)          /* ML */
{ struct stat buf;

  if (stat(String_val(path), &buf) == -1)
      failwith("stat");
  return (copy_double ((double) (SYStoSMLtime(buf.st_mtime))));
}

value sml_settime(value path, value time)          /* ML */
{ 
  struct utimbuf tbuf;

  tbuf.actime = tbuf.modtime = SMLtoSYStime((long) (Double_val(time)));
  if (utime(String_val(path), &tbuf) == -1)
      failwith("utime");
  return Val_unit;
}

#ifdef WIN32
#define F_OK            0       /* does file exist */
#define	X_OK		1	/* is it executable by caller */
#define	W_OK		2	/* is it writable by caller */
#define	R_OK		4	/* is it readable by caller */
#endif

value sml_access(value path, value permarg)          /* ML */
{ 
  long perms;
  long perm = Long_val(permarg);

  perms  = ((0x1 & perm) ? R_OK : 0);
  perms |= ((0x2 & perm) ? W_OK : 0);
  perms |= ((0x4 & perm) ? X_OK : 0);
  if (perms == 0) perms = F_OK;

  if (access(String_val(path), perms) == 0)
    return Val_bool(1);
  return Val_bool(0);
}

#ifndef HAS_STRERROR
#if (!defined(__FreeBSD__) && !defined(linux) && !defined(__APPLE__))
  extern int sys_nerr;
  extern char * sys_errlist [];
#endif   
  extern char *realpath();
#endif

value sml_tmpnam(value v)          /* ML */
{ char *res;
#ifdef WIN32
  value value_res;
 
  res = _tempnam(NULL, "mosml");
  if (res == NULL)
    failwith("tmpnam");
  value_res = copy_string(res);
  free(res);
  return value_res;
#else
  res = tmpnam(NULL);
  if (res == NULL) 
    failwith("tmpnam");  
  return copy_string(res);
#endif
}

value sml_errormsg(value err)   /* ML */
{
  int errnum;
  errnum = Long_val(err);
#ifdef HAS_STRERROR
  return copy_string(strerror(errnum));
#else
  if (errnum < 0 || errnum >= sys_nerr) 
      return copy_string("(Unknown error)");
  else 
    return copy_string((char *)sys_errlist[errnum]);
#endif
}

value sml_asin(value f)           /* ML */
{ double r = Double_val(f);
  float_exn = SYS__EXN_DOMAIN;
  Raise_float_if( r < -1.0 || r > 1.0 );  
  r = asin(r);
  Raise_float_if( r != r );
  return copy_double(r);
}

value sml_acos(value f)           /* ML */
{ double r = Double_val(f);
  float_exn = SYS__EXN_DOMAIN;
  Raise_float_if( r < -1.0 || r > 1.0 );  
  r = acos(r);
  Raise_float_if( r != r );
  return copy_double(r);
}

value sml_atan2(value f1, value f2)           /* ML */
{ double r, r1, r2;
  float_exn = SYS__EXN_DOMAIN;
  r1 = Double_val(f1);
  r2 = Double_val(f2);
  if (r1 == 0.0 && r2 == 0.0) 
    return copy_double(0.0);
  r = atan2(r1, r2);
  Check_float(r);
  Raise_float_if( r != r );
  return copy_double(r);
}

value sml_pow(value f1, value f2)           /* ML */
{ double r, r1, r2;
  float_exn = SYS__EXN_DOMAIN;
  r1 = Double_val(f1);
  r2 = Double_val(f2);
  if (r1 == 0.0 && r2 == 0.0) 
    return copy_double(1.0);
  if (   (r1 == 0.0 && r2 < 0.0) 
      || (r1 < 0.0 && (   fabs(r2) > (double) (Max_long) 
		       || r2 != (double)(long)r2)))
    raiseprimitive0(float_exn);
  r = pow(r1, r2);
  float_exn = SYS__EXN_OVERFLOW;
  Check_float(r);
  float_exn = SYS__EXN_DOMAIN;
  Raise_float_if( r != r );
  return copy_double(r);
}

value sml_localtime (value v) /* ML */
{
  value res;
  struct tm *tmr;
  time_t clock = SMLtoSYStime((long) (Double_val(v)));
  tmr = localtime(&clock);
  res = alloc (9, 0);
  Field (res, 0) = Val_long ((*tmr).tm_hour);
  Field (res, 1) = Val_long ((*tmr).tm_isdst);
  Field (res, 2) = Val_long ((*tmr).tm_mday);
  Field (res, 3) = Val_long ((*tmr).tm_min); 
  Field (res, 4) = Val_long ((*tmr).tm_mon);
  Field (res, 5) = Val_long ((*tmr).tm_sec);
  Field (res, 6) = Val_long ((*tmr).tm_wday);
  Field (res, 7) = Val_long ((*tmr).tm_yday);
  Field (res, 8) = Val_long ((*tmr).tm_year);

  return res;
}

value sml_gmtime (value v) /* ML */
{
  value res;
  struct tm *tmr;
  time_t clock = SMLtoSYStime((long) (Double_val(v)));
  tmr = gmtime(&clock);
  res = alloc (9, 0);
  Field (res, 0) = Val_long ((*tmr).tm_hour);
  Field (res, 1) = Val_long ((*tmr).tm_isdst);
  Field (res, 2) = Val_long ((*tmr).tm_mday);
  Field (res, 3) = Val_long ((*tmr).tm_min); 
  Field (res, 4) = Val_long ((*tmr).tm_mon);
  Field (res, 5) = Val_long ((*tmr).tm_sec);
  Field (res, 6) = Val_long ((*tmr).tm_wday);
  Field (res, 7) = Val_long ((*tmr).tm_yday);
  Field (res, 8) = Val_long ((*tmr).tm_year);
  return res;
}

value sml_mktime (value v) /* ML */
{
  struct tm tmr = {0};

  tmr.tm_hour  = Long_val(Field (v, 0));
  tmr.tm_isdst = Long_val(Field (v, 1));
  tmr.tm_mday  = Long_val(Field (v, 2));
  tmr.tm_min   = Long_val(Field (v, 3));
  tmr.tm_mon   = Long_val(Field (v, 4));
  tmr.tm_sec   = Long_val(Field (v, 5));
  tmr.tm_wday  = Long_val(Field (v, 6));
  tmr.tm_yday  = Long_val(Field (v, 7));
  tmr.tm_year  = Long_val(Field (v, 8));

  return copy_double((double)SYStoSMLtime(tm2cal(&tmr)));
}

value sml_asctime (value v) /* ML */
{
  struct tm tmr = {0};
  char *res;

  tmr.tm_hour  = Long_val(Field (v, 0));
  tmr.tm_isdst = Long_val(Field (v, 1));
  tmr.tm_mday  = Long_val(Field (v, 2));
  tmr.tm_min   = Long_val(Field (v, 3));
  tmr.tm_mon   = Long_val(Field (v, 4));
  tmr.tm_sec   = Long_val(Field (v, 5));
  tmr.tm_wday  = Long_val(Field (v, 6));
  tmr.tm_yday  = Long_val(Field (v, 7));
  tmr.tm_year  = Long_val(Field (v, 8));

  res = asctime(&tmr);
  if (res == NULL) 
    failwith("asctime");
  return copy_string(res);
}

value sml_strftime (value fmt, value v) /* ML */
{
  struct tm tmr = {0};
#define BUFSIZE 256      
  char buf[BUFSIZE];
  long ressize;

  tmr.tm_hour  = Long_val(Field (v, 0));
  tmr.tm_isdst = Long_val(Field (v, 1));
  tmr.tm_mday  = Long_val(Field (v, 2));
  tmr.tm_min   = Long_val(Field (v, 3));
  tmr.tm_mon   = Long_val(Field (v, 4));
  tmr.tm_sec   = Long_val(Field (v, 5));
  tmr.tm_wday  = Long_val(Field (v, 6));
  tmr.tm_yday  = Long_val(Field (v, 7));
  tmr.tm_year  = Long_val(Field (v, 8));

  ressize = strftime(buf, BUFSIZE, String_val(fmt), &tmr);
  if (ressize == 0 || ressize == BUFSIZE) 
    failwith("strftime");
  return copy_string(buf);
#undef BUFSIZE
}

value sml_general_string_of_float(value fmt, value arg)    /* ML */
{
#define BUFSIZE 512
  char format_buffer[BUFSIZE];

  /* Unfortunately there seems to be no way to ensure that this does not
   * crash by overflowing the format_buffer (e.g. when specifying a huge 
   * number of decimal digits in the fixed-point format).  Well, we might
   * use snprintf if universally supported?
   */

  double x = Double_val(arg);
  if (x == -0.0) x = 0.0;
  sprintf(format_buffer, String_val(fmt), x);

  mkSMLMinus(format_buffer);
  return copy_string(format_buffer);
#undef BUFSIZE
}

value sml_filesize(value path)          /* ML */
{ struct stat buf;

  if (stat(String_val(path), &buf) == -1)
      failwith("stat");
  return (Val_long (buf.st_size));
}

value sml_int_of_hex(value s)          /* ML */
{ value v;
  long res;
  int sign;
  char * p;

  /* The argument s has form [~]?0x[0-9a-fA-F]+ */

  p = String_val(s);
  sign = 1;
  if (*p == '~') {
    sign = -1;
    p++;
  }
  /* skip 0x in s */
  p += 2; 

  res = sign * scanhex(p, (unsigned long)Min_long);
  v = Val_long(res);
  if( Long_val(v) != res )
    goto raise_failure;
  return v;

  raise_failure:
    failwith("sml_int_of_hex");
    return Val_unit;		/* Can't reach return */
}

value sml_word_of_hex(value s)          /* ML */
{ value v;
  long res;
  char * p;

  /* The argument s has form 0wx[0-9a-fA-F]+ */

  p = String_val(s);
  /* skip 0wx in s */
  p += 3; 

  res = scanhex(p, 2 * (unsigned long)Min_long);
  v = Val_long((long)res);
  return v;
}

value sml_word_of_dec(value s)          /* ML */
{ value v;
  long res;
  char * p;

  /* The argument s has form 0w[0-9]+ */
  p = String_val(s);
  /* skip 0w in s */
  p += 2; 

  res = (long)scandec(p, 2 * (unsigned long)Min_long);
  v = Val_long((long)res);
  return v;
}

value sml_hexstring_of_word(value arg)      /* ML */
{
  char format_buffer[32];

  sprintf(format_buffer, "0wx%lX", Long_val((unsigned long)arg));
  return copy_string(format_buffer);
}

value sml_sinh(value f)         /* ML */
{ double r;
  float_exn = SYS__EXN_OVERFLOW;  
  r = Double_val(f);
  r = sinh(r);
  Check_float(r);
  return copy_double(r);
}

value sml_cosh(value f)         /* ML */
{ double r;
  float_exn = SYS__EXN_OVERFLOW;
  r = Double_val(f);
  r = cosh(r);
  Check_float(r);
  return copy_double(r);
}

value sml_tanh(value f)         /* ML */
{ double r;
  float_exn = SYS__EXN_DOMAIN;
  r = Double_val(f);
  r = tanh(r);
  Check_float(r);
  return copy_double(r);
}

/* A weak pointer v is dead (dangling) if NULL, or if we are in the
   weak phase and v is a white block in the heap.

   Conversely, v is live if
   * v is non-NULL 
   AND 
   * v isn't a block (e.g. an int or char), OR
   * v isn't in the heap (e.g. is an atom, or in the young generation), OR
   * we're in the mark phase (in which v may be resurrected by darkening), OR
   * we're in the weak phase but v has been darkened (so it will survive 
     the sweep phase), OR
   * we're in the sweep phase (since the pointer hasn't been reset by the 
     weak phase, v must have been dark at that time; hence v will 
     not be deallocated, but sweeping may have changed its color already).  
*/

int isdead(value v)
{
  return v == (value)NULL
         || (gc_phase == Phase_weak 
	     && Is_block(v) && Is_in_heap(v) && Is_white_val(v));
}

value weak_sub(value arr, value index)			/* ML */
{
  value v = Field(arr, Long_val(index));
  if (isdead(v))
    failwith("Dangling weak pointer");
  else
    if (gc_phase == Phase_mark)
      darken(v);
  return v;
}

value weak_isdead(value arr, value index)             /* ML */
{
  return Val_bool(isdead(Field(arr, Long_val(index))));
}

value weak_arr(value size)				/* ML */
{
  value res;
  mlsize_t sz, i;

  sz = Long_val(size);
  if (sz == 0) return Atom(Weak_tag);
  res = alloc_shr(sz, Weak_tag);	/* Must go in the major heap */
  for (i = 0; i < sz; i++)
    Field(res, i) = (value)NULL;

  return res;
}

/* Turn an ML value into an externalized ML value (a string), a la extern.c */

value string_mlval(value val)	/* ML */
{
  value s;
  byteoffset_t res;

  extern_size = INITIAL_EXTERN_SIZE;
  extern_block =
    (byteoffset_t *) stat_alloc(extern_size * sizeof(unsigned long));
  extern_pos = 0;
  extern_table_size = INITIAL_EXTERN_TABLE_SIZE;
  alloc_extern_table();
  extern_table_used = 0;
  res = emit_all(val);
  stat_free((char *) extern_table);

  /* We can allocate a string in the heap since the argument value is
     not used from now on. */
  if (extern_pos == 0)
    {
      s = alloc_string(8);
      ((asize_t *)s)[0] = (asize_t)extern_pos; 
      ((asize_t *)s)[1] = (asize_t)res;    
    }
  else
    {
      s = alloc_string(4 + extern_pos * sizeof(unsigned long));
      ((asize_t *)s)[0] = (asize_t)extern_pos; 
      bcopy((char *) extern_block, &Byte(s, 4), 
	    extern_pos * sizeof(unsigned long));
    }
  stat_free((char *) extern_block);
  return s;
}

/* Turn an externalized ML value (a string) into an ML value, a la intern.c */

value mlval_string(value s)	/* ML */
{
  value res;
  mlsize_t whsize, wosize;
  unsigned long bhsize;
  color_t color;
  header_t hd;

  whsize = ((mlsize_t *)s)[0];

  if (whsize == 0) {
    res = (value) ((mlsize_t *)s)[1];
    if (Is_long(res))
      return res;
    else
      return Atom(res >> 2);
  }
  bhsize = Bsize_wsize (whsize);
  wosize = Wosize_whsize (whsize);
  
  if (wosize > Max_wosize)
    failwith("mlval_string: structure too big");
  res = alloc_shr(wosize, String_tag);
  hd = Hd_val (res);
  color = Color_hd (hd);
  Assert (color == White || color == Black);
  if (bhsize + 4 > string_length(s)) {
    Hd_val (res) = hd;                      /* Avoid confusing the GC. */
    failwith ("mlval_string: truncated object");
  }
  bcopy(&Byte(s, 4), Hp_val(res), bhsize);
  adjust_pointers((value*)(Hp_val (res)), whsize, color);

  return res;
}

/* Make a double from a float object, represented as a big-endian
   four-byte Word8Vector value */

value w8vectofloat(value v)		/* ML */
{
  /* The v vector must have length = 4 bytes */
  union { float flt; char w8[4]; } buf;
  int i;
  char* p = String_val(v);
  for (i=0; i<4; i++)
#ifdef MOSML_BIG_ENDIAN
    buf.w8[i] = p[i];
#else
    buf.w8[i] = p[3-i];
#endif

  return copy_double(buf.flt);
}

/* Make a big-endian four-byte Word8Vector value from a float,
   represented as a double. */

value floattow8vec(value v)		/* ML */
{
  union { float flt; char w8[4]; } buf;
  value res;
  char* p;
  int i;
  buf.flt = (float)(Double_val(v));
  res = alloc_string(4);
  p = String_val(res);
  for (i=0; i<4; i++)
#ifdef MOSML_BIG_ENDIAN
    p[i] = buf.w8[i]; 
#else
    p[i] = buf.w8[3-i];
#endif

  return res;
}

/* Make a double from a double object, represented as a big-endian
   eight-byte Word8Vector value */

value w8vectodouble(value v)		/* ML */
{
  /* The v vector must have length = 8 bytes */

  value res;
  
#ifdef MOSML_BIG_ENDIAN
  res = copy_double(Double_val(v));
#else
  Push_roots(r, 1);
  r[0] = v;
  res = copy_double(0.0);
  { 
    int i;
    for (i=0; i<8; i++)
      Byte(res, i) = Byte(r[0], 7-i);
  }
  Pop_roots();
#endif

  return res;
}

/* Make a big-endian eight-byte Word8Vector value from a double. */

value doubletow8vec(value v)		/* ML */
{
  value res;
  Push_roots(r, 1);
  r[0] = v;
  res = alloc_string(8);
  Store_double_val(res, Double_val(r[0]));
  Pop_roots();

#ifndef MOSML_BIG_ENDIAN
  { 
    int i;
    for (i=0; i<4; i++)
      { 
	char tmp = Byte(res, i); 
	Byte(res, i) = Byte(res, 7-i);
	Byte(res, 7-i) = tmp;
      }
  }
#endif

  return res;
}

/* Modified from John Reppy's code (see SML Basis mail of 1997-08-01) */

value sml_localoffset(value v)	/* ML */
{
  struct tm   *gmt;
  time_t      t1, t2;
  double      td;

  t1 = time((time_t*)0);
  gmt = gmtime (&t1);
  t2 = tm2cal(gmt);

  /* SunOS appears to lack difftime: */
#if defined(sun) && !defined(__svr4__)
  td = (long)t2 - (long)t1;
#else
  td = difftime(t2, t1);
#endif

  return copy_double(td); /* not SYStoSMLtime(td) */
}

/* Return a name (as a string) of SML exception exn */

value sml_exnname(value exn)	/* ML */
{
  value strval = Field(Field(exn, 0), 0);
  return strval;
}

/* Create a string representation of SML exception exn, if possible. */

char* exnmessage_aux(value exn)
{
#define BUFSIZE 256
  char* buf = (char*)malloc(BUFSIZE+1);
  /* An exn val is a pair (strref, argval) : string ref * 'a */
  value strref = Field(exn, 0);
  value strval = Field(strref, 0);
  value argval = Field(exn, 1);
  if (strref == Field(global_data, SYS__EXN_SYSERR)) {
    value msgval = Field(argval, 0);
#if defined(__CYGWIN__) || defined(hpux)
    sprintf(buf, "%s: %s",
	     String_val(strval), String_val(msgval));
#elif defined(WIN32)
    _snprintf(buf, BUFSIZE, "%s: %s",
	     String_val(strval), String_val(msgval));
#else
    snprintf(buf, BUFSIZE, "%s: %s",
	     String_val(strval), String_val(msgval));
#endif
    return buf;
  } else if (strref == Field(global_data, SYS__EXN_IO)) {
    value causeval = Field(argval, 0);
    value fcnval   = Field(argval, 1);
    value nameval  = Field(argval, 2);
    char* causetxt = exnmessage_aux(causeval);
#if defined(__CYGWIN__) || defined(hpux)
    sprintf(buf, "%s: %s failed on `%s'; %s", 
	     String_val(strval), String_val(fcnval), 
	     String_val(nameval), causetxt);
#elif defined(WIN32)
    _snprintf(buf, BUFSIZE, "%s: %s failed on `%s'; %s", 
	     String_val(strval), String_val(fcnval), 
	     String_val(nameval), causetxt);
#else
    snprintf(buf, BUFSIZE, "%s: %s failed on `%s'; %s", 
	     String_val(strval), String_val(fcnval), 
	     String_val(nameval), causetxt);
#endif
    free(causetxt);
    return buf;
  } else if (Is_block(argval)) {
    if (Tag_val(argval) == String_tag) { 
#if defined(__CYGWIN__) || defined(hpux)
      sprintf(buf, "%s: %s", String_val(strval), String_val(argval));
#elif defined(WIN32)
      _snprintf(buf, BUFSIZE, "%s: %s", String_val(strval), String_val(argval));
#else
      snprintf(buf, BUFSIZE, "%s: %s", String_val(strval), String_val(argval));
#endif
      return buf;
    } else if (Tag_val(argval) == Double_tag){
      char doubletxt[64];
      string_of_float_aux(doubletxt, Double_val(argval));
#if defined(__CYGWIN__) || defined(hpux)
      sprintf(buf, "%s: %s", String_val(strval), doubletxt);
#elif defined(WIN32)
      _snprintf(buf, BUFSIZE, "%s: %s", String_val(strval), doubletxt);
#else
      snprintf(buf, BUFSIZE, "%s: %s", String_val(strval), doubletxt);
#endif
      return buf;
    }
  }
  /* If unknown exception, copy the name and return it */
#if defined(__CYGWIN__)
  sprintf(buf, "%s", String_val(strval));
#elif defined(WIN32)
  _snprintf(buf, BUFSIZE, "%s", String_val(strval));
#else
  snprintf(buf, BUFSIZE, "%s", String_val(strval));
#endif
  return buf;
#undef BUFSIZE 
}


/* Return a string representation of SML exception exn, if possible */

value sml_exnmessage(value exn)	/* ML */
{
  char* buf = exnmessage_aux(exn);
  value res = copy_string(buf);
  free(buf);
  return res;
}

/* Sleep for the number of usec indicated the Double val vtime */

value sml_sleep(value vtime)	/* ML */
{
  double time = Double_val(vtime);
#ifdef WIN32
/* cvr: is this correct for win32? */
  unsigned long msec = (long)(time/1000.0);
  if (time > 0) {
    Sleep(msec);
  }
#else
  unsigned long sec = (long)(time/1000000.0);
  unsigned long usec = (long)(time - 1000000.0 * sec);
  if (time > 0) {
    sleep(sec);
    usleep(usec);
  } 
#endif       
  return Val_unit;
}
