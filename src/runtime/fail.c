/* Raising exceptions from C. */

#if !defined(WIN32) && (defined(__unix__) || defined(unix)) && !defined(USG)
#include <sys/param.h>
#endif

#if defined(__MWERKS__) || defined(WIN32)
#define MAXDOUBLE 1.7976931348623157081e+308
#else
#if (defined(BSD) && BSD >= 199306) || defined(__CYGWIN__)
#include <float.h>
#define MAXDOUBLE DBL_MAX
#else
#include <values.h>
#endif
#endif
#include "alloc.h"
#include "fail.h"
#include "memory.h"
#include "mlvalues.h"
#include "signals.h"


/* The exception (Fail "floating point error") will be raised if
   smlexn has not been initialized before a floating point error
   occurs.  */

volatile unsigned char float_exn = FAILURE_EXN;

double maxdouble = MAXDOUBLE/2;

struct longjmp_buffer * external_raise;
value exn_bucket;

EXTERN void mlraise(value v)
{
  in_blocking_section = 0;
  exn_bucket = v;
  longjmp(external_raise->buf, 1);
}

EXTERN void raise_with_arg(tag_t tag, value arg)
{
  value bucket;
  Push_roots (a, 1);
  a[0] = arg;

  bucket = alloc (1, tag);
  Field(bucket, 0) = a[0];
  Pop_roots ();
  mlraise(bucket);
}

EXTERN void raise_with_string(tag_t tag, char * msg)
{
  raise_with_arg(tag, copy_string(msg));
}

EXTERN void failwith (char* msg)
{
  raise_with_string(FAILURE_EXN, msg);
}

void invalid_argument (char * msg)
{
  raise_with_string(INVALID_EXN, msg);
}

void raise_out_of_memory()
{
  mlraise(Atom(OUT_OF_MEMORY_EXN));
}
