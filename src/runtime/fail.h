#ifndef _fail_
#define _fail_

#include <setjmp.h>
#include "misc.h"
#include "mlvalues.h"

struct longjmp_buffer {
  jmp_buf buf;
};

extern struct longjmp_buffer * external_raise;
extern value exn_bucket;

EXTERN Noreturn mlraise(value);
EXTERN Noreturn raiseprimitive0(int exnindex);
EXTERN Noreturn raiseprimitive1(int exnindex, value arg);
EXTERN Noreturn raise_with_string(int exnindex, char * msg);
EXTERN Noreturn failwith(char *);
EXTERN Noreturn invalid_argument(char *);
EXTERN Noreturn raise_overflow(void);
EXTERN Noreturn raise_out_of_memory(void);
extern volatile int float_exn;

extern double maxdouble;

#endif /* _fail_ */
