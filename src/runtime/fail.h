#ifndef _fail_
#define _fail_


#include <setjmp.h>
#include "misc.h"
#include "mlvalues.h"

#define OUT_OF_MEMORY_EXN 0     /* "exc","Out_of_memory",1 */
#define SYS_ERROR_EXN 1         /* "sys","Sys_error",1 */
#define FAILURE_EXN 2           /* "exc","Failure",3 */
#define INVALID_EXN 3           /* "exc","Invalid_argument",2 */
#define END_OF_FILE_EXN 4       /* "io","End_of_file",1 */
#define ZERO_DIVIDE_EXN 5       /* "int","Division_by_zero",1 */
#define BREAK_EXN 6             /* "sys","Break",2 */
#define NOT_FOUND_EXN 7         /* "exc","Not_found",4 */
#define UNIX_ERROR_EXN 8        /* "unix","Unix_error",1 */
#define GRAPHIC_FAILURE_EXN 9   /* "graphics","Graphic_failure",1 */
#define PARSE_FAILURE_EXN 10    /* "stream","Parse_failure",1 */

/* Additional predefined exceptions for Moscow ML */

#define SMLEXN_EXCEPTION 11     /* "general","Exception",1 */
#define SMLEXN_BIND      12     /* "general","Bind",2 */
#define SMLEXN_CHR       13     /* "general","Chr",3 */
#define SMLEXN_DIV       14     /* "general","Div",4 */
#define SMLEXN_DOMAIN    15     /* "general","Domain",5 */
#define SMLEXN_MATCH     16     /* "general","Match",6 */
#define SMLEXN_ORD       17     /* "general","Ord",7 */
#define SMLEXN_OVF       18     /* "general","Overflow",8 */

/* End of Moscow ML exceptions */

struct longjmp_buffer {
  jmp_buf buf;
};

extern struct longjmp_buffer * external_raise;
extern value exn_bucket;

EXTERN Noreturn mlraise (value);
EXTERN Noreturn raise_with_arg (tag_t tag, value arg);
EXTERN Noreturn raise_with_string (tag_t tag, char * msg);
EXTERN Noreturn failwith (char *);
extern volatile unsigned char float_exn;
Noreturn invalid_argument (char *);
Noreturn raise_out_of_memory (void);
extern double maxdouble;

#endif /* _fail_ */
