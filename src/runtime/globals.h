/* ML global variables reachable from C. */

#ifndef _globals_
#define _globals_


#include "mlvalues.h"

extern value global_data;

#define GLOBAL_DATA 0           /* "meta","global_data" */
#define SYS__S_IRUSR 1          /* "sys","s_irusr" */
#define SYS__S_IWUSR 2          /* "sys","s_iwusr" */
#define SYS__S_IXUSR 3          /* "sys","s_ixusr" */
#define SYS__S_IRGRP 4          /* "sys","s_irgrp" */
#define SYS__S_IWGRP 5          /* "sys","s_iwgrp" */
#define SYS__S_IXGRP 6          /* "sys","s_ixgrp" */
#define SYS__S_IROTH 7          /* "sys","s_iroth" */
#define SYS__S_IWOTH 8          /* "sys","s_iwoth" */
#define SYS__S_IXOTH 9          /* "sys","s_ixoth" */
#define SYS__S_ISUID 10         /* "sys","s_isuid" */
#define SYS__S_ISGID 11         /* "sys","s_isgid" */
#define SYS__S_IRALL 12         /* "sys","s_irall" */
#define SYS__S_IWALL 13         /* "sys","s_iwall" */
#define SYS__S_IXALL 14         /* "sys","s_ixall" */
#define SYS__COMMAND_LINE 15    /* "sys","command_line" */
#define SYS__INTERACTIVE 16     /* "sys","interactive" */
#define SYS__MAX_STRING_LENGTH 17 /* "sys","max_string_length" */
#define SYS__MAX_VECT_LENGTH 18   /* "sys","max_vect_length" */

/* Exn indexes names for pervasive dynamic exceptions.  The
   corresponding exn names (string refs) are allocated by sys_init */

#define SYS__EXN_MEMORY     19   /* "sys","exn_memory" */
#define SYS__EXN_ARGUMENT   20   /* "sys","exn_argument" */
#define SYS__EXN_GRAPHIC    21   /* "sys","exn_graphic" */
#define SYS__EXN_SYSERR     22   /* "sys","exn_syserr" */
#define SYS__EXN_FAIL       23   /* "sys","exn_fail" */
#define SYS__EXN_SIZE       24   /* "sys","exn_size" */
#define SYS__EXN_INTERRUPT  25   /* "sys","exn_interrupt" */
#define SYS__EXN_SUBSCRIPT  26   /* "sys","exn_subscript" */
#define SYS__EXN_CHR        27   /* "sys","exn_chr" */
#define SYS__EXN_DIV        28   /* "sys","exn_div" */
#define SYS__EXN_DOMAIN     29   /* "sys","exn_domain" */
#define SYS__EXN_ORD        30   /* "sys","exn_ord" */
#define SYS__EXN_OVERFLOW   31   /* "sys","exn_overflow" */
#define SYS__EXN_BIND       32   /* "sys","exn_bind" */
#define SYS__EXN_MATCH      33   /* "sys","exn_match" */
#define SYS__EXN_IO         34   /* "sys","exn_io" */

/* Frequently used exception values (NOT exn indexes); alloc by sys_init */

#define EXN_INTERRUPT       35   /* "sys","val_exn_interrupt" */
#define EXN_DIV             36   /* "sys","val_exn_div" */
#define EXN_OVERFLOW        37   /* "sys","val_exn_overflow" */

#define SYS__FIRST_EXN 19
#define SYS__LAST_EXN 34

#endif /* _globals_ */
