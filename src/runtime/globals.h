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

#endif /* _globals_ */
