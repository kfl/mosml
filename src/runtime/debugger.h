#ifndef _debugger_
#define _debugger_

#include "misc.h"
#include "mlvalues.h"

#ifdef DEBUG

#define LOG_BUFFER_SIZE 100
extern bytecode_t log_buffer[LOG_BUFFER_SIZE];
extern bytecode_t * log_ptr;
extern int trace_flag;

#define Debug(x) x

#if defined(__STDC__) || defined(WIN32)
#define Assert(x) if (!(x)) failed_assert ( #x , __FILE__, __LINE__)
#define Dprintx(x) printf ("expression %s %ld\n", #x, (unsigned long) (x))
#else
#ifndef __LINE__
#define __LINE__ 0
#endif
#ifndef __FILE__
#define __FILE__ "(?)"
#endif
#define Assert(x) if (!(x)) failed_assert ("(?)" , __FILE__, __LINE__)
#define Dprintx(x) printf ("expression %ld\n", (unsigned long) (x))
#endif /* __STDC__ */

void failed_assert (char *, char *, int);
void print_value (value);
bytecode_t disasm_instr (bytecode_t);
void post_mortem (int);
unsigned long not_random (void);

#else /* DEBUG */

#define Debug(x)
#define Assert(x)
#define Dprintx(x)

#endif /* DEBUG */

#define nTrace(msg, x, y)

#ifdef TRACE
#define Trace(msg, x, y) printf (msg, x, y)
#else
#define Trace(msg, x, y)
#endif


#endif /* _debugger_ */
