#ifndef _signals_
#define _signals_

#include "misc.h"

#if defined(__STDC__) || defined(WIN32)

extern volatile int signal_is_pending;
extern volatile CODE signal_handler;
extern volatile int signal_number;
extern int in_blocking_section;

#else

extern int signal_is_pending;
extern CODE signal_handler;
extern int signal_number;
extern int in_blocking_section;

#endif

void execute_signal (void);
EXTERN void enter_blocking_section (void);
EXTERN void leave_blocking_section (void);
extern CODE raise_break_exn;
#endif /* _signals_ */
